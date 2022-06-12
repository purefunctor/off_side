use pest::{error::Error, iterators::FlatPairs, Parser};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Grammar;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind<'a> {
    UpperName(&'a str),
    LowerName(&'a str),
    SymbolName(&'a str),
    LiteralInteger(&'a str),
    LayoutStart,
    LayoutSeperator,
    LayoutEnd,
    Eof,
}

impl<'a> TokenKind<'a> {
    /// Returns `true` if the token kind is [`Eof`].
    ///
    /// [`Eof`]: TokenKind::Eof
    #[must_use]
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Eof)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub start: Position,
    pub end: Position,
}

/// The lexer driver.
#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer given a source string.
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    /// Returns a stream of tokens if the source string successfully
    /// parses, emits an error otherwise.
    pub fn lex(self) -> Result<TokenStream<'a>, Error<Rule>> {
        let source = self.source;
        let mut offset = 0;
        let mut offsets = vec![];
        for line in source.split('\n') {
            offsets.push(offset);
            offset += line.len() + 1
        }
        let pairs = Grammar::parse(Rule::tokens, source)?.flatten();
        Ok(TokenStream {
            source,
            offsets,
            pairs,
        })
    }
}

/// An iterator over a stream of tokens.
pub struct TokenStream<'a> {
    source: &'a str,
    offsets: Vec<usize>,
    pairs: FlatPairs<'a, Rule>,
}

impl<'a> TokenStream<'a> {
    /// Obtains the line and column position given an offset.
    pub fn get_position(&self, offset: usize) -> Position {
        if offset > self.source.len() {
            panic!("TokenStream::get_position - offset is greater than the source")
        }

        // Find the closest line offset and its index
        let closest_index = self
            .offsets
            .binary_search_by_key(&offset, |&offset| offset)
            .unwrap_or_else(|index| index.saturating_sub(1));
        let line_offset = self.offsets[closest_index];

        // Determine the line and column numbers
        let line = closest_index + 1;
        let column = offset - line_offset + 1;

        Position {
            offset,
            line,
            column,
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let pair = self.pairs.next()?;

        let start = pair.as_span().start();
        let end = pair.as_span().end();

        Some(Token {
            kind: match pair.as_rule() {
                Rule::upper_name => TokenKind::UpperName(pair.as_str()),
                Rule::lower_name => TokenKind::LowerName(pair.as_str()),
                Rule::symbol_name => TokenKind::SymbolName(pair.as_str()),
                Rule::digit_value => TokenKind::LiteralInteger(pair.as_str()),
                Rule::EOI => TokenKind::Eof,
                _ => unreachable!(),
            },
            start: self.get_position(start),
            end: self.get_position(end),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LayoutDelimeter {
    Root,
    Top,
    Let,
    Where,
}

impl LayoutDelimeter {
    pub fn is_indented(&self) -> bool {
        match self {
            LayoutDelimeter::Root | LayoutDelimeter::Top => false,
            LayoutDelimeter::Let | LayoutDelimeter::Where => true,
        }
    }
}

pub type LayoutStack = Vec<(Position, LayoutDelimeter)>;

#[derive(Default)]
pub struct LayoutEngine<'a> {
    pub stack: LayoutStack,
    pub tokens: Vec<Token<'a>>,
}

macro_rules! lower_name {
    ($name:expr) => {
        Token {
            kind: TokenKind::LowerName($name),
            ..
        }
    };
}

impl<'a> LayoutEngine<'a> {
    pub fn new() -> Self {
        let stack = vec![(
            Position {
                offset: 0,
                line: 1,
                column: 1,
            },
            LayoutDelimeter::Root,
        )];
        let tokens = vec![];
        Self { stack, tokens }
    }

    pub fn insert_layout(&mut self, token: Token<'a>, future: Position) {
        match token {
            lower_name!("let") => {
                self.insert_default(token);
                self.insert_start(LayoutDelimeter::Let, future);
            }
            lower_name!("where") => {
                self.collapse_offside(token);
                self.insert_default(token);
                self.insert_start(LayoutDelimeter::Where, future);
            }
            _ => {
                self.collapse_offside(token);
                self.insert_seperator(token);
                self.insert_default(token);
            }
        };
    }

    fn insert_start(&mut self, delimiter: LayoutDelimeter, future: Position) {
        let past_indented = self
            .stack
            .iter()
            .rfind(|(_, delimiter)| delimiter.is_indented());

        if let Some((past, _)) = past_indented {
            if future.column <= past.column {
                return;
            }
        }

        self.stack.push((future, delimiter));
        self.tokens.push(Token {
            kind: TokenKind::LayoutStart,
            start: future,
            end: future,
        })
    }

    fn insert_default(&mut self, token: Token<'a>) {
        self.tokens.push(token);
    }

    fn collapse_offside(&mut self, token: Token<'a>) {
        let (index, tokens) = self.collapse(|position, delimiter| {
            delimiter.is_indented() && token.start.column < position.column
        }, token);
        self.stack.truncate(index);
        self.tokens.extend(tokens);
    }

    fn collapse(&mut self, predicate: impl Fn(&Position, &LayoutDelimeter) -> bool, token: Token<'a>) -> (usize, Vec<Token<'a>>) {
        let mut end = self.stack.len();
        let mut tokens = vec![];
        for (position, delimiter) in self.stack.iter().rev() {
            if predicate(position, delimiter) {
                end -= 1;
                tokens.push(Token {
                    kind: TokenKind::LayoutSeperator,
                    start: token.start,
                    end: token.end,
                })
            }
        }
        (end, tokens)
    }

    fn insert_seperator(&mut self, token: Token<'a>) {
        match self.stack.last() {
            Some((position, LayoutDelimeter::Top)) => {
                if token.start.column == position.column && token.start.line != position.line {
                    self.stack.pop();
                    self.tokens.push(Token {
                        kind: TokenKind::LayoutSeperator,
                        start: token.start,
                        end: token.end,
                    });
                }
            }
            Some((position, delimiter)) => {
                if delimiter.is_indented()
                    && token.start.column == position.column
                    && token.start.line != position.line
                {
                    self.tokens.push(Token {
                        kind: TokenKind::LayoutSeperator,
                        start: token.start,
                        end: token.start,
                    })
                }
            }
            _ => {}
        }
    }

    pub fn unwind_stack(&mut self, end_position: Position) {
        while let Some((_, delimiter)) = self.stack.pop() {
            if let LayoutDelimeter::Root = delimiter {
                self.tokens.push(Token {
                    kind: TokenKind::Eof,
                    start: end_position,
                    end: end_position,
                })
            } else if delimiter.is_indented() {
                self.tokens.push(Token {
                    kind: TokenKind::LayoutEnd,
                    start: end_position,
                    end: end_position,
                })
            }
        }
    }
}
