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

        if let Rule::EOI = pair.as_rule() {
            return None;
        }

        let start = pair.as_span().start();
        let end = pair.as_span().end();

        Some(Token {
            kind: match pair.as_rule() {
                Rule::upper_name => TokenKind::UpperName(pair.as_str()),
                Rule::lower_name => TokenKind::LowerName(pair.as_str()),
                Rule::symbol_name => TokenKind::SymbolName(pair.as_str()),
                Rule::digit_value => TokenKind::LiteralInteger(pair.as_str()),
                _ => unreachable!(),
            },
            start: self.get_position(start),
            end: self.get_position(end),
        })
    }
}

#[derive(Debug)]
pub enum LayoutDelimeter {
    Let,
}

impl LayoutDelimeter {
    pub fn is_indented(&self) -> bool {
        match self {
            LayoutDelimeter::Let => true,
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
        Self {
            stack: LayoutStack::default(),
            tokens: Vec::default(),
        }
    }

    pub fn insert_layout(&mut self, token: Token<'a>, future: Position) {
        match token {
            lower_name!("let") => self.insert_start(LayoutDelimeter::Let, future),
            _ => {
                self.collapse_offside(token);
                self.insert_seperator(token);
                self.insert_default(token);
            }
        }
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
        while let Some((position, delimiter)) = self.stack.pop() {
            if delimiter.is_indented() && token.start.column < position.column {
                self.tokens.push(Token {
                    kind: TokenKind::LayoutEnd,
                    start: token.start,
                    end: position,
                })
            } else {
                self.stack.push((position, delimiter));
                break;
            }
        }
    }

    fn insert_seperator(&mut self, token: Token<'a>) {
        if let Some((position, delimiter)) = self.stack.last() {
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
    }
}
