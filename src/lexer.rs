use std::iter;

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

impl<'a> Token<'a> {
    pub fn layout_end(position: Position) -> Self {
        Self {
            kind: TokenKind::LayoutEnd,
            start: position,
            end: position,
        }
    }
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
pub enum LayoutDelimiter {
    Root,
    Top,
    LetExpression,
    LetStatement,
    Where,
    Do,
    Ado,
}

impl LayoutDelimiter {
    pub fn is_indented(&self) -> bool {
        use LayoutDelimiter::*;

        match self {
            Root | Top => false,
            Ado | Do | LetExpression | LetStatement | Where => true,
        }
    }

    /// Returns `true` if the layout delimeter is [`Do`].
    ///
    /// [`Do`]: LayoutDelimeter::Do
    #[must_use]
    pub fn is_do(&self) -> bool {
        matches!(self, Self::Do)
    }
}

pub type LayoutStack = Vec<(Position, LayoutDelimiter)>;

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
            LayoutDelimiter::Root,
        )];
        let tokens = vec![];
        Self { stack, tokens }
    }

    pub fn insert_layout(&mut self, token: Token<'a>, future: Position) {
        match token {
            lower_name!("let") => {
                self.collapse_offside(token);
                self.insert_seperator(token);
                self.tokens.push(token);
                let delimiter = match self.stack.last() {
                    Some((position, LayoutDelimiter::Do))
                    | Some((position, LayoutDelimiter::Ado))
                        if position.column == token.start.column =>
                    {
                        LayoutDelimiter::LetStatement
                    }
                    _ => LayoutDelimiter::LetExpression,
                };
                self.insert_start(delimiter, future);
            }
            lower_name!("where") => {
                self.collapse_where(token);
                self.tokens.push(token);
                self.insert_start(LayoutDelimiter::Where, future);
            }
            lower_name!("ado") => {
                self.tokens.push(token);
                self.insert_start(LayoutDelimiter::Ado, future);
            }
            lower_name!("do") => {
                self.tokens.push(token);
                self.insert_start(LayoutDelimiter::Do, future);
            }
            lower_name!("in") => {
                let (stack_end, end_count) = self.collapse(|_, delimiter| match delimiter {
                    LayoutDelimiter::Ado | LayoutDelimiter::LetExpression => false,
                    _ => delimiter.is_indented(),
                });

                match &self.stack[..stack_end] {
                    [.., (_, LayoutDelimiter::Ado), (_, LayoutDelimiter::LetStatement)] => {
                        self.stack.truncate(stack_end.saturating_sub(2));
                        self.tokens.extend(
                            iter::repeat(Token::layout_end(token.start)).take(end_count + 2),
                        );
                    }
                    [.., (_, delimiter)] if delimiter.is_indented() => {
                        self.stack.truncate(stack_end.saturating_sub(1));
                        self.tokens.extend(
                            iter::repeat(Token::layout_end(token.start)).take(end_count + 1),
                        );
                    }
                    _ => {
                        self.collapse_offside(token);
                        self.insert_seperator(token);
                    }
                }
                self.tokens.push(token);
            }
            _ => {
                self.collapse_offside(token);
                self.insert_seperator(token);
                self.tokens.push(token);
            }
        };
    }

    fn insert_start(&mut self, delimiter: LayoutDelimiter, future: Position) {
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

    fn collapse_offside(&mut self, token: Token<'a>) {
        let (stack_end, end_count) = self.collapse(|position, delimiter| {
            delimiter.is_indented() && token.start.column < position.column
        });
        self.stack.truncate(stack_end);
        self.tokens
            .extend(iter::repeat(Token::layout_end(token.start)).take(end_count));
    }

    fn collapse_where(&mut self, token: Token<'a>) {
        let (stack_end, end_count) = self.collapse(|position, delimiter| {
            delimiter.is_do() || (delimiter.is_indented() && token.start.column <= position.column)
        });
        self.stack.truncate(stack_end);
        self.tokens
            .extend(iter::repeat(Token::layout_end(token.start)).take(end_count));
    }

    fn collapse<F>(&self, predicate: F) -> (usize, usize)
    where
        F: Fn(&Position, &LayoutDelimiter) -> bool,
    {
        let mut stack_end = self.stack.len();
        let mut end_count = 0;
        for (position, delimiter) in self.stack.iter().rev() {
            if predicate(position, delimiter) {
                stack_end = stack_end.saturating_sub(1);
                end_count += 1;
            } else {
                break;
            }
        }
        (stack_end, end_count)
    }

    fn insert_seperator(&mut self, token: Token<'a>) {
        match self.stack.last() {
            Some((position, LayoutDelimiter::Top)) => {
                if token.start.column == position.column && token.start.line != position.line {
                    self.stack.pop();
                    self.tokens.push(Token {
                        kind: TokenKind::LayoutSeperator,
                        start: token.start,
                        end: token.start,
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

    pub fn unwind_stack(&mut self, position: Position) {
        while let Some((_, delimiter)) = self.stack.pop() {
            if let LayoutDelimiter::Root = delimiter {
                self.tokens.push(Token {
                    kind: TokenKind::Eof,
                    start: position,
                    end: position,
                })
            } else if delimiter.is_indented() {
                self.tokens.push(Token {
                    kind: TokenKind::LayoutEnd,
                    start: position,
                    end: position,
                })
            }
        }
    }
}
