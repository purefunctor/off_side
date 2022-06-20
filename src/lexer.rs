use std::{collections::VecDeque, iter::Peekable};

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub start: Position,
    pub end: Position,
}

impl<'a> Token<'a> {
    pub fn eof(position: Position) -> Self {
        Self {
            kind: TokenKind::Eof,
            start: position,
            end: position,
        }
    }

    pub fn layout_start(position: Position) -> Self {
        Self {
            kind: TokenKind::LayoutStart,
            start: position,
            end: position,
        }
    }

    pub fn layout_separator(position: Position) -> Self {
        Self {
            kind: TokenKind::LayoutSeperator,
            start: position,
            end: position,
        }
    }

    pub fn layout_end(position: Position) -> Self {
        Self {
            kind: TokenKind::LayoutEnd,
            start: position,
            end: position,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Delimiter {
    Ado,
    Brace,
    Case,
    CaseBinders,
    CaseGuard,
    DeclarationGuard,
    Do,
    LetExpression,
    LetStatement,
    Of,
    Parenthesis,
    Property,
    Root,
    Square,
    Top,
    Where,
    If,
    Then,
}

impl Delimiter {
    fn is_do(&self) -> bool {
        use Delimiter::*;
        matches!(self, Do)
    }

    pub fn is_indented(&self) -> bool {
        use Delimiter::*;
        matches!(self, Ado | Do | Of | LetExpression | LetStatement | Where)
    }
}

macro_rules! lower_name {
    ($name:pat) => {
        TokenKind::LowerName($name)
    };
}

macro_rules! symbol_name {
    ($name:expr) => {
        TokenKind::SymbolName($name)
    };
}

pub fn lex(source: &str) -> Result<LexWithLayout, Error<Rule>> {
    let pairs = Grammar::parse(Rule::tokens, source)?.flatten().peekable();
    Ok(LexWithLayout::new(source, pairs))
}

/// An iterator over `Token`s which handles layout rules.
pub struct LexWithLayout<'a> {
    /// The contents of the source file being tokenized.
    source: &'a str,
    /// The byte offsets of each line in the source file.
    offsets: Vec<usize>,
    /// An iterator over the token pairs generated by `pest`.
    pairs: Peekable<FlatPairs<'a, Rule>>,
    /// The current token being handled, initially set to `None`.
    current: Option<Token<'a>>,
    /// The current stack of layout delimiters and their positions.
    stack: Vec<(Position, Delimiter)>,
    /// The current queue of tokens to be emitted.
    queue: VecDeque<Token<'a>>,
}

impl<'a> Iterator for LexWithLayout<'a> {
    type Item = Token<'a>;

    /// This particular implementation of `next` is non-linear in that
    /// it first drains the `queue` before actually moving to the next
    /// token. Once empty, it may get filled up again unless `pairs`
    /// is already exhausted. If an `Eof` is encountered while the
    /// `queue` is empty, `LayoutEnd` tokens will get pushed to the
    /// `queue`, and will be yieled at the next iteration.
    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            self.next_current()?;
            self.next_with_layout();
            let token = self.queue.pop_back();
            if let Some(Token {
                kind: TokenKind::Eof,
                ..
            }) = token
            {
                self.unwind_layout();
                self.queue.pop_back()
            } else {
                token
            }
        } else {
            self.queue.pop_back()
        }
    }
}

/// Top-level methods.
impl<'a> LexWithLayout<'a> {
    /// Creates a new `LexWithLayout` iterator.
    fn new(source: &'a str, pairs: Peekable<FlatPairs<'a, Rule>>) -> Self {
        let mut offset = 0;
        let mut offsets = vec![];
        for line in source.split('\n') {
            offsets.push(offset);
            offset += line.len() + 1
        }
        let current = None;
        let stack = vec![(
            Position {
                offset: 0,
                line: 1,
                column: 1,
            },
            Delimiter::Root,
        )];
        let queue = VecDeque::new();
        LexWithLayout {
            source,
            offsets,
            pairs,
            current,
            stack,
            queue,
        }
    }

    /// Determines the line and column position of a given byte offset.
    fn get_position(&self, offset: usize) -> Position {
        if offset > self.source.len() {
            panic!("offset is greater than the source")
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

/// `current_*` and `future_*` methods for querying information about
/// the current and next tokens.
impl<'a> LexWithLayout<'a> {
    /// Get the kind of the current token.
    fn current_kind(&self) -> TokenKind<'a> {
        match self.current {
            Some(current) => current.kind,
            None => panic!("current token is uninitialized"),
        }
    }
    /// Get the start position of the current token.
    fn current_start(&self) -> Position {
        match self.current {
            Some(current) => current.start,
            None => panic!("current token is uninitialized"),
        }
    }
    /// Get the end position of the current token.
    fn current_end(&self) -> Position {
        match self.current {
            Some(current) => current.end,
            None => panic!("current token is uninitialized"),
        }
    }
    /// Get the start position of a future token. Returns the end of
    /// the current token if there's no more future tokens.
    fn future_start(&mut self) -> Position {
        if let Some(future) = self.pairs.peek() {
            let start_offset = future.as_span().start();
            self.get_position(start_offset)
        } else {
            self.current_end()
        }
    }
}

/// Primitive methods for manipulating the token queue and the
/// delimiter stack.
impl<'a> LexWithLayout<'a> {
    /// Pushes a delimiter to the stack at the current token's
    /// position, panicking if it's uninitialized.
    fn push_delimiter_current_start(&mut self, delimiter: Delimiter) {
        let start = self.current_start();
        self.stack.push((start, delimiter));
    }
    /// Pushes a delimiter to the stack at a future token's position,
    /// using the current token's end position if it does not exist.
    fn push_delimiter_future_start(&mut self, delimiter: Delimiter) {
        let start = self.future_start();
        self.stack.push((start, delimiter));
    }
    /// Pushes the current token to the queue, panicking if it's
    /// uninitialized.
    fn queue_current(&mut self) {
        match self.current {
            Some(current) => self.queue.push_front(current),
            None => panic!("current token is uninitialized"),
        }
    }
    /// Truncates the delimiter stack to `n` elements.
    fn discard_delimiter_n(&mut self, value: usize) {
        self.stack.truncate(value);
    }
    /// Pushes `n` `LayoutEnd` tokens to the queue.
    fn queue_n_layout_end(&mut self, value: usize) {
        for _ in 0..value {
            self.queue
                .push_front(Token::layout_end(self.current_start()))
        }
    }
}

/// Core methods for implementing various layout rules.
impl<'a> LexWithLayout<'a> {
    /// Given a predicate, this "removes" elements from the delimiter
    /// stack and "adds" the appropriate amount of `LayoutEnd` tokens
    /// to the queue.
    ///
    /// In practice, this performs no mutable operations and only
    /// returns two things: the new size of the stack to be used
    /// alongside `discard_delimiter_n` and the number of `LayoutEnd`
    /// tokens to be inserted with `queue_n_layout_end`.
    fn collapse<F>(&self, predicate: F) -> (usize, usize)
    where
        F: Fn(&Position, &Delimiter) -> bool,
    {
        let mut take_n = self.stack.len();
        let mut make_n = 0;

        for (position, delimiter) in self.stack.iter().rev() {
            if predicate(position, delimiter) {
                take_n = take_n.saturating_sub(1);
                make_n += 1;
            } else {
                break;
            }
        }

        (take_n, make_n)
    }
    /// A version of `collapse` that alsp performs the associated
    /// mutations.
    fn collapse_mut<F>(&mut self, predicate: F)
    where
        F: Fn(&Position, &Delimiter) -> bool,
    {
        let (take_n, make_n) = self.collapse(predicate);
        self.discard_delimiter_n(take_n);
        self.queue_n_layout_end(make_n);
    }
    /// A version of `collapse` that discards delimiters if they
    /// introduce an indentation context and the current token is
    /// dedented past them, effectively ending said indentation
    /// context.
    fn collapse_offside(&mut self) {
        let token = self.current_start();
        self.collapse_mut(|position, delimiter| {
            delimiter.is_indented() && token.column < position.column
        });
    }
    /// Pushes a `LayoutStart` token to the queue and a provided
    /// delimiter to the delimiter stack if a future token is indented
    /// past the most recent delimiter that introduces an indentation
    /// context.
    fn insert_start(&mut self, delimiter: Delimiter) {
        let next = self.future_start();

        let recent_indented = self
            .stack
            .iter()
            .rfind(|(_, delimiter)| delimiter.is_indented());

        if let Some((past, _)) = recent_indented {
            if next.column <= past.column {
                return;
            }
        }

        self.stack.push((next, delimiter));
        self.queue.push_front(Token::layout_start(next));
    }
    /// Pushes a `LayoutSeperator` token to the delimiter stack if
    /// the topmost element introduces an indentation context and
    /// the current token aligns with it.
    fn insert_seperator(&mut self) {
        let token = self.current_start();
        match self.stack.last() {
            Some((position, Delimiter::Top)) => {
                if token.column == position.column && token.line != position.line {
                    self.stack.pop();
                    self.queue.push_front(Token::layout_separator(token));
                }
            }
            Some((position, delimiter)) => {
                if delimiter.is_indented()
                    && token.column == position.column
                    && token.line != position.line
                {
                    self.queue.push_front(Token::layout_separator(token));
                    if let Delimiter::Of = delimiter {
                        self.push_delimiter_current_start(Delimiter::CaseBinders);
                    }
                }
            }
            _ => {}
        }
    }
    /// Pushes the current token to the queue after running
    /// `collapse_offside` and `insert_separator`.
    fn queue_current_default(&mut self) {
        self.collapse_offside();
        self.insert_seperator();
        self.queue_current();
    }
    /// Discards all remaining elements from the layout stack while
    /// pushing the appropriate amount of `LayoutEnd` and `Eof` tokens
    /// to the queue.
    fn unwind_layout(&mut self) {
        let position = self.current_end();
        while let Some((_, delimiter)) = self.stack.pop() {
            if let Delimiter::Root = delimiter {
                self.queue.push_front(Token::eof(position));
            } else if delimiter.is_indented() {
                self.queue.push_front(Token::layout_end(position));
            }
        }
    }
}

/// Driver methods for advancing the current state.
impl<'a> LexWithLayout<'a> {
    /// Updates the current token by advancing the internal pairs
    /// iterator, returning an `Option` for easier short-circuiting.
    fn next_current(&mut self) -> Option<()> {
        use TokenKind::*;

        let pair = self.pairs.next()?;
        let rule = pair.as_rule();
        let span = pair.as_span();
        let slice = pair.as_str();
        self.current = Some(Token {
            kind: match rule {
                Rule::upper_name => UpperName(slice),
                Rule::lower_name => LowerName(slice),
                Rule::symbol_name => SymbolName(slice),
                Rule::digit_value => LiteralInteger(slice),
                Rule::EOI => Eof,
                rule => unreachable!("unhandled rule {:?}", rule),
            },
            start: self.get_position(span.start()),
            end: self.get_position(span.end()),
        });

        Some(())
    }
    /// Pushes the current token to the queue with respect to layout
    /// rules in the language.
    fn next_with_layout(&mut self) {
        use Delimiter::*;

        // if we're currently inside record syntax, make sure that
        // hard syntax names are inserted without layout rules.
        if let Some((_, Property)) = self.stack.last() {
            if let lower_name!(
                "let" | "where" | "do" | "ado" | "in" | "case" | "of" | "if" | "then" | "else"
            ) = self.current_kind()
            {
                self.stack.pop();
                self.queue_current();
                return;
            }
        }

        match self.current_kind() {
            lower_name!("let") => {
                self.queue_current_default();
                // `let` in `do`/`ado` is a statement.
                let delimiter = match self.stack.last() {
                    Some((position, Do | Ado))
                        if self.current_start().column == position.column =>
                    {
                        LetStatement
                    }
                    _ => LetExpression,
                };
                self.insert_start(delimiter);
            }
            lower_name!("where") => {
                let token = self.current_start();
                // `where` ends the following contexts:
                // 1. `do` blocks
                // 2. any delimiter with an indentation context _and_
                //    whose position aligns with, or is greater than
                //    the current token.
                self.collapse_mut(|position, delimiter| {
                    delimiter.is_do()
                        || (delimiter.is_indented() && token.column <= position.column)
                });
                self.queue_current();
                self.insert_start(Where);
            }
            lower_name!("do") => {
                self.queue_current();
                self.insert_start(Do);
            }
            lower_name!("ado") => {
                self.queue_current();
                self.insert_start(Ado);
            }
            lower_name!("in") => {
                // `in` ends any indented delimiter except for `Ado`
                // or a `LetExpression` since we handle them manually.
                let (stack_size, end_count) = self.collapse(|_, delimiter| match delimiter {
                    Ado | LetExpression => false,
                    _ => delimiter.is_indented(),
                });

                match &self.stack[..stack_size] {
                    // 1. this is run when a `LetExpression` is used
                    // inside of a `LetStatement` in an `Ado` block,
                    // like so:
                    //
                    // ```hs
                    // test = ado
                    //   let a = let b = c in c
                    //   in a
                    // ```
                    [.., (_, Ado), (_, LetStatement)] => {
                        // `in` terminates the whole `Ado` block, so
                        // we include them when truncating the stack
                        // and pushing layout tokens.
                        self.discard_delimiter_n(stack_size.saturating_sub(2));
                        self.queue_n_layout_end(end_count + 2)
                    }
                    // 2. this is run when any delimiter that
                    // introduces an indentation context is
                    // encountered; this also essentially catches
                    // `Ado` blocks w/o the specialization seen above.
                    [.., (_, delimiter)] if delimiter.is_indented() => {
                        self.discard_delimiter_n(stack_size.saturating_sub(1));
                        self.queue_n_layout_end(end_count + 1);
                    }
                    // 3. fallthrough; this just collapses everything
                    // "normally" and inserts a separator before the
                    // current token.
                    _ => {
                        self.collapse_offside();
                        self.insert_seperator();
                    }
                }
                self.queue_current();
            }
            lower_name!("case") => {
                self.queue_current_default();
                self.push_delimiter_future_start(Case);
            }
            lower_name!("of") => {
                // end all indented delimiters likely after `case`.
                let (stack_size, end_count) = self.collapse(|_, delimiter| delimiter.is_indented());

                match &self.stack[..stack_size] {
                    [.., (_, Case)] => {
                        self.discard_delimiter_n(stack_size.saturating_sub(1));
                        self.queue_n_layout_end(end_count);
                        self.queue_current();
                        self.insert_start(Of);
                        // `CaseBinders` essentially "hides" `Of` from
                        // being eliminated by a collapse. After a
                        // `->` is encountered, the underlying `Of` is
                        // revealed.
                        self.push_delimiter_future_start(CaseBinders);
                    }
                    _ => {
                        self.discard_delimiter_n(stack_size);
                        self.queue_n_layout_end(end_count);
                        self.queue_current_default();
                    }
                }
            }
            lower_name!("if") => {
                self.queue_current_default();
                self.push_delimiter_current_start(If);
            }
            lower_name!("then") => {
                let (stack_end, end_count) = self.collapse(|_, delimiter| delimiter.is_indented());

                match &self.stack[..stack_end] {
                    [.., (_, If)] => {
                        self.discard_delimiter_n(stack_end.saturating_sub(1));
                        self.queue_n_layout_end(end_count);
                        self.queue_current();
                        self.push_delimiter_current_start(Then);
                    }
                    _ => {
                        self.queue_current_default();
                    }
                }
            }
            lower_name!("else") => {
                let (stack_end, end_count) = self.collapse(|_, delimiter| delimiter.is_indented());

                match &self.stack[..stack_end] {
                    [.., (_, Then)] => {
                        self.discard_delimiter_n(stack_end.saturating_sub(1));
                        self.queue_n_layout_end(end_count);
                        self.queue_current();
                    }
                    _ => {
                        unimplemented!("instance chains!");
                    }
                }
            }
            symbol_name!("|") => {
                // end all indented delimiters if the current token is
                // also dedented past said delimiter.
                let token = self.current_start();
                let (stack_end, end_count) = self.collapse(|position, delimiter| {
                    delimiter.is_indented() && token.column <= position.column
                });

                match &self.stack[..stack_end] {
                    [.., (_, Of)] => {
                        self.discard_delimiter_n(stack_end);
                        self.queue_n_layout_end(end_count);
                        // `CaseGuard` essentially "hides" `Of` from
                        // being eliminated by a collapse. After a
                        // `->` is encountered, the underlying `Of` is
                        // revealed.
                        self.push_delimiter_future_start(CaseGuard);
                        self.queue_current();
                    }
                    [.., (_, LetExpression | LetStatement | Where)] => {
                        self.discard_delimiter_n(stack_end);
                        self.queue_n_layout_end(end_count);
                        // `DeclarationGuard` essentially hides their
                        // delimiters above from being eliminated by
                        // a collapse. After a `=` is encountered, the
                        // underlying delimiters are revealed.
                        self.push_delimiter_future_start(DeclarationGuard);
                        self.queue_current();
                    }
                    _ => {
                        self.discard_delimiter_n(stack_end);
                        self.queue_n_layout_end(end_count);
                        self.queue_current_default();
                    }
                }
            }
            symbol_name!("->") => {
                // 1. `->` always ends `Do`
                // 2. `->` never ends `Of`
                // 3. `->` ends indented delimiters if the current
                // token is also dedented past said delimiter.
                let token = self.current_start();
                self.collapse_mut(|position, delimiter| match delimiter {
                    Do => true,
                    Of => false,
                    _ => delimiter.is_indented() && token.column <= position.column,
                });
                // `->` elimjnates certain delimiters, which then
                // exposes the ones which can be eliminated through a
                // collapse.
                if let Some((_, CaseGuard | CaseBinders)) = self.stack.last() {
                    self.stack.pop();
                }
                self.queue_current();
            }
            symbol_name!("=") => {
                // `=` always collapses `LetExpression`,
                // `LetStatement`, and `Where`.
                let (stack_end, end_count) = self.collapse(|_, delimiter| {
                    matches!(delimiter, LetExpression | LetStatement | Where)
                });

                match &self.stack[..stack_end] {
                    [.., (_, DeclarationGuard)] => {
                        self.discard_delimiter_n(stack_end);
                        self.queue_n_layout_end(end_count);
                        self.queue_current();
                    }
                    _ => {
                        self.queue_current_default();
                    }
                }
            }
            symbol_name!(",") => {
                self.collapse_mut(|_, delimiter| delimiter.is_indented());
                self.queue_current();
                if let Some((_, Brace)) = self.stack.last() {
                    self.push_delimiter_current_start(Property);
                }
            }
            symbol_name!("[") => {
                self.queue_current_default();
                self.push_delimiter_current_start(Square);
            }
            symbol_name!("]") => {
                self.collapse_mut(|_, delimiter| delimiter.is_indented());
                if let Some((_, Square)) = self.stack.last() {
                    self.stack.pop();
                }
                self.queue_current();
            }
            symbol_name!("(") => {
                self.queue_current_default();
                self.push_delimiter_current_start(Parenthesis);
            }
            symbol_name!(")") => {
                self.collapse_mut(|_, delimiter| delimiter.is_indented());
                if let Some((_, Parenthesis)) = self.stack.last() {
                    self.stack.pop();
                }
                self.queue_current();
            }
            symbol_name!("{") => {
                self.queue_current_default();
                self.push_delimiter_current_start(Brace);
                self.push_delimiter_current_start(Property);
            }
            symbol_name!("}") => {
                self.collapse_mut(|_, delimiter| delimiter.is_indented());
                if let Some((_, Property)) = self.stack.last() {
                    self.stack.pop();
                }
                if let Some((_, Brace)) = self.stack.last() {
                    self.stack.pop();
                }
                self.queue_current();
            }
            _ => {
                self.queue_current_default();
            }
        }
    }
}
