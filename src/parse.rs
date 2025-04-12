use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
    error::Error,
    fmt::Display,
    ops::Range,
    rc::Rc,
};

use itertools::Itertools;

use crate::{cons_list::List, rule::concat};

/// A failed parser match
#[derive(Debug, Clone)]
pub struct Fail {
    /// the offset in the input where the failure occured
    pub location: usize,
    /// The token(s) we expected to encounter at this location
    pub expected: String,
    /// the token which we actually encountered
    pub found: String,
}

impl Fail {
    pub fn fail(location: usize, expected: String, input: &str) -> Self {
        let mut found = None;
        for c in input.chars().skip(location) {
            if !c.is_whitespace() {
                found = Some(c.to_string());
                break;
            }
        }

        Self {
            location,
            expected,
            found: found.unwrap_or("EOS".to_string()),
        }
    }
}

impl Display for Fail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected {} at {}, found {} instead",
            self.expected, self.location, self.found
        )
    }
}

impl Error for Fail {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Seq(Vec<Value>),
    Eof,
    Empty,
}

impl Value {
    pub fn flatten_string(&self) -> String {
        match self {
            Value::String(s) => s.into(),
            Value::Seq(v) => v.iter().map(|i| i.flatten_string()).join(""),
            _ => "".into(),
        }
    }

    pub fn flatten_seq(&self) -> Vec<Value> {
        match self {
            Value::Seq(v) => v.iter().flat_map(|i| i.flatten_seq()).collect(),
            e @ _ => vec![e.clone()],
        }
    }

    pub fn nth(&self, n: usize) -> Value {
        match self {
            Value::Seq(v) => v.get(n).cloned().unwrap_or(Value::Empty),
            _ => Value::Empty,
        }
    }

    pub fn slice(&self, r: Range<usize>) -> &[Value] {
        match self {
            Value::Seq(v) => &v[r],
            _ => &[],
        }
    }
}

/// A successful parser match
#[derive(Debug, Clone)]
pub struct Match {
    /// The starting offset of the match
    pub start: usize,
    /// The ending offset of the match
    pub end: usize,
    /// The value produced by the match
    pub value: Value,
}

pub trait Rule {
    fn parse(&self, _parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let found = if start < input.len() {
            input[start..start + 1].into()
        } else {
            "EOS".into()
        };
        Err(Fail {
            location: start,
            expected: self.expected(),
            found,
        })
    }

    fn expected(&self) -> String {
        "None".into()
    }
}

/// Matches any single token
#[derive(Clone)]
pub struct One {}

impl Rule for One {
    fn expected(&self) -> String {
        "One".into()
    }

    fn parse(&self, _parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let value = if start < input.len() {
            Value::String(input[start..start + 1].into())
        } else {
            Value::Eof
        };
        Ok(Match {
            start,
            end: start + 1,
            value,
        })
    }
}

/// Produces a zero-length match if the provided rule does not match
///
/// Mostly useful in combination with other rules
#[derive(Clone)]
pub struct Not<R>
where
    R: Rule + Clone,
{
    rule: R,
}

pub fn not<R>(rule: R) -> Not<R>
where
    R: Rule + Clone,
{
    Not { rule }
}

impl<R> Rule for Not<R>
where
    R: Rule + Clone,
{
    fn expected(&self) -> String {
        format!("!{}", self.rule.expected())
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(&self.rule, start, input) {
            Ok(_) => Err(Fail::fail(start, self.expected(), input)),
            _ => Ok(Match {
                start,
                end: start,
                value: Value::Empty,
            }),
        }
    }
}

/// Match only if the predicate is true for the result of the provided rule
#[derive(Clone)]
pub struct Predicate<R, F>
where
    R: Rule + Clone,
    F: Fn(&Value) -> bool + Clone,
{
    pub(crate) rule: R,
    pub(crate) predicate: F,
    pub(crate) name: String,
}

pub fn predicate<R, F>(rule: R, predicate: F, name: &str) -> Predicate<R, F>
where
    R: Rule + Clone,
    F: Fn(&Value) -> bool + Clone + 'static,
{
    Predicate {
        rule,
        predicate,
        name: name.into(),
    }
}

impl<R, F> Rule for Predicate<R, F>
where
    R: Rule + Clone,
    F: Fn(&Value) -> bool + Clone,
{
    fn expected(&self) -> String {
        self.name.to_string()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(&self.rule, start, input) {
            Ok(m) => {
                if (self.predicate)(&m.value) {
                    Ok(m)
                } else {
                    if !parser.skipping {
                        parser
                            .expected
                            .entry(start)
                            .or_default()
                            .insert(self.expected());
                    }

                    Err(Fail::fail(start, self.expected(), input))
                }
            }
            e => e,
        }
    }
}

/// Match a list of rules in sequence
#[derive(Clone)]
pub struct Seq<L>
where
    L: List + Clone,
{
    rules: L,
}

pub fn seq<L: List + Clone>(rules: L) -> Seq<L> {
    Seq { rules }
}

impl<L> Rule for Seq<L>
where
    L: List + Clone,
{
    fn expected(&self) -> String {
        self.rules.map(|r| r.expected()).join(",")
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let mut result = vec![];
        let mut end = start;

        self.rules
            .for_each(|r| match parser.parse_rule(r, end, input) {
                Ok(m) => {
                    result.push(m.value);
                    end = m.end;
                    Ok(())
                }
                Err(e) => Err(e),
            })?;

        Ok(Match {
            start,
            end,
            value: Value::Seq(result),
        })
    }
}

/// Match any one of a set of rules
#[derive(Clone)]
pub struct Or<L>
where
    L: List + Clone,
{
    rules: L,
}

pub fn or<L: List + Clone>(rules: L) -> Or<L> {
    Or { rules }
}

impl<L> Rule for Or<L>
where
    L: List + Clone,
{
    fn expected(&self) -> String {
        self.rules.map(|r| r.expected()).join("|")
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let mut result = Err(Fail::fail(start, self.expected(), input));

        let _ = self
            .rules
            .for_each(|r| match parser.parse_rule(r, start, input) {
                Ok(m) => {
                    result = Ok(m);
                    Err(Fail::fail(start, self.expected(), input))
                }
                _ => Ok(()),
            });

        result
    }
}

/// Repeat the given rule zero or more times
///
/// Always returns a successful match, even if empty
#[derive(Clone)]
pub struct Repeat<R>
where
    R: Rule + Clone,
{
    pub(crate) rule: R,
}

pub fn repeat<R>(rule: R) -> Repeat<R>
where
    R: Rule + Clone,
{
    Repeat { rule }
}

impl<R> Rule for Repeat<R>
where
    R: Rule + Clone,
{
    fn expected(&self) -> String {
        self.rule.expected() + "*"
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let mut result = vec![];
        let mut end = start;

        loop {
            match parser.parse_rule(&self.rule, end, input) {
                Ok(m) => {
                    if let Value::Eof = m.value {
                        break;
                    }
                    result.push(m.value);
                    end = m.end;
                }
                _ => break,
            }
        }

        Ok(Match {
            start,
            end,
            value: Value::Seq(result),
        })
    }
}

/// Suppresses token skipping while matching the given rule
#[derive(Clone)]
pub struct Lexer<R>
where
    R: Rule + Clone,
{
    pub(crate) rule: R,
}

pub fn lex<R>(rule: R) -> impl Rule + Clone
where
    R: Rule + Clone,
{
    Lexer { rule: concat(rule) }
}

impl<R> Rule for Lexer<R>
where
    R: Rule + Clone,
{
    fn expected(&self) -> String {
        self.rule.expected()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        parser.lexing.push(());
        let result = parser.parse_rule(&self.rule, start, input);
        parser.lexing.pop();

        result
    }
}

/// Apply the specified action to any successful match produced by the given rule
#[derive(Clone)]
pub struct Action<R, F>
where
    R: Rule + Clone,
    F: Fn(&Value) -> Value + Clone,
{
    pub(crate) rule: R,
    pub(crate) action: F,
}

pub fn action<R, F>(rule: R, action: F) -> Action<R, F>
where
    R: Rule + Clone,
    F: Fn(&Value) -> Value + Clone,
{
    Action { rule, action }
}

impl<R, F> Rule for Action<R, F>
where
    R: Rule + Clone,
    F: Fn(&Value) -> Value + Clone,
{
    fn expected(&self) -> String {
        self.rule.expected()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(&self.rule, start, input) {
            Ok(mut m) => {
                m.value = (self.action)(&m.value);
                Ok(m)
            }
            e => e,
        }
    }
}

/// Forward declaration for a rule, so that it has a name that can be used recursively
///
/// You must set() the rule body before use
///
/// This is based on "Left recursion in Parsing Expression Grammars" by Medeiros et al
/// <https://doi.org/10.1016/j.scico.2014.01.013>
#[derive(Clone)]
pub struct Forward {
    rule: Rc<RefCell<Option<Box<dyn Rule>>>>,
    memo: Rc<RefCell<HashMap<usize, Result<Match, Fail>>>>,
}

pub fn forward() -> Forward {
    Forward {
        rule: Rc::new(RefCell::new(None)),
        memo: Rc::new(RefCell::new(HashMap::new())),
    }
}

impl Forward {
    pub fn set(&mut self, rule: Box<dyn Rule>) {
        *self.rule.borrow_mut() = Some(rule);
    }
}

impl Rule for Forward {
    fn expected(&self) -> String {
        "ForwardDeclaration".into()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        if let Some(rule) = self.rule.borrow().as_ref() {
            // always check if we already have a memoised result first
            if let Some(m) = self.memo.borrow().get(&start) {
                return m.clone();
            }

            let mut longest = start;
            let mut previous: Result<_, Fail> = Err(Fail::fail(start, self.expected(), input));
            self.memo.borrow_mut().insert(start, previous.clone());

            loop {
                // run the parser rule over and over until it fails
                match parser.parse_rule(rule.as_ref(), start, input) {
                    Ok(m) => {
                        // can early out if we parsed the entire input this way
                        if let Value::Eof = m.value {
                            return Ok(m);
                        }

                        if m.end > longest {
                            // update the memo table if we found a longer match
                            longest = m.end;
                            previous = Ok(m);
                            self.memo.borrow_mut().insert(start, previous.clone());
                        } else {
                            // no longer matches possible, return the next-longest match
                            self.memo.borrow_mut().remove(&start);
                            return previous;
                        }
                    }
                    e => {
                        // return failures directly
                        return e;
                    }
                }
            }
        }

        panic!("rule not set on Forward declaration");
    }
}

/// The core parser implementation
pub struct Parser {
    skip: Option<Box<dyn Rule>>,
    lexing: Vec<()>,
    skipping: bool,
    expected: HashMap<usize, BTreeSet<String>>,
}

impl Parser {
    /// Initialise a parser, optionally using a rule to skip tokens
    pub fn new(skip: Option<Box<dyn Rule>>) -> Self {
        Self {
            skip,
            lexing: vec![],
            skipping: false,
            expected: HashMap::new(),
        }
    }

    fn parse_rule(
        &mut self,
        rule: &dyn Rule,
        mut start: usize,
        input: &str,
    ) -> Result<Match, Fail> {
        start = self.skip(start, input);

        let result = rule.parse(self, start, input);
        if let Err(e) = &result {
            if !self.skipping {
                let furthest = self.expected.keys().max().cloned().unwrap_or(e.location);
                let expected = self.expected.entry(furthest).or_default();
                return Err(Fail::fail(furthest, expected.iter().join(","), input));
            }
        }

        result
    }

    fn skip(&mut self, mut start: usize, input: &str) -> usize {
        let mut result = start;

        if self.lexing.is_empty() {
            // have to temporarily take the skip rule to avoid a double-borrow
            if let Some(skip) = self.skip.take() {
                self.lexing.push(());
                self.skipping = true;

                loop {
                    match skip.parse(self, start, input) {
                        Ok(m) => {
                            result = m.end;
                            start = m.end;
                        }
                        Err(_) => break,
                    }
                }

                self.skipping = false;
                self.lexing.pop();

                // make sure we put the skip rule back when we are done
                self.skip = Some(skip);
            }
        }

        result
    }
}
