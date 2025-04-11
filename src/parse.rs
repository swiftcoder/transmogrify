use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
    error::Error,
    fmt::Display,
    ops::Range,
    rc::Rc,
};

use dyn_clone::{DynClone, clone_box, clone_trait_object};
use itertools::Itertools;

use crate::rule::concat;

#[derive(Debug, Clone)]
pub struct Fail {
    location: usize,
    expected: String,
    found: String,
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

#[derive(Debug, Clone)]
pub struct Match {
    start: usize,
    end: usize,
    value: Value,
}

pub trait Rule: DynClone {
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

clone_trait_object!(Rule);

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

#[derive(Clone)]
pub struct Not {
    rule: Box<dyn Rule>,
}

pub fn not(rule: Box<dyn Rule>) -> Box<dyn Rule> {
    Box::new(Not { rule })
}

impl Rule for Not {
    fn expected(&self) -> String {
        format!("!{}", self.rule.expected())
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(self.rule.as_ref(), start, input) {
            Ok(_) => Err(Fail::fail(start, self.expected(), input)),
            _ => Ok(Match {
                start,
                end: start,
                value: Value::Empty,
            }),
        }
    }
}

#[derive(Clone)]
pub struct Predicate<F>
where
    F: Fn(&Value) -> bool + Clone,
{
    pub(crate) rule: Box<dyn Rule>,
    pub(crate) predicate: F,
    pub(crate) name: String,
}

pub fn predicate<F>(rule: Box<dyn Rule>, predicate: F, name: &str) -> Box<dyn Rule>
where
    F: Fn(&Value) -> bool + Clone + 'static,
{
    Box::new(Predicate {
        rule,
        predicate,
        name: name.into(),
    })
}

impl<F> Rule for Predicate<F>
where
    F: Fn(&Value) -> bool + Clone,
{
    fn expected(&self) -> String {
        self.name.to_string()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(self.rule.as_ref(), start, input) {
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

#[derive(Clone)]
pub struct Seq {
    rules: Vec<Box<dyn Rule>>,
}

pub fn seq<V: IntoIterator<Item = Box<dyn Rule>>>(rules: V) -> Box<dyn Rule> {
    Box::new(Seq {
        rules: rules.into_iter().collect(),
    })
}

impl Rule for Seq {
    fn expected(&self) -> String {
        self.rules.iter().map(|r| r.expected()).join(",")
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let mut result = vec![];
        let mut end = start;

        for r in &self.rules {
            match parser.parse_rule(r.as_ref(), end, input) {
                Ok(m) => {
                    result.push(m.value);
                    end = m.end;
                }
                e => return e,
            }
        }

        Ok(Match {
            start,
            end,
            value: Value::Seq(result),
        })
    }
}

#[derive(Clone)]
pub struct Or {
    rules: Vec<Box<dyn Rule>>,
}

pub fn or<V: IntoIterator<Item = Box<dyn Rule>>>(rules: V) -> Box<dyn Rule> {
    Box::new(Or {
        rules: rules.into_iter().collect(),
    })
}

impl Rule for Or {
    fn expected(&self) -> String {
        self.rules.iter().map(|r| r.expected()).join("|")
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        for r in &self.rules {
            match parser.parse_rule(r.as_ref(), start, input) {
                Ok(m) => return Ok(m),
                _ => {}
            }
        }

        Err(Fail::fail(start, self.expected(), input))
    }
}

#[derive(Clone)]
pub struct Repeat {
    pub(crate) rule: Box<dyn Rule>,
}

pub fn repeat(rule: Box<dyn Rule>) -> Box<dyn Rule> {
    Box::new(Repeat { rule })
}

impl Rule for Repeat {
    fn expected(&self) -> String {
        self.rule.expected() + "*"
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        let mut result = vec![];
        let mut end = start;

        loop {
            match parser.parse_rule(self.rule.as_ref(), end, input) {
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

#[derive(Clone)]
pub struct Lexer {
    pub(crate) rule: Box<dyn Rule>,
}

pub fn lex(rule: Box<dyn Rule>) -> Box<dyn Rule> {
    Box::new(Lexer { rule: concat(rule) })
}

impl Rule for Lexer {
    fn expected(&self) -> String {
        self.rule.expected()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        parser.lexing.push(());
        let result = parser.parse_rule(self.rule.as_ref(), start, input);
        parser.lexing.pop();

        result
    }
}

#[derive(Clone)]
pub struct Action<F>
where
    F: Fn(&Value) -> Value + Clone,
{
    pub(crate) rule: Box<dyn Rule>,
    pub(crate) action: F,
}

pub fn action<F>(rule: Box<dyn Rule>, action: F) -> Box<dyn Rule>
where
    F: Fn(&Value) -> Value + Clone + 'static,
{
    Box::new(Action { rule, action })
}

impl<F> Rule for Action<F>
where
    F: Fn(&Value) -> Value + Clone,
{
    fn expected(&self) -> String {
        self.rule.expected()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(self.rule.as_ref(), start, input) {
            Ok(mut m) => {
                m.value = (self.action)(&m.value);
                Ok(m)
            }
            e => e,
        }
    }
}

/// Forward declaration for a rule, so that it has a name that can be used recursively.
/// You must set() the rule body before use.

/// This is based on "Left recursion in Parsing Expression Grammars" by Medeiros et al
/// https://doi.org/10.1016/j.scico.2014.01.013
#[derive(Clone)]
pub struct Forward {
    rule: Rc<RefCell<Option<Box<dyn Rule>>>>,
    memo: Rc<RefCell<HashMap<usize, Result<Match, Fail>>>>,
}

pub fn forward() -> Box<Forward> {
    Box::new(Forward {
        rule: Rc::new(RefCell::new(None)),
        memo: Rc::new(RefCell::new(HashMap::new())),
    })
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

pub struct Parser {
    skip: Option<Box<dyn Rule>>,
    lexing: Vec<()>,
    skipping: bool,
    expected: HashMap<usize, BTreeSet<String>>,
}

impl Parser {
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
            if let Some(skip) = &self.skip {
                self.lexing.push(());
                self.skipping = true;

                let skip2 = clone_box(&*skip);
                while let Ok(m) = skip2.parse(self, start, input) {
                    result = m.end;
                    start = m.end;
                }

                self.skipping = false;
                self.lexing.pop();
            }
        }

        result
    }
}
