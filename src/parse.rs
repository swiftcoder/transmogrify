use std::{error::Error, fmt::Display};

use itertools::Itertools;

#[derive(Debug)]
pub struct Fail {
    location: usize,
    expected: String,
    found: String,
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

#[derive(Debug)]
pub enum Value {
    String(String),
    Seq(Vec<Value>),
    Eof,
}

#[derive(Debug)]
pub struct Match {
    start: usize,
    end: usize,
    value: Value,
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
pub struct Predicate {
    pub(crate) rule: Box<dyn Rule>,
    pub(crate) predicate: Box<dyn Fn(&Value) -> bool>,
    pub(crate) name: String,
}

impl Rule for Predicate {
    fn expected(&self) -> String {
        self.name.to_string()
    }

    fn parse(&self, parser: &mut Parser, start: usize, input: &str) -> Result<Match, Fail> {
        match parser.parse_rule(self.rule.as_ref(), start, input) {
            Ok(m) => {
                if (self.predicate)(&m.value) {
                    Ok(m)
                } else {
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

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    fn parse_rule(&mut self, rule: &dyn Rule, start: usize, input: &str) -> Result<Match, Fail> {
        rule.parse(self, start, input)
    }
}
