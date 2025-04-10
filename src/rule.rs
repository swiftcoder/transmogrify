use crate::parse::{One, Predicate, Rule, Value};

pub fn chr(c: char) -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::String(s) => s == &c.to_string(),
            _ => false,
        }),
        name: c.to_string(),
    })
}

pub fn whitespace() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_whitespace()),
            _ => false,
        }),
        name: "\\s".into(),
    })
}

pub fn horizontal_whitespace() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_whitespace() && c != '\n'),
            _ => false,
        }),
        name: "\\s".into(),
    })
}

pub fn newline() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::String(s) => s.chars().all(|c| c == '\n'),
            _ => false,
        }),
        name: "\\n".into(),
    })
}

pub fn ident_start() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_alphabetic() || c == '_'),
            _ => false,
        }),
        name: "[_a-ZA-Z]".into(),
    })
}

pub fn ident_continue() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_alphanumeric() || c == '_'),
            _ => false,
        }),
        name: "[_a-ZA-Z0-9]".into(),
    })
}

pub fn eof() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: Box::new(move |v| match v {
            Value::Eof => true,
            _ => false,
        }),
        name: "EOF".into(),
    })
}
