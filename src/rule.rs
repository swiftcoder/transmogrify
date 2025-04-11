use dyn_clone::clone_box;

use crate::parse::{One, Predicate, Rule, Value, action, lex, not, or, predicate, repeat, seq};

pub fn char_(c: char) -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::String(s) => s == &c.to_string(),
            _ => false,
        },
        name: c.to_string(),
    })
}

pub fn str_(s: &str) -> Box<dyn Rule> {
    lex(seq(s.chars().map(|c| char_(c)).collect::<Vec<_>>()))
}

pub fn whitespace() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_whitespace()),
            _ => false,
        },
        name: "\\s".into(),
    })
}

pub fn horizontal_whitespace() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_whitespace() && c != '\n'),
            _ => false,
        },
        name: "\\s".into(),
    })
}

pub fn newline() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::String(s) => s.chars().all(|c| c == '\n'),
            _ => false,
        },
        name: "\\n".into(),
    })
}

pub fn ident_start() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_alphabetic() || c == '_'),
            _ => false,
        },
        name: "[_a-ZA-Z]".into(),
    })
}

pub fn ident_continue() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_alphanumeric() || c == '_'),
            _ => false,
        },
        name: "[_a-ZA-Z0-9]".into(),
    })
}

pub fn digit() -> Box<dyn Rule> {
    predicate(
        Box::new(One {}),
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_numeric()),
            _ => false,
        },
        "\\d".into(),
    )
}

pub fn eof() -> Box<dyn Rule> {
    Box::new(Predicate {
        rule: Box::new(One {}),
        predicate: move |v| match v {
            Value::Eof => true,
            _ => false,
        },
        name: "EOF".into(),
    })
}

pub fn enclosed(
    prefix: Box<dyn Rule>,
    rule: Box<dyn Rule>,
    suffix: Box<dyn Rule>,
) -> Box<dyn Rule> {
    action(seq([prefix, rule, suffix]), |v| match v {
        Value::Seq(s) => s[1].clone(),
        _ => Value::Empty,
    })
}

pub fn optional(rule: Box<dyn Rule>) -> Box<dyn Rule> {
    or([clone_box(&*rule), not(rule)])
}

pub fn separated_list(rule: Box<dyn Rule>, separator: Box<dyn Rule>) -> Box<dyn Rule> {
    let inner = action(
        seq([clone_box(&*separator), clone_box(&*rule)]),
        |v| match v {
            e @ Value::Seq(_) => e.nth(1),
            _ => Value::Empty,
        },
    );

    action(
        optional(seq([rule, repeat(inner), optional(separator)])),
        |v| match v {
            Value::Seq(s) => Value::Seq(
                s[0..s.len() - 1]
                    .iter()
                    .flat_map(|i: &Value| i.flatten_seq())
                    .collect(),
            ),
            _ => Value::Empty,
        },
    )
}

pub fn concat(rule: Box<dyn Rule>) -> Box<dyn Rule> {
    action(rule, |v| Value::String(v.flatten_string()))
}
