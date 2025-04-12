use crate::{
    list,
    parse::{One, Rule, Value, action, lex, not, or, predicate, repeat, seq},
};

pub fn char_(c: char) -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s == &c.to_string(),
            _ => false,
        },
        &c.to_string(),
    )
}

pub fn str_(s: &str) -> impl Rule + Clone {
    lex(seq(s.chars().map(|c| char_(c)).collect::<Vec<_>>()))
}

pub fn whitespace() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_whitespace()),
            _ => false,
        },
        "\\s",
    )
}

pub fn horizontal_whitespace() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_whitespace() && c != '\n'),
            _ => false,
        },
        "\\s",
    )
}

pub fn newline() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c == '\n'),
            _ => false,
        },
        "\\n",
    )
}

pub fn ident_start() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_alphabetic() || c == '_'),
            _ => false,
        },
        "[_a-ZA-Z]",
    )
}

pub fn ident_continue() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_alphanumeric() || c == '_'),
            _ => false,
        },
        "[_a-ZA-Z0-9]".into(),
    )
}

pub fn digit() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::String(s) => s.chars().all(|c| c.is_numeric()),
            _ => false,
        },
        "\\d".into(),
    )
}

pub fn eof() -> impl Rule + Clone {
    predicate(
        One {},
        move |v| match v {
            Value::Eof => true,
            _ => false,
        },
        "EOF",
    )
}

pub fn enclosed<P, R, S>(prefix: P, rule: R, suffix: S) -> impl Rule + Clone
where
    P: Rule + Clone,
    R: Rule + Clone,
    S: Rule + Clone,
{
    action(seq(list![prefix, rule, suffix]), |v| match v {
        Value::Seq(s) => s[1].clone(),
        _ => Value::Empty,
    })
}

pub fn optional<R>(rule: R) -> impl Rule + Clone
where
    R: Rule + Clone,
{
    or(list![rule.clone(), not(rule)])
}

pub fn separated_list<R, S>(rule: R, separator: S) -> impl Rule + Clone
where
    R: Rule + Clone,
    S: Rule + Clone,
{
    let inner = action(seq(list![separator.clone(), rule.clone()]), |v| match v {
        e @ Value::Seq(_) => e.nth(1),
        _ => Value::Empty,
    });

    let outer = optional(seq(list![rule, repeat(inner), optional(separator)]));

    action(outer, |v| match v {
        Value::Seq(s) => Value::Seq(
            s[0..s.len() - 1]
                .iter()
                .flat_map(|i: &Value| i.flatten_seq())
                .collect(),
        ),
        _ => Value::Empty,
    })
}

pub fn concat<R>(rule: R) -> impl Rule + Clone
where
    R: Rule + Clone,
{
    action(rule, |v: &Value| Value::String(v.flatten_string()))
}
