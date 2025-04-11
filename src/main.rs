use anyhow::Ok;
use dyn_clone::clone_box;
use parse::{Parser, Rule, forward, lex, or, repeat, seq};
use rule::{chr, concat, ident_continue, ident_start, whitespace};

mod parse;
mod rule;

fn main() -> anyhow::Result<()> {
    let mut parser = Parser::new(Some(whitespace()));

    let ident = concat(lex(seq([ident_start(), repeat(ident_continue())])));

    let mut expr = forward();

    let addition = seq([clone_box(&*expr), chr('+'), clone_box(&*expr)]);

    expr.set(or([addition, ident]));

    let rule = seq([expr as Box<dyn Rule>]);

    let result = rule.parse(&mut parser, 0, "Hello + World + Foo")?;
    println!("result: {:?}", result);

    Ok(())
}
