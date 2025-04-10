use anyhow::Ok;
use parse::{Parser, or, repeat, seq};
use rule::{chr, horizontal_whitespace, ident_continue, ident_start};

mod parse;
mod rule;

fn main() -> anyhow::Result<()> {
    println!("Hello, world!");

    let mut parser = Parser::new();

    let ident = seq([ident_start(), repeat(ident_continue())]);
    let rule = seq([ident, chr(','), horizontal_whitespace(), ident]);

    let result = rule.parse(&mut parser, 0, "Hello, World!")?;
    println!("result: {:?}", result);

    Ok(())
}
