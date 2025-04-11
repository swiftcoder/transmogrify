use anyhow::Ok;
use dyn_clone::clone_box;
use parse::{Parser, Rule, forward, lex, or, repeat, seq};
use rule::{char_, enclosed, eof, ident_continue, ident_start, separated_list, str_, whitespace};

mod parse;
mod rule;

fn main() -> anyhow::Result<()> {
    let mut parser = Parser::new(Some(whitespace()));

    let ident = lex(seq([ident_start(), repeat(ident_continue())]));

    let mut type_expr = forward();

    let template_type = seq([
        clone_box(&*type_expr),
        enclosed(char_('<'), clone_box(&*type_expr), char_('>')),
    ]);
    let type_name = clone_box(&*ident);

    type_expr.set(or([template_type, type_name]));

    let typed_var = seq([clone_box(&*ident), char_(':'), clone_box(&*type_expr)]);

    let mut expr = forward();

    let addition = seq([clone_box(&*expr), char_('+'), clone_box(&*expr)]);
    let parenthical = enclosed(char_('('), clone_box(&*expr), char_(')'));
    let array_access = seq([
        clone_box(&*expr),
        enclosed(char_('['), clone_box(&*expr), char_(']')),
    ]);
    let function_call = seq([
        clone_box(&*expr),
        enclosed(
            char_('('),
            separated_list(clone_box(&*expr), char_(',')),
            char_(')'),
        ),
    ]);

    expr.set(or([
        addition,
        array_access,
        function_call,
        parenthical,
        clone_box(&*ident),
    ]));

    let struct_ = seq([
        str_("struct"),
        clone_box(&*ident),
        enclosed(
            char_('{'),
            separated_list(clone_box(&*typed_var), char_(',')),
            char_('}'),
        ),
    ]);

    let mut statement = forward();

    let let_ = seq([
        str_("let"),
        clone_box(&*ident),
        char_('='),
        clone_box(&*expr),
    ]);

    statement.set(repeat(seq([or([let_]), char_(';')])));

    let function_ = seq([
        str_("fn"),
        clone_box(&*ident),
        enclosed(
            char_('('),
            separated_list(clone_box(&*typed_var), char_(',')),
            char_(')'),
        ),
        str_("->"),
        clone_box(&*type_expr),
        enclosed(char_('{'), statement, char_('}')),
    ]);

    let top_level_statement = or([struct_, function_]);

    let rule = seq([repeat(top_level_statement), eof()]);

    let result = rule.parse(
        &mut parser,
        0,
        r#"
        struct PointLight {
            position: vec3f,
            color: vec3f,
        }

        struct LightStorage {
            pointCount : u32,
            point : array<PointLight>,
        }

        fn fragmentMain(worldPos : vec3f,
                        normal : vec3f,
                        uv : vec2f) -> vec4f {
            let x = foo;
        }
    "#,
    )?;
    println!("result: {:?}", result);

    Ok(())
}
