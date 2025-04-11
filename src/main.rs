use anyhow::Ok;
use dyn_clone::clone_box;
use parse::{One, Parser, Rule, forward, lex, not, or, repeat, seq};
use rule::{
    char_, digit, enclosed, eof, ident_continue, ident_start, newline, optional, separated_list,
    str_, whitespace,
};

mod parse;
mod rule;

fn main() -> anyhow::Result<()> {
    let skip = or([
        seq([str_("//"), repeat(seq([not(newline()), Box::new(One {})]))]),
        whitespace(),
    ]);

    let mut parser = Parser::new(Some(skip));

    let ident = lex(seq([ident_start(), repeat(ident_continue())]));
    let number = lex(seq([digit(), repeat(digit()), optional(or([char_('u')]))]));

    let mut type_expr = forward();

    let template_type = seq([
        clone_box(&*type_expr),
        enclosed(char_('<'), clone_box(&*type_expr), char_('>')),
    ]);
    let type_name = clone_box(&*ident);

    type_expr.set(or([template_type, type_name]));

    let typed_var = seq([clone_box(&*ident), char_(':'), clone_box(&*type_expr)]);

    let mut expr = forward();

    let addition = seq([
        clone_box(&*expr),
        or([char_('+'), char_('-')]),
        clone_box(&*expr),
    ]);
    let multiplication = seq([
        clone_box(&*expr),
        or([char_('*'), char_('/')]),
        clone_box(&*expr),
    ]);
    let comparison = seq([clone_box(&*expr), char_('<'), clone_box(&*expr)]);
    let postfix = seq([clone_box(&*expr), str_("++")]);
    let parenthical = enclosed(char_('('), clone_box(&*expr), char_(')'));
    let field_access = seq([
        clone_box(&*expr) as Box<dyn Rule>,
        char_('.'),
        clone_box(&*ident),
    ]);
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
        multiplication,
        comparison,
        postfix,
        field_access,
        array_access,
        function_call,
        parenthical,
        clone_box(&*ident),
        clone_box(&*number),
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
    let var_ = seq([
        str_("var"),
        clone_box(&*ident),
        char_('='),
        clone_box(&*expr),
    ]);

    let assign = seq([
        clone_box(&*expr),
        or([char_('='), str_("+="), str_("-=")]),
        clone_box(&*expr),
    ]);

    let return_ = seq([str_("return"), clone_box(&*expr)]);

    let for_ = seq([
        str_("for"),
        enclosed(
            char_('('),
            seq([
                clone_box(&*var_),
                char_(';'),
                clone_box(&*expr),
                char_(';'),
                clone_box(&*expr),
            ]),
            char_(')'),
        ),
        enclosed(char_('{'), clone_box(&*statement), char_('}')),
    ]);

    statement.set(repeat(or([
        seq([or([let_, var_, assign, return_]), char_(';')]),
        for_,
    ])));

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
            // Sample the base color of the surface from a texture.
            let baseColor = textureSample(baseColorTexture, baseColorSampler, uv);

            let N = normalize(normal);
            var surfaceColor = vec3f(0);

            for (var i = 0u; i < lights.pointCount; i++) {
                let worldToLight = lights.point[i].position - worldPos;
                let dist = length(worldToLight);
                let dir = normalize(worldToLight);

                let radiance = lights.point[i].color * (1 / pow(dist, 2));
                let nDotL = max(dot(N, dir), 0);

                surfaceColor += baseColor.rgb * radiance * nDotL;
            }

            return vec4(surfaceColor, baseColor.a);
        }
    "#,
    )?;
    println!("result: {:?}", result);

    Ok(())
}
