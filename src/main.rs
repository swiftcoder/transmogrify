use anyhow::Ok;
use parse::{One, Parser, Rule, forward, lex, not, or, repeat, seq};
use rule::{
    char_, digit, enclosed, eof, ident_continue, ident_start, newline, optional, separated_list,
    str_, whitespace,
};

mod cons_list;
mod parse;
mod rule;

fn main() -> anyhow::Result<()> {
    let skip = or(list![
        seq(list![
            str_("//"),
            repeat(seq(list![not(newline()), One {}]))
        ]),
        whitespace(),
    ]);

    let mut parser = Parser::new(Some(Box::new(skip)));

    let ident = lex(seq(list![ident_start(), repeat(ident_continue())]));
    let number = lex(seq(list![
        digit(),
        repeat(digit()),
        optional(or(list![char_('u')]))
    ]));

    let mut type_expr = forward();

    let template_type = seq(list![
        type_expr.clone(),
        enclosed(char_('<'), type_expr.clone(), char_('>')),
    ]);

    type_expr.set(Box::new(or(list![template_type, ident.clone()])));

    let typed_var = seq(list![ident.clone(), char_(':'), type_expr.clone()]);

    let mut expr = forward();

    let addition = seq(list![
        expr.clone(),
        or(list![char_('+'), char_('-')]),
        expr.clone(),
    ]);
    let multiplication = seq(list![
        expr.clone(),
        or(list![char_('*'), char_('/')]),
        expr.clone(),
    ]);
    let comparison = seq(list![
        expr.clone(),
        or(list![
            char_('<'),
            char_('>'),
            str_("=="),
            str_("!="),
            str_("<="),
            str_(">="),
        ]),
        expr.clone(),
    ]);
    let postfix = seq(list![expr.clone(), str_("++")]);
    let parenthical = enclosed(char_('('), expr.clone(), char_(')'));
    let field_access = seq(list![expr.clone(), char_('.'), ident.clone(),]);
    let array_access = seq(list![
        expr.clone(),
        enclosed(char_('['), expr.clone(), char_(']')),
    ]);
    let function_call = seq(list![
        expr.clone(),
        enclosed(
            char_('('),
            separated_list(expr.clone(), char_(',')),
            char_(')'),
        ),
    ]);

    expr.set(Box::new(or(list![
        addition,
        multiplication,
        comparison,
        postfix,
        field_access,
        array_access,
        function_call,
        parenthical,
        ident.clone(),
        number.clone(),
    ])));

    let struct_ = seq(list![
        str_("struct"),
        ident.clone(),
        enclosed(
            char_('{'),
            separated_list(typed_var.clone(), char_(',')),
            char_('}'),
        ),
    ]);

    let var_decl = seq(list![
        str_("var"),
        optional(enclosed(
            char_('<'),
            separated_list(ident.clone(), char_(',')),
            char_('>'),
        )),
        typed_var.clone(),
        char_(';'),
    ]);

    let mut statement = forward();

    let let_ = seq(list![str_("let"), ident.clone(), char_('='), expr.clone(),]);
    let var_ = seq(list![str_("var"), ident.clone(), char_('='), expr.clone(),]);

    let assign = seq(list![
        expr.clone(),
        or(list![char_('='), str_("+="), str_("-=")]),
        expr.clone(),
    ]);

    let return_ = seq(list![str_("return"), expr.clone()]);

    let for_ = seq(list![
        str_("for"),
        enclosed(
            char_('('),
            seq(list![
                var_.clone(),
                char_(';'),
                expr.clone(),
                char_(';'),
                expr.clone(),
            ]),
            char_(')'),
        ),
        enclosed(char_('{'), statement.clone(), char_('}')),
    ]);

    statement.set(Box::new(repeat(or(list![
        seq(list![or(list![let_, var_, assign, return_]), char_(';')]),
        for_,
    ]))));

    let function_ = seq(list![
        str_("fn"),
        ident.clone(),
        enclosed(
            char_('('),
            separated_list(typed_var.clone(), char_(',')),
            char_(')'),
        ),
        str_("->"),
        type_expr.clone(),
        enclosed(char_('{'), statement, char_('}')),
    ]);

    let top_level_statement = or(list![struct_, var_decl, function_]);

    let rule = seq(list![repeat(top_level_statement), eof()]);

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
        var<storage> lights : LightStorage;

        // Texture and sampler.
        var baseColorSampler : sampler;
        var baseColorTexture : texture_2d<f32>;

        fn fragmentMain(worldPos : vec3f,
                        normal : vec3f,
                        uv : vec2f) -> vec4f {
            // Sample the base color of the surface from a texture.
            let baseColor = textureSample(baseColorTexture, baseColorSampler, uv);

            let N = normalize(normal);
            var surfaceColor = vec3f(0);

            // Loop over the scene point lights.
            for (var i = 0u; i < lights.pointCount; i++) {
                let worldToLight = lights.point[i].position - worldPos;
                let dist = length(worldToLight);
                let dir = normalize(worldToLight);

                // Determine the contribution of this light to the surface color.
                let radiance = lights.point[i].color * (1 / pow(dist, 2));
                let nDotL = max(dot(N, dir), 0);

                // Accumulate light contribution to the surface color.
                surfaceColor += baseColor.rgb * radiance * nDotL;
            }

            // Return the accumulated surface color.
            return vec4(surfaceColor, baseColor.a);
        }
    "#,
    )?;
    println!("result: {:?}", result);

    Ok(())
}
