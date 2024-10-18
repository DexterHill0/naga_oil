use indexmap::IndexMap;
use naga::front::wgsl::parse::lexer::{Lexer, Token};

use super::{Composer, ImportDefWithOffset, ImportDefinition};

macro_rules! pos {
    ($span:ident, $offset:ident) => {{
        $span.to_range().unwrap().start + $offset
    }};
}

// this does not deal with escape characters but it can if necessary
fn parse_string<'a>(lexer: &mut Lexer<'a>) -> (String, &'a str, usize) {
    let _ = lexer.next(); // skip `"`;

    let mut out = String::new();
    out.push('"');

    let last_string_chr_pos;

    'outer: loop {
        let (_, span) = lexer.next_raw();
        {
            let range = span.to_range().unwrap();
            for (i, chr) in lexer.source[range.start..range.end].chars().enumerate() {
                if chr == '"' {
                    // in the test case, the worst case is the string has `//` which will make the rest of the line
                    // a Trivia token and therefore exclude everything after `"` including any of the module names
                    // to avoid this we return the rest of source code as a new str starting the the end of the string we just parsed
                    // so in the case of `#import "a//foobar"::b`, the rest of the source would be `::b`
                    last_string_chr_pos = range.start + i + 1;
                    break 'outer;
                }
                out.push(chr);
            }
        }
    }

    out.push('"');

    (
        out,
        &lexer.source[last_string_chr_pos..],
        last_string_chr_pos,
    )
}

// this does allocate more than the previous one
// not sure if thats going to be a probel
pub fn parse_imports<'a>(
    // input: &'a str,
    lexer: Lexer<'a>,
    declared_imports: &mut IndexMap<String, Vec<String>>,
) -> Result<(), (&'a str, usize)> {
    let mut lexer = lexer;
    // the outer vec is each path "level"
    // and the inner vec is each part of each path, for example, assuming there is no popping/truncating
    // a path: `a::b::c, e`
    // is represented as `vec![vec![a, b, c], vec![e]]`
    let mut stack: Vec<Vec<String>> = vec![];
    let mut as_name: Option<&'a str> = None;
    let mut is_deprecated_itemlist = false;

    let mut nest_level: isize = 0;
    let mut is_in_path;

    // when parsing a string a new lexer is created out of the remaining source
    // that means the spans will be incorrect, so this is added in order to correct them
    let mut span_offset = 0;

    // this is parsed in the preprocessor so it should only ignored
    // in tests
    #[cfg(test)]
    {
        lexer.skip(Token::Unknown('#'));
        lexer.skip(Token::Word("import"));
    }

    let last_span = 'outer: loop {
        let (token, token_span) = lexer.peek();

        match token {
            Token::End => break token_span,
            Token::Word(ident) => {
                let _ = lexer.next();

                is_in_path = false;

                if stack.last().is_none() {
                    stack.push(vec![]);
                }

                // NOTE(dexterhill0) this is covering the case where someone writes:
                // `#import a::b c, d::e f`. i dont know if this is allowed syntax however
                // that would produce the result of `a::b::c, a::b::c::d::e::f` which is unintuitive
                // and handling the error would be just as easy as making it work (like below)
                if is_deprecated_itemlist && lexer.peek().0 == Token::Separator(':') {
                    stack.push(vec![]);
                }

                // just inserted a vec above so it can never be None
                let current = stack.last_mut().unwrap();

                current.push(ident.to_string());
            }
            Token::Unknown('"') => {
                is_in_path = false;
                let (path, source_rest, so) = parse_string(&mut lexer);

                span_offset = so;

                lexer = Lexer::new(source_rest);

                if stack.last().is_none() {
                    stack.push(vec![]);
                }

                let current = stack.last_mut().unwrap();

                current.push(path);
            }
            _ => return Err(("expected identifier", pos!(token_span, span_offset))),
        }

        match lexer.peek() {
            (Token::Separator(':'), span) => {
                let _ = lexer.next();

                if !lexer.skip(Token::Separator(':')) {
                    return Err(("expected `::`", pos!(span, span_offset)));
                }

                is_in_path = true;

                if lexer.skip(Token::Paren('{')) {
                    nest_level += 1;
                    // we only push when we encounter `{` as that is the only syntax that allows for more than 1 item name
                    // without `{` the path would only be `x::y::z` and so we don't need to create a new level
                    // we also want to clone the previous stack item so: `w::x::{y, z}` is correctly represented as
                    // `vec![vec![w, x, y], vec![w, y, z]]` instead of `vec![vec![w, y, y], vec![z]]`
                    stack.push(stack[stack.len() - 1].clone());
                }
            }
            (Token::Word("as"), ..) => {
                let _ = lexer.next();

                match lexer.next() {
                    (Token::Word(name), ..) => {
                        as_name = Some(name);
                    }
                    (_, span) => {
                        return Err(("expected identifier after `as`", pos!(span, span_offset)));
                    }
                };
            }
            (Token::Word(name), ..) => {
                let _ = lexer.next();

                #[cfg(not(feature = "allow_deprecated"))]
                tracing::warn!("item list imports are deprecated, please use `rust::style::item_imports` (or use feature `allow_deprecated`)`\n| {}", input);

                as_name = Some(name);

                is_deprecated_itemlist = true;

                // just to get here an ident had to be parsed therefore
                // we can guarantee there will be vector inserted
                let current = stack.last_mut().unwrap();
                current.push(name.to_string());
            }
            (Token::Paren('{'), span) => return Err(("expected `::`", pos!(span, span_offset))),
            _ => {}
        }

        match lexer.peek() {
            (
                t
                @ (Token::Paren('}') | Token::Separator(',') | Token::Separator(';') | Token::End),
                token_span,
            ) => {
                let _ = lexer.next();

                let stack_top = stack.last_mut().unwrap();

                // use either the as name or the
                // top of the stack as that will the item name
                let item_name = as_name
                    .map(String::from)
                    .or_else(|| Some(stack_top.last().unwrap().to_string()))
                    .unwrap();

                declared_imports
                    .entry(item_name)
                    .or_default()
                    .push(stack_top.join("::"));

                match t {
                    Token::Paren('}') => {
                        nest_level -= 1;
                    }
                    Token::Separator(';') | Token::End => break token_span,
                    _ => (),
                }

                while let (Token::Paren('}'), span) = lexer.peek() {
                    let _ = lexer.next();
                    nest_level -= 1;

                    if nest_level < 0 {
                        break 'outer span;
                    }

                    lexer.skip(Token::Separator(','));
                }

                // pop the item name from the stack
                stack_top.pop();

                // NOTE(dexterhill0): here i am assuming syntax like:
                // `#import a::b c, d::e f` is not allowed
                if !is_deprecated_itemlist {
                    // the nest level will also represent how far down the path we are
                    // if we truncate using the nest level, when we move out of a level it will correctly
                    // update the path
                    stack.truncate(nest_level as usize); // if its below 0 then it will have errored
                }

                lexer.skip(Token::Separator(','));

                as_name = None;
            }
            // if we find any other token, dont consume but still break
            (_, span) if nest_level == 0 && !is_in_path => {
                break span;
            }
            _ => continue,
        }
    };

    if nest_level != 0 {
        return Err(("unclosed brace", pos!(last_span, span_offset)));
    }

    // dbg!(declared_imports);

    Ok(())
}

// pub fn substitute_identifiers(
//     input: &str,
//     offset: usize,
//     declared_imports: &IndexMap<String, Vec<String>>,
//     used_imports: &mut IndexMap<String, ImportDefWithOffset>,
//     allow_ambiguous: bool,
// ) -> Result<String, usize> {
//     let tokens = Tokenizer::new(input, true);
//     let mut output = String::with_capacity(input.len());
//     let mut in_substitution_position = true;

//     for token in tokens {
//         match token {
//             Token::Identifier(ident, token_pos) => {
//                 if in_substitution_position {
//                     let (first, residual) = ident.split_once("::").unwrap_or((ident, ""));
//                     let full_paths = declared_imports
//                         .get(first)
//                         .cloned()
//                         .unwrap_or(vec![first.to_owned()]);

//                     if !allow_ambiguous && full_paths.len() > 1 {
//                         return Err(offset + token_pos);
//                     }

//                     for mut full_path in full_paths {
//                         if !residual.is_empty() {
//                             full_path.push_str("::");
//                             full_path.push_str(residual);
//                         }

//                         if let Some((module, item)) = full_path.rsplit_once("::") {
//                             used_imports
//                                 .entry(module.to_owned())
//                                 .or_insert_with(|| ImportDefWithOffset {
//                                     definition: ImportDefinition {
//                                         import: module.to_owned(),
//                                         ..Default::default()
//                                     },
//                                     offset: offset + token_pos,
//                                 })
//                                 .definition
//                                 .items
//                                 .push(item.to_owned());
//                             output.push_str(item);
//                             output.push_str(&Composer::decorate(module));
//                         } else if full_path.find('"').is_some() {
//                             // we don't want to replace local variables that shadow quoted module imports with the
//                             // quoted name as that won't compile.
//                             // since quoted items always refer to modules, we can just emit the original ident
//                             // in this case
//                             output.push_str(ident);
//                         } else {
//                             // if there are no quotes we do the replacement. this means that individually imported
//                             // items can be used, and any shadowing local variables get harmlessly renamed.
//                             // TODO: it can lead to weird errors, but such is life
//                             output.push_str(&full_path);
//                         }
//                     }
//                 } else {
//                     output.push_str(ident);
//                 }
//             }
//             Token::Other(other, _) => {
//                 output.push(other);
//                 if other == '.' || other == '@' {
//                     in_substitution_position = false;
//                     continue;
//                 }
//             }
//             Token::Whitespace(ws, _) => output.push_str(ws),
//         }

//         in_substitution_position = true;
//     }

//     Ok(output)
// }

#[cfg(test)]
fn test_parse(input: &str) -> Result<IndexMap<String, Vec<String>>, (&str, usize)> {
    let mut declared_imports = IndexMap::default();
    parse_imports(Lexer::new(input), &mut declared_imports)?;
    Ok(declared_imports)
}

#[test]
fn import_tokens() {
    let input = r"
        #import a::b
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([(
            "b".to_owned(),
            vec!("a::b".to_owned())
        )]))
    );

    let input = r"
        #import a::{b, c}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("b".to_owned(), vec!("a::b".to_owned())),
            ("c".to_owned(), vec!("a::c".to_owned())),
        ]))
    );

    let input = r"
        #import a::{b as d, c}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("d".to_owned(), vec!("a::b".to_owned())),
            ("c".to_owned(), vec!("a::c".to_owned())),
        ]))
    );

    let input = r"
        #import a::{b::{c, d}, e}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("d".to_owned(), vec!("a::b::d".to_owned())),
            ("e".to_owned(), vec!("a::e".to_owned())),
        ]))
    );

    let input = r"
        #import a::b::{c, d}, e
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("d".to_owned(), vec!("a::b::d".to_owned())),
            ("e".to_owned(), vec!("e".to_owned())),
        ]))
    );

    let input = r"
        #import a, b
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("a".to_owned(), vec!("a".to_owned())),
            ("b".to_owned(), vec!("b".to_owned())),
        ]))
    );

    let input = r"
        #import a::b c, d
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("d".to_owned(), vec!("a::b::d".to_owned())),
        ]))
    );

    let input = r"
        #import a::b c, d::e f
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("c".to_owned(), vec!("a::b::c".to_owned())),
            ("f".to_owned(), vec!("d::e::f".to_owned())),
        ]))
    );

    let input = r"
        #import a::b c
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([(
            "c".to_owned(),
            vec!("a::b::c".to_owned())
        ),]))
    );

    let input = r"
        #import a::b::{c::{d, e}, f, g::{h as i, j}}
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("d".to_owned(), vec!("a::b::c::d".to_owned())),
            ("e".to_owned(), vec!("a::b::c::e".to_owned())),
            ("f".to_owned(), vec!("a::b::f".to_owned())),
            ("i".to_owned(), vec!("a::b::g::h".to_owned())),
            ("j".to_owned(), vec!("a::b::g::j".to_owned())),
        ]))
    );

    let input = r"
        #import a::b::{
            c::{d, e},
            f,
            g::{
                h as i,
                j::k::l as m,
            }
        }
    ";
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            ("d".to_owned(), vec!("a::b::c::d".to_owned())),
            ("e".to_owned(), vec!("a::b::c::e".to_owned())),
            ("f".to_owned(), vec!("a::b::f".to_owned())),
            ("i".to_owned(), vec!("a::b::g::h".to_owned())),
            ("m".to_owned(), vec!("a::b::g::j::k::l".to_owned())),
        ]))
    );

    let input = r#"
        #import "path//with\ all sorts of .stuff"::{a, b}
    "#;
    assert_eq!(
        test_parse(input),
        Ok(IndexMap::from_iter([
            (
                "a".to_owned(),
                vec!(r#""path//with\ all sorts of .stuff"::a"#.to_owned())
            ),
            (
                "b".to_owned(),
                vec!(r#""path//with\ all sorts of .stuff"::b"#.to_owned())
            ),
        ]))
    );

    let input = r"
        #import a::b::{
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a::b::{{c}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a::b::{c}}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a::b{{c,d}}
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a:b
    ";
    assert!(test_parse(input).is_err());

    let input = r"
        #import a:::b
    ";
    assert!(test_parse(input).is_err());
}
