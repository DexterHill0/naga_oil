use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use indexmap::IndexMap;
use naga::front::wgsl::parse::{
    lexer::{Lexer, Token},
    number::Number,
};
use regex::Regex;

use super::{
    comment_strip_iter::CommentReplaceExt, parse_imports::parse_imports, ComposerErrorInner,
    ImportDefWithOffset, ShaderDefValue,
};

struct TokenDisplay<'a>(pub Token<'a>);
impl ToString for TokenDisplay<'_> {
    fn to_string(&self) -> String {
        match self.0 {
            Token::Separator(c) | Token::Paren(c) | Token::Unknown(c) => format!("character `{c}`"),
            Token::Attribute => "attribute".into(),
            Token::Number(_) => "number".into(),
            Token::Word(w) => format!("word `{w}`"),
            Token::Operation(o)
            | Token::LogicalOperation(o)
            | Token::ShiftOperation(o)
            | Token::AssignmentOperation(o) => format!("operator `{o}`"),
            Token::IncrementOperation => "operator `++`".into(),
            Token::DecrementOperation => "operator `--`".into(),
            Token::Arrow => "arrow".into(),
            Token::Trivia => "comment".into(),
            Token::End => "end of file".into(),
        }
    }
}

enum ShaderDefIfOp {
    Gt,
    Gte,
    Lt,
    Lte,
    Ne,
    Eq,
}

impl ToString for ShaderDefIfOp {
    fn to_string(&self) -> String {
        match self {
            ShaderDefIfOp::Gt => ">",
            ShaderDefIfOp::Gte => ">=",
            ShaderDefIfOp::Lt => "<",
            ShaderDefIfOp::Lte => "<=",
            ShaderDefIfOp::Ne => "!=",
            ShaderDefIfOp::Eq => "==",
        }
        .into()
    }
}

macro_rules! get_next_or_error {
    ($lexer:ident, $tok:path, $offset:ident) => {{
        match $lexer.next() {
            ($tok(v), ..) => Ok(v),
            tok => {
                return Err(ComposerErrorInner::UnexpectedTokenInShaderDirective {
                    pos: $offset,
                    expected: "definition name".into(),
                    found: TokenDisplay(tok.0).to_string(),
                })
            }
        }
    }};
}

#[derive(Debug)]
pub struct Preprocessor<'a> {
    // version_regex: Regex,
    // ifdef_regex: Regex,
    // ifndef_regex: Regex,
    // ifop_regex: Regex,
    // else_regex: Regex,
    // endif_regex: Regex,
    // def_regex: Regex,
    // def_regex_delimited: Regex,
    // import_regex: Regex,
    // define_import_path_regex: Regex,
    // define_shader_def_regex: Regex,
    // TODO: remove if uneeded
    _p: std::marker::PhantomData<&'a ()>,
    // lexer: Lexer<'a>,
}

impl Default for Preprocessor<'_> {
    fn default() -> Self {
        Self {
            // version_regex: Regex::new(r"^\s*#version\s+([0-9]+)").unwrap(),
            // ifdef_regex: Regex::new(r"^\s*#\s*(else\s+)?\s*ifdef\s+([\w|\d|_]+)").unwrap(),
            // ifndef_regex: Regex::new(r"^\s*#\s*(else\s+)?\s*ifndef\s+([\w|\d|_]+)").unwrap(),
            // ifop_regex: Regex::new(
            //     r"^\s*#\s*(else\s+)?\s*if\s+([\w|\d|_]+)\s*([=!<>]*)\s*([-\w|\d]+)",
            // )
            // .unwrap(),
            // else_regex: Regex::new(r"^\s*#\s*else").unwrap(),
            // endif_regex: Regex::new(r"^\s*#\s*endif").unwrap(),
            // def_regex: Regex::new(r"#\s*([\w|\d|_]+)").unwrap(),
            // def_regex_delimited: Regex::new(r"#\s*\{([\w|\d|_]+)\}").unwrap(),
            // import_regex: Regex::new(r"^\s*#\s*import\s").unwrap(),
            // define_import_path_regex: Regex::new(r"^\s*#\s*define_import_path\s+([^\s]+)").unwrap(),
            // define_shader_def_regex: Regex::new(r"^\s*#\s*define\s+([\w|\d|_]+)\s*([-\w|\d]+)?")
            //     .unwrap(),

            // constructing the lexer is very cheap so i think this should be okay
            // i didnt want to wrap it in an option because that would cause downstream
            // code to be worse
            // lexer: Lexer::new(""),
            _p: std::marker::PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct PreprocessorMetaData {
    pub name: Option<String>,
    pub imports: Vec<ImportDefWithOffset>,
    pub defines: HashMap<String, ShaderDefValue>,
    pub effective_defs: HashSet<String>,
}

enum ScopeLevel {
    Active,           // conditions have been met
    PreviouslyActive, // conditions have previously been met
    NotActive,        // no conditions yet met
}

struct Scope(Vec<ScopeLevel>);

impl Scope {
    fn new() -> Self {
        Self(vec![ScopeLevel::Active])
    }

    fn branch(
        &mut self,
        is_else: bool,
        condition: bool,
        offset: usize,
    ) -> Result<(), ComposerErrorInner> {
        if is_else {
            let prev_scope = self.0.pop().unwrap();
            let parent_scope = self
                .0
                .last()
                .ok_or(ComposerErrorInner::ElseWithoutCondition(offset))?;
            let new_scope = if !matches!(parent_scope, ScopeLevel::Active) {
                ScopeLevel::NotActive
            } else if !matches!(prev_scope, ScopeLevel::NotActive) {
                ScopeLevel::PreviouslyActive
            } else if condition {
                ScopeLevel::Active
            } else {
                ScopeLevel::NotActive
            };

            self.0.push(new_scope);
        } else {
            let parent_scope = self.0.last().unwrap_or(&ScopeLevel::Active);
            let new_scope = if matches!(parent_scope, ScopeLevel::Active) && condition {
                ScopeLevel::Active
            } else {
                ScopeLevel::NotActive
            };

            self.0.push(new_scope);
        }

        Ok(())
    }

    fn pop(&mut self, offset: usize) -> Result<(), ComposerErrorInner> {
        self.0.pop();
        if self.0.is_empty() {
            Err(ComposerErrorInner::TooManyEndIfs(offset))
        } else {
            Ok(())
        }
    }

    fn active(&self) -> bool {
        matches!(self.0.last().unwrap(), ScopeLevel::Active)
    }

    fn finish(&self, offset: usize) -> Result<(), ComposerErrorInner> {
        if self.0.len() != 1 {
            Err(ComposerErrorInner::NotEnoughEndIfs(offset))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct PreprocessOutput {
    pub preprocessed_source: String,
    pub imports: Vec<ImportDefWithOffset>,
}

impl<'p> Preprocessor<'p> {
    fn get_shader_def_value_string(
        &self,
        lexer: &mut Lexer<'_>,
        offset: usize,
    ) -> Result<String, ComposerErrorInner> {
        Ok(match lexer.next() {
            (Token::Word("true"), ..) => "true".into(),
            (Token::Word("false"), ..) => "false".into(),
            (Token::Number(num), ..) => match num {
                Ok(Number::I32(n)) => n.to_string(),
                Ok(Number::U32(n)) => n.to_string(),
                Ok(Number::I64(n)) => n.to_string(),
                Ok(Number::U64(n)) => n.to_string(),
                Ok(..) => return Err(ComposerErrorInner::InvalidNumberInDefComparison(offset)),
                Err(e) => return Err(ComposerErrorInner::WglsNumberError(offset, e)),
            },

            tok => {
                return Err(ComposerErrorInner::UnexpectedTokenInShaderDirective {
                    pos: offset,
                    expected: "bool/int/uint".into(),
                    found: TokenDisplay(tok.0).to_string(),
                })
            }
        })
    }

    fn get_next_word(
        &self,
        lexer: &mut Lexer<'p>,
        offset: usize,
    ) -> Result<&'p str, ComposerErrorInner> {
        match lexer.next() {
            (Token::Word(v), ..) => Ok(v),
            tok => {
                return Err(ComposerErrorInner::UnexpectedTokenInShaderDirective {
                    pos: offset,
                    expected: "definition name".into(),
                    found: TokenDisplay(tok.0).to_string(),
                })
            }
        }
    }

    fn check_scope(
        &self,
        shader_defs: &HashMap<String, ShaderDefValue>,
        mut lexer: Lexer<'p>,
        scope: Option<&mut Scope>,
        offset: usize,
    ) -> Result<(bool, Option<&'p str>), ComposerErrorInner> {
        let is_else = lexer.skip(Token::Word("else"));

        match (is_else, lexer.next()) {
            // either `is_else` will be true, `else` will have been skipped and therefore the following token is `ifdef`
            // or `is_else` will be false, no token will have been skippd and therefore the next token is `ifdef`
            (_, (Token::Word("ifdef"), ..)) => {
                let def = self.get_next_word(&mut lexer, offset)?;
                let cond = shader_defs.contains_key(def);
                scope.map_or(Ok(()), |scope| scope.branch(is_else, cond, offset))?;
                return Ok((true, Some(def)));
            }
            (_, (Token::Word("ifndef"), ..)) => {
                let def = self.get_next_word(&mut lexer, offset)?;
                let cond = !shader_defs.contains_key(def);
                scope.map_or(Ok(()), |scope| scope.branch(is_else, cond, offset))?;
                return Ok((true, Some(def)));
            }
            (_, (Token::Word("if"), ..)) => {
                let def = self.get_next_word(&mut lexer, offset)?;
                let op = match lexer.next() {
                    // logical operation includes && and || which we dont want
                    (Token::LogicalOperation('='), ..) => ShaderDefIfOp::Eq,
                    (Token::LogicalOperation('!'), ..) => ShaderDefIfOp::Ne,
                    (Token::LogicalOperation('<'), ..) => ShaderDefIfOp::Lte,
                    (Token::LogicalOperation('>'), ..) => ShaderDefIfOp::Gte,
                    (Token::Paren('<'), ..) => ShaderDefIfOp::Lt,
                    (Token::Paren('>'), ..) => ShaderDefIfOp::Gt,
                    tok => {
                        return Err(ComposerErrorInner::UnexpectedTokenInShaderDirective {
                            pos: offset,
                            expected: "comparison operator".into(),
                            found: TokenDisplay(tok.0).to_string(),
                        })
                    }
                };

                let val = self.get_shader_def_value_string(&mut lexer, offset)?;

                let invalid_def = |ty: &str| ComposerErrorInner::InvalidShaderDefComparisonValue {
                    pos: offset,
                    shader_def_name: def.to_string(),
                    value: val.as_str().to_string(),
                    expected: ty.to_string(),
                };

                let def_value =
                    shader_defs
                        .get(def)
                        .ok_or(ComposerErrorInner::UnknownShaderDef {
                            pos: offset,
                            shader_def_name: def.to_string(),
                        })?;

                fn act_on<T: Eq + Ord>(
                    a: T,
                    b: T,
                    op: ShaderDefIfOp,
                    pos: usize,
                ) -> Result<bool, ComposerErrorInner> {
                    match op {
                        ShaderDefIfOp::Eq => Ok(a == b),
                        ShaderDefIfOp::Ne => Ok(a != b),
                        ShaderDefIfOp::Gt => Ok(a > b),
                        ShaderDefIfOp::Gte => Ok(a >= b),
                        ShaderDefIfOp::Lt => Ok(a < b),
                        ShaderDefIfOp::Lte => Ok(a <= b),
                        _ => Err(ComposerErrorInner::UnknownShaderDefOperator {
                            pos,
                            operator: op.to_string(),
                        }),
                    }
                }

                let new_scope = match def_value {
                    ShaderDefValue::Bool(def_value) => {
                        let val = val.as_str().parse().map_err(|_| invalid_def("bool"))?;
                        act_on(*def_value, val, op, offset)?
                    }
                    ShaderDefValue::Int(def_value) => {
                        let val = val.as_str().parse().map_err(|_| invalid_def("int"))?;
                        act_on(*def_value, val, op, offset)?
                    }
                    ShaderDefValue::UInt(def_value) => {
                        let val = val.as_str().parse().map_err(|_| invalid_def("uint"))?;
                        act_on(*def_value, val, op, offset)?
                    }
                };

                scope.map_or(Ok(()), |scope| scope.branch(is_else, new_scope, offset))?;
                return Ok((true, Some(def)));
            }
            // else case
            (true, ..) => {
                scope.map_or(Ok(()), |scope| scope.branch(true, true, offset))?;
                return Ok((true, None));
            }
            (false, (Token::Word("endif"), ..)) => {
                scope.map_or(Ok(()), |scope| scope.pop(offset))?;
                return Ok((true, None));
            }
            _ => (),
        }

        Ok((false, None))
    }

    // process #if[(n)?def]? / #else / #endif preprocessor directives,
    // strip module name and imports
    // also strip "#version xxx"
    // replace items with resolved decorated names
    pub fn preprocess(
        &mut self,
        shader_str: &str,
        shader_defs: &HashMap<String, ShaderDefValue>,
    ) -> Result<PreprocessOutput, ComposerErrorInner> {
        // let mut declared_imports = IndexMap::new();
        // let mut used_imports = IndexMap::new();
        let mut scope = Scope::new();
        let mut final_string = String::new();
        let mut offset = 0;

        /*
        // this code broadly stolen from bevy_render::ShaderProcessor
        let mut lines = shader_str.lines();
        let mut lines = lines.replace_comments().zip(shader_str.lines()).peekable();

        while let Some((mut line, original_line)) = lines.next() {
            let mut output = false;

            if let Some(cap) = self.version_regex.captures(&line) {
                let v = cap.get(1).unwrap().as_str();
                if v != "440" && v != "450" {
                    return Err(ComposerErrorInner::GlslInvalidVersion(offset));
                }
            } else if self
                .check_scope(shader_defs, &line, Some(&mut scope), offset)?
                .0
                || self.define_import_path_regex.captures(&line).is_some()
                || self.define_shader_def_regex.captures(&line).is_some()
            {
                // ignore
            } else if scope.active() {
                if self.import_regex.is_match(&line) {
                    let mut import_lines = String::default();
                    let mut open_count = 0;
                    let initial_offset = offset;

                    loop {
                        // output spaces for removed lines to keep spans consistent (errors report against substituted_source, which is not preprocessed)
                        final_string.extend(std::iter::repeat(" ").take(line.len()));
                        offset += line.len() + 1;

                        // PERF: Ideally we don't do multiple `match_indices` passes over `line`
                        // in addition to the final pass for the import parse
                        open_count += line.match_indices('{').count();
                        open_count = open_count.saturating_sub(line.match_indices('}').count());

                        // PERF: it's bad that we allocate here. ideally we would use something like
                        //     let import_lines = &shader_str[initial_offset..offset]
                        // but we need the comments removed, and the iterator approach doesn't make that easy
                        import_lines.push_str(&line);
                        import_lines.push('\n');

                        if open_count == 0 || lines.peek().is_none() {
                            break;
                        }

                        final_string.push('\n');
                        line = lines.next().unwrap().0;
                    }

                    parse_imports(import_lines.as_str(), &mut declared_imports).map_err(
                        |(err, line_offset)| {
                            ComposerErrorInner::ImportParseError(
                                err.to_owned(),
                                initial_offset + line_offset,
                            )
                        },
                    )?;
                    output = true;
                } else {
                    let replaced_lines = [original_line, &line].map(|input| {
                        let mut output = input.to_string();
                        for capture in self.def_regex.captures_iter(input) {
                            let def = capture.get(1).unwrap();
                            if let Some(def) = shader_defs.get(def.as_str()) {
                                output = self
                                    .def_regex
                                    .replace(&output, def.value_as_string())
                                    .to_string();
                            }
                        }
                        for capture in self.def_regex_delimited.captures_iter(input) {
                            let def = capture.get(1).unwrap();
                            if let Some(def) = shader_defs.get(def.as_str()) {
                                output = self
                                    .def_regex_delimited
                                    .replace(&output, def.value_as_string())
                                    .to_string();
                            }
                        }
                        output
                    });

                    let original_line = &replaced_lines[0];
                    let decommented_line = &replaced_lines[1];

                    // we don't want to capture imports from comments so we run using a dummy used_imports, and disregard any errors
                    let item_replaced_line = substitute_identifiers(
                        original_line,
                        offset,
                        &declared_imports,
                        &mut Default::default(),
                        true,
                    )
                    .unwrap();
                    // we also run against the de-commented line to replace real imports, and throw an error if appropriate
                    let _ = substitute_identifiers(
                        decommented_line,
                        offset,
                        &declared_imports,
                        &mut used_imports,
                        false,
                    )
                    .map_err(|pos| {
                        ComposerErrorInner::ImportParseError(
                            "Ambiguous import path for item".to_owned(),
                            pos,
                        )
                    })?;

                    final_string.push_str(&item_replaced_line);
                    let diff = line.len().saturating_sub(item_replaced_line.len());
                    final_string.extend(std::iter::repeat(" ").take(diff));
                    offset += original_line.len() + 1;
                    output = true;
                }
            }

            if !output {
                // output spaces for removed lines to keep spans consistent (errors report against substituted_source, which is not preprocessed)
                final_string.extend(std::iter::repeat(" ").take(line.len()));
                offset += line.len() + 1;
            }
            final_string.push('\n');
        }

        scope.finish(offset)?;

        */

        todo!();
        // Ok(PreprocessOutput {
        //     preprocessed_source: final_string,
        //     imports: used_imports.into_values().collect(),
        // })
    }

    // extract module name and all possible imports
    pub fn get_preprocessor_metadata(
        &mut self,
        shader_str: &str,
        allow_defines: bool,
    ) -> Result<PreprocessorMetaData, ComposerErrorInner> {
        // TODO(dexterhill0): does the lexer even need to be on `self`?
        let mut lexer = Lexer::new(shader_str);

        let mut declared_imports = IndexMap::default();
        // let mut used_imports = IndexMap::default();
        let mut name = None;
        let mut offset = 0;
        let mut defines = HashMap::default();
        let mut effective_defs = HashSet::default();

        loop {
            let (token, token_span) = lexer.next();

            if token == Token::End {
                break;
            }

            // this can be changed in the future if need be
            if !matches!(token, Token::Unknown('#')) {
                continue;
            }

            let (is_scope, def) =
                self.check_scope(&HashMap::default(), lexer.clone(), None, offset)?;

            if is_scope {
                if let Some(def) = def {
                    effective_defs.insert(def.to_owned());
                }
            }

            // naga lexer doesnt count '#' as a valid character
            // but luckily for us it still stores it as an "unkown" token
            // personally i think `@` would work better but that would be a breaking change
            match lexer.peek() {
                (Token::Word("import"), directive_span) => {
                    let mut import_lines = String::default();
                    let mut open_count = 0;
                    let initial_offset = offset;

                    // let mut check = lexer.clone();
                    // let mut indent = 1;

                    // let after_close = loop {
                    //     match check.next() {
                    //         Token::Paren('{') => indent += 1,
                    //         Token::End => {
                    //             return Err(SyntaxError::UnmatchedToken {
                    //                 not_found: Token::RParen,
                    //                 for_char: Token::LParen,
                    //                 area: self.make_area(start),
                    //             })
                    //         }
                    //         Token::Paren('}') => {
                    //             indent -= 1;
                    //             if indent == 0 {
                    //                 break check.next()?;
                    //             }
                    //         }
                    //         _ => (),
                    //     }
                    // };

                    //         loop {
                    //             // PERF: Ideally we don't do multiple `match_indices` passes over `line`
                    //             // in addition to the final pass for the import parse
                    //             open_count += line.match_indices('{').count();
                    //             open_count = open_count.saturating_sub(line.match_indices('}').count());

                    //             // PERF: it's bad that we allocate here. ideally we would use something like
                    //             //     let import_lines = &shader_str[initial_offset..offset]
                    //             // but we need the comments removed, and the iterator approach doesn't make that easy
                    //             import_lines.push_str(&line);
                    //             import_lines.push('\n');

                    //             if open_count == 0 || lines.peek().is_none() {
                    //                 break;
                    //             }

                    //             // output spaces for removed lines to keep spans consistent (errors report against substituted_source, which is not preprocessed)
                    //             offset += line.len() + 1;

                    //             line = lines.next().unwrap();
                    //         }

                    parse_imports(lexer.clone(), &mut declared_imports).map_err(
                        |(err, line_offset)| {
                            ComposerErrorInner::ImportParseError(
                                err.to_owned(),
                                initial_offset + line_offset,
                            )
                        },
                    )?;
                }
                (Token::Word("define_import_path"), directive_span) => {
                    name = self
                        .get_next_word(&mut lexer, offset)
                        .ok()
                        .map(|f| f.to_string());
                }
                (Token::Word("define"), directive_span) => {
                    if allow_defines {
                        let name = self.get_next_word(&mut lexer, offset)?;
                        let val = self.get_shader_def_value_string(&mut lexer, offset);

                        let value = if let Ok(val) = val {
                            if let Ok(val) = val.as_str().parse::<u64>() {
                                ShaderDefValue::UInt(val)
                            } else if let Ok(val) = val.as_str().parse::<i64>() {
                                ShaderDefValue::Int(val)
                            } else if let Ok(val) = val.as_str().parse::<bool>() {
                                ShaderDefValue::Bool(val)
                            } else {
                                ShaderDefValue::Bool(false) // this error will get picked up when we fully preprocess the module
                            }
                        } else {
                            ShaderDefValue::Bool(true)
                        };

                        defines.insert(name.to_string(), value);
                    } else {
                        return Err(ComposerErrorInner::DefineInModule(offset));
                    }
                }

                _ => continue,
            }
        }

        //     } else {
        //         for cap in self
        //             .def_regex
        //             .captures_iter(&line)
        //             .chain(self.def_regex_delimited.captures_iter(&line))
        //         {
        //             effective_defs.insert(cap.get(1).unwrap().as_str().to_owned());
        //         }

        //         substitute_identifiers(&line, offset, &declared_imports, &mut used_imports, true)
        //             .unwrap();
        //     }

        //     offset += line.len() + 1;
        // }

        todo!();

        Ok(PreprocessorMetaData {
            name,
            imports: vec![], //used_imports.into_values().collect(),
            defines,
            effective_defs,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[rustfmt::skip]
    const WGSL_ELSE_IFDEF: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

#ifdef TEXTURE
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef SECOND_TEXTURE
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef THIRD_TEXTURE
// Third texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else
@group(1) @binding(0)
var sprite_texture: texture_2d_array<f32>;
#endif

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

    //preprocessor tests
    #[test]
    fn process_shader_def_unknown_operator() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE !! true
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        let mut processor = Preprocessor::default();

        let result_missing = processor.preprocess(
            WGSL,
            &[("TEXTURE".to_owned(), ShaderDefValue::Bool(true))].into(),
        );

        let expected: Result<Preprocessor, ComposerErrorInner> =
            Err(ComposerErrorInner::UnknownShaderDefOperator {
                pos: 124,
                operator: "!!".to_string(),
            });

        assert_eq!(format!("{result_missing:?}"), format!("{expected:?}"),);
    }
    #[test]
    fn process_shader_def_equal_int() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE == 3
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                
                     
                                    
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result_eq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Int(3))].into(),
            )
            .unwrap();
        assert_eq!(result_eq.preprocessed_source, EXPECTED_EQ);

        let result_neq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Int(7))].into(),
            )
            .unwrap();
        assert_eq!(result_neq.preprocessed_source, EXPECTED_NEQ);

        let result_missing = processor.preprocess(WGSL, &Default::default());

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        });
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor.preprocess(
            WGSL,
            &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
        );

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
            expected: "bool".to_string(),
            value: "3".to_string(),
        });

        assert_eq!(
            format!("{result_wrong_type:?}"),
            format!("{expected_err:?}")
        );
    }

    #[test]
    fn process_shader_def_equal_bool() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE == true
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                   
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                   
                     
                                    
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result_eq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
            )
            .unwrap();
        assert_eq!(result_eq.preprocessed_source, EXPECTED_EQ);

        let result_neq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into(),
            )
            .unwrap();
        assert_eq!(result_neq.preprocessed_source, EXPECTED_NEQ);
    }

    #[test]
    fn process_shader_def_not_equal_bool() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE != false
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                    
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                    
                     
                                    
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result_eq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
            )
            .unwrap();
        assert_eq!(result_eq.preprocessed_source, EXPECTED_EQ);

        let result_neq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into(),
            )
            .unwrap();
        assert_eq!(result_neq.preprocessed_source, EXPECTED_NEQ);

        let result_missing = processor.preprocess(WGSL, &[].into());
        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        });
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor.preprocess(
            WGSL,
            &[("TEXTURE".to_string(), ShaderDefValue::Int(7))].into(),
        );

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
            expected: "int".to_string(),
            value: "false".to_string(),
        });
        assert_eq!(
            format!("{result_wrong_type:?}"),
            format!("{expected_err:?}"),
        );
    }

    #[test]
    fn process_shader_def_replace() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    var a: i32 = #FIRST_VALUE;
    var b: i32 = #FIRST_VALUE * #SECOND_VALUE;
    var c: i32 = #MISSING_VALUE;
    var d: bool = #BOOL_VALUE;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_REPLACED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    var a: i32 = 5;           
    var b: i32 = 5 * 3;                       
    var c: i32 = #MISSING_VALUE;
    var d: bool = true;       
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(
                WGSL,
                &[
                    ("BOOL_VALUE".to_string(), ShaderDefValue::Bool(true)),
                    ("FIRST_VALUE".to_string(), ShaderDefValue::Int(5)),
                    ("SECOND_VALUE".to_string(), ShaderDefValue::Int(3)),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(result.preprocessed_source, EXPECTED_REPLACED);
    }

    #[test]
    fn process_shader_define_in_shader() {
        #[rustfmt::skip]
        const WGSL: &str = r"
#define NOW_DEFINED
#ifdef NOW_DEFINED
defined
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
                   
                  
defined
      
";
        let mut processor = Preprocessor::default();
        let PreprocessorMetaData {
            defines: shader_defs,
            ..
        } = processor.get_preprocessor_metadata(&WGSL, true).unwrap();
        println!("defines: {:?}", shader_defs);
        let result = processor.preprocess(&WGSL, &shader_defs).unwrap();
        assert_eq!(result.preprocessed_source, EXPECTED);
    }

    #[test]
    fn process_shader_define_in_shader_with_value() {
        #[rustfmt::skip]
        const WGSL: &str = r"
#define DEFUINT 1
#define DEFINT -1
#define DEFBOOL false
#if DEFUINT == 1
uint: #DEFUINT
#endif
#if DEFINT == -1
int: #DEFINT
#endif
#if DEFBOOL == false
bool: #DEFBOOL
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
                 
                 
                     
                
uint: 1       
      
                
int: -1     
      
                    
bool: false   
      
";
        let mut processor = Preprocessor::default();
        let PreprocessorMetaData {
            defines: shader_defs,
            ..
        } = processor.get_preprocessor_metadata(&WGSL, true).unwrap();
        println!("defines: {:?}", shader_defs);
        let result = processor.preprocess(&WGSL, &shader_defs).unwrap();
        assert_eq!(result.preprocessed_source, EXPECTED);
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_else() {
        #[rustfmt::skip]
        const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
@group(1) @binding(0)
var sprite_texture: texture_2d_array<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor.preprocess(&WGSL_ELSE_IFDEF, &[].into()).unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_no_match_and_no_fallback_else() {
        #[rustfmt::skip]
        const WGSL_ELSE_IFDEF_NO_ELSE_FALLBACK: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

#ifdef TEXTURE
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef OTHER_TEXTURE
// Other texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(&WGSL_ELSE_IFDEF_NO_ELSE_FALLBACK, &[].into())
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_first_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
              
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
                          
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_second_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[("SECOND_TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_third_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Third texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[("THIRD_TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_only_accepts_one_valid_else_ifdef() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[
                    ("SECOND_TEXTURE".to_string(), ShaderDefValue::Bool(true)),
                    ("THIRD_TEXTURE".to_string(), ShaderDefValue::Bool(true)),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_complicated_nesting() {
        // Test some nesting including #else ifdef statements
        // 1. Enter an #else ifdef
        // 2. Then enter an #else
        // 3. Then enter another #else ifdef

        #[rustfmt::skip]
        const WGSL_COMPLICATED_ELSE_IFDEF: &str = r"
#ifdef NOT_DEFINED
// not defined
#else ifdef IS_DEFINED
// defined 1
#ifdef NOT_DEFINED
// not defined
#else
// should be here
#ifdef NOT_DEFINED
// not defined
#else ifdef ALSO_NOT_DEFINED
// not defined
#else ifdef IS_DEFINED
// defined 2
#endif
#endif
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
// defined 1
// should be here
// defined 2
";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_COMPLICATED_ELSE_IFDEF,
                &[("IS_DEFINED".to_string(), ShaderDefValue::Bool(true))].into(),
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifndef() {
        #[rustfmt::skip]
        const INPUT: &str = r"
#ifdef NOT_DEFINED
fail 1
#else ifdef ALSO_NOT_DEFINED
fail 2
#else ifndef ALSO_ALSO_NOT_DEFINED
ok
#else
fail 3
#endif
";

        const EXPECTED: &str = r"ok";
        let mut processor = Preprocessor::default();
        let result = processor.preprocess(&INPUT, &[].into()).unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_if() {
        #[rustfmt::skip]
        const INPUT: &str = r"
#ifdef NOT_DEFINED
fail 1
#else if x == 1
fail 2
#else if x == 2
ok
#else
fail 3
#endif
";

        const EXPECTED: &str = r"ok";
        let mut processor = Preprocessor::default();
        let result = processor
            .preprocess(&INPUT, &[("x".to_owned(), ShaderDefValue::Int(2))].into())
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }
}
