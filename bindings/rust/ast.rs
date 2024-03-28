//! Concrete Syntax Tree analysis and conversion to Abstract Syntax Tree.

pub mod ast;
pub mod cst_ast;
pub mod cst_parse;
pub mod display;

use crate::cst;

pub fn load_subtypes() -> cst_parse::SubtypeMap {
    print!("Loading subtype map... ");

    let nodes_json_src =  std::fs::read_to_string("src/node-types.json").expect("Unable to read subtype file");
    let nodes_json: serde_json::Value = serde_json::from_str(&nodes_json_src).unwrap();
    let mut subtypes = cst_parse::SubtypeMap::new();
    match nodes_json {
            serde_json::Value::Array(arr) =>
            for v in arr {
                match (&v["subtypes"], &v["type"]) {
                    (serde_json::Value::Array(subts),
                     serde_json::Value::String(t)
                    )=> {
                        for subt in subts {
                            match &subt["type"] {
                                serde_json::Value::String(s) => {
                                    subtypes.insert(s.clone(), t.clone());
                                },
                                _ => panic!("wtf sub type")
                            }
                        }
                    },
                    _ => ()
                };
            },
        _ => panic!("wtf json")
    }

    println!("Done");
    subtypes
}

pub fn parse_str<T: cst_ast::CstNode>(src: &str) -> cst_parse::ParseResultN<T> {
    let dispatch = match <T as cst_ast::CstNode>::ts_dispatch() {
        None => String::new(),
        Some(disp) => "@ts.parse(".to_string() + &disp + ")\n"
    };

    let src = dispatch.clone() + src;

    let tree = cst::parse_str(&src).expect("CST parse error");
    let mut tc = tree.walk();

    println!("Parsed {}", tree.root_node().to_sexp());

    if !dispatch.is_empty() {
        tc.reset(tc.node().child_by_field_name("content").expect("No source"))
    }

    let src_data: Vec<u16> = src.encode_utf16().collect();
    let subtypes = load_subtypes();
    let mut env = cst_parse::ParseEnv::new(src_data, subtypes, tree.root_node().has_error());

    <T as cst_ast::CstNode>::parse(&mut tc, &mut env)
}
