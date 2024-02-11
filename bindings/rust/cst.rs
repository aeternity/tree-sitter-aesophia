pub mod loc_info;

/// Alias for tree-sitter node
pub type TsNode<'a> = tree_sitter::Node<'a>;

/// Alias for tree-sitter coursor
pub type TsCursor<'a> = tree_sitter::TreeCursor<'a>;

pub type NodeId = usize;

pub type FileId = usize;

pub trait HasNodeId {
    fn node_id(&self) -> NodeId;
}


/// Information about CST node subtyping. Maps subtype to its
/// supertype. This says for example that `expr_if` is a special case
/// of `expr`.

pub fn load_lang() -> tree_sitter::Parser {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(crate::tree_sitter_aesophia::language())
        .expect("Error loading aesophia language");

    parser
}

pub fn parse_str(src: &str) -> Option<tree_sitter::Tree> {
    println!("PARSING CST:\n================================================================================\n{}================================================================================\n", src);
    let mut parser = load_lang();
    let tree = parser.parse(src, None);
    tree
}


pub fn parse_file(filename: &str) -> (String, Option<tree_sitter::Tree>) {
    match std::fs::read_to_string(filename) {
        Err(e) => panic!("Failed to load file: {}", e),
        Ok(src) => {
            let res = parse_str(&src);
            (src, res)
        }
    }
}
