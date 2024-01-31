pub const BUILTIN_FILENAME: &'static str = "__BUILTIN__";

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LocInfo {
    pub start_line: u32,
    pub start_col: u32,
    pub start_byte: u32,
    pub end_line: u32,
    pub end_col: u32,
    pub end_byte: u32,
    pub filename: String,
}

impl LocInfo {
    pub fn builtin() -> Self {
        Self {
            start_line: 0,
            start_col: 0,
            start_byte: 0,
            end_line: 0,
            end_col: 0,
            end_byte: 0,
            filename: BUILTIN_FILENAME.to_string(),
        }
    }
}

impl<'tree> From<&tree_sitter::Node<'tree>> for LocInfo {
    fn from(node: &tree_sitter::Node<'tree>) -> LocInfo {
        use tree_sitter::Point;
        let Point{row: start_line, column: start_col} = node.start_position();
        let Point{row: end_line, column: end_col} = node.end_position();
        LocInfo {
            start_line: start_line as u32,
            start_col: start_col as u32,
            start_byte: node.start_byte() as u32,
            end_line: end_line as u32,
            end_col: end_col as u32,
            end_byte: node.end_byte() as u32,
            filename: "<filename mock>".to_string(),
        }
    }
}
