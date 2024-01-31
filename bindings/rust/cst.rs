/// Alias for tree-sitter node
pub type TsNode<'a> = tree_sitter::Node<'a>;

/// Alias for tree-sitter coursor
pub type TsCursor<'a> = tree_sitter::TreeCursor<'a>;

pub type NodeId = usize;

pub type FileId = usize;

pub trait HasNodeId {
    fn node_id(&self) -> NodeId;
}
