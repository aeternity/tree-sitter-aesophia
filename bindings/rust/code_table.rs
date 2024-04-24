use crate::cst;

use std::collections::HashMap;

pub type TreeTableRef = cst::NodeId;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TreeTable<T> {
    pub data: HashMap<TreeTableRef, T>,
}

impl<T: Clone> TreeTable<T> {
    pub fn new() -> TreeTable<T> {
        TreeTable {
            data: HashMap::new(),
        }
    }

    pub fn set(&mut self, node: TreeTableRef, v: T) {
        self.data.insert(node, v);
    }

    pub fn get(&self, node: TreeTableRef) -> Option<&T> {
        self.data.get(&node) // FIXME unnecessary clone?
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct CodeTableRef {
    file_id: cst::FileId,
    node_id: TreeTableRef,
}

impl CodeTableRef {
    pub fn new(file_id: cst::FileId, node_id: TreeTableRef) -> Self {
        CodeTableRef { file_id, node_id }
    }

    pub fn file_id(&self) -> cst::FileId {
        self.file_id
    }

    pub fn node_id(&self) -> TreeTableRef {
        self.node_id
    }
}

impl std::fmt::Display for CodeTableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.file_id, self.node_id)
    }
}

pub trait HasCodeRef {
    fn code_ref(&self) -> CodeTableRef;
}

#[derive(Debug)]
pub struct CodeTable<T> {
    pub tables: Vec<TreeTable<T>>,
    pub filenames: Vec<String>,
}

impl<T: Clone> CodeTable<T> {
    pub fn new(tree_infos: Vec<String>) -> CodeTable<T> {
        let mut tables = vec![];
        let mut filenames = vec![];
        for name in tree_infos {
            tables.push(TreeTable::new());
            filenames.push(name);
        }
        CodeTable { tables, filenames }
    }

    pub fn set(&mut self, idx: CodeTableRef, v: T) {
        self.tables[idx.file_id].set(idx.node_id, v)
    }

    pub fn get(&self, idx: CodeTableRef) -> Option<&T> {
        self.tables[idx.file_id].get(idx.node_id)
    }
}
