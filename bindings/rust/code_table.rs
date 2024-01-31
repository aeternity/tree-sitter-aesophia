use crate::cst;

pub type TreeTableRef = cst::NodeId;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TreeTable<T> {
    pub data: Vec<Option<T>>
}

impl<T: Clone> TreeTable<T> {
    pub fn new(size: usize) -> TreeTable<T> {
        TreeTable {
            data: vec![None; size]
        }
    }

    pub fn set(&mut self, node: TreeTableRef, v: T) {
        self.data.insert(node, Some(v))
    }

    pub fn get(&self, node: TreeTableRef) -> Option<T> {
        self.data[node].clone() // FIXME unnecessary clone?
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct CodeTableRef {
    file_id: cst::FileId,
    node_id: TreeTableRef,
}

impl CodeTableRef {
    pub fn new(file_id: cst::FileId, node_id: TreeTableRef) -> Self {
        CodeTableRef{file_id, node_id}
    }
}

#[derive(Debug)]
pub struct CodeTable<T> {
    pub tables: Vec<TreeTable<T>>,
    pub filenames: Vec<String>,
}

impl<T: Clone> CodeTable<T> {
    pub fn new(tree_infos: Vec<(String, usize)>) -> CodeTable<T> {
        let mut tables = vec![];
        let mut filenames = vec![];
        for (name, size) in tree_infos {
            tables.push(TreeTable::new(size));
            filenames.push(name);
        }
        CodeTable {
            tables,
            filenames,
        }
    }

    pub fn set(&mut self, idx: CodeTableRef, v: T) {
        self.tables[idx.file_id].set(idx.node_id, v)
    }

    pub fn get(&self, idx: CodeTableRef) -> Option<T> {
        self.tables[idx.file_id].get(idx.node_id)
    }
}
