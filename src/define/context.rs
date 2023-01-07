use std::collections::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

use super::ast::*;
use super::value::Value;

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Trait,
    Data,
    Record,
    LocalVar,
    Parameter,
    GlobalVar,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Value,
    pub kind: SymbolKind,
    pub chained: Chained,
}

pub type SymbolTablePtr = Rc<RefCell<SymbolTable>>;

#[derive(Debug)]
pub struct SymbolTable {
    pub table: HashMap<String, Symbol>,
    pub parent: Option<SymbolTablePtr>,
}

impl SymbolTable {
    pub fn empty() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
            parent: None,
        }
    }

    pub fn new(parent: SymbolTablePtr) -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn set_parent(&mut self, parent: SymbolTablePtr) {
        self.parent = Some(parent);
    }

    pub fn register(&mut self, name: String, ty: Value, kind: SymbolKind, chained: Chained) {
        self.table.insert(name, Symbol { ty, kind, chained });
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        match self.table.get(name) {
            Some(symbol) => Some(symbol.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().lookup(name),
                None => None,
            },
        }
    }
}

pub fn mk_empty_symtable() -> SymbolTablePtr {
    Rc::new(RefCell::new(SymbolTable::empty()))
}
