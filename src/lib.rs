use std::{cell::RefCell, rc::Rc};

pub mod backend;
pub mod define;
pub mod front;

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn ptr<T>(v: T) -> Ptr<T> {
    Rc::new(RefCell::new(v))
}
