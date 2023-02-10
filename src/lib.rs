use std::{cell::RefCell, rc::Rc};

pub mod backend;
pub mod define;
pub mod diagnostic;
pub mod front;

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn ptr<T>(v: T) -> Ptr<T> {
  Rc::new(RefCell::new(v))
}

#[derive(Debug, Clone)]
pub struct Counter {
  count: u64,
}

impl Counter {
  pub fn new() -> Self {
    Self { count: 0 }
  }

  pub fn next(&mut self) -> u64 {
    self.count += 1;
    return self.count;
  }
}
