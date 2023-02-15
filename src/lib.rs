use gc::{Gc, GcCell, Trace};

pub mod backend;
pub mod define;
pub mod diagnostic;
pub mod front;

pub type Ptr<T> = Gc<GcCell<T>>;

pub fn ptr<T>(v: T) -> Ptr<T>
where
  T: Trace,
{
  Gc::new(GcCell::new(v))
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
