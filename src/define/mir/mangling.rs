use super::*;

pub trait NameMangling {
  fn mangle(&self) -> String;
}

impl NameMangling for Fn {
  fn mangle(&self) -> String {
    let mut mangled_name = String::from("__O");
    for namespace in &self.namespaces {
      mangled_name.push_str(namespace.len().to_string().as_str());
      mangled_name.push_str(namespace.as_str());
    }
    mangled_name.push_str(self.name.len().to_string().as_str());
    mangled_name.push_str(self.name.as_str());
    return mangled_name;
  }
}

impl NameMangling for Module {
  fn mangle(&self) -> String {
    let mut mangled_name = String::from("__O");
    for namespace in &self.namespaces {
      mangled_name.push_str(namespace.len().to_string().as_str());
      mangled_name.push_str(namespace.as_str());
    }
    mangled_name.push_str(self.name.len().to_string().as_str());
    mangled_name.push_str(self.name.as_str());
    return mangled_name;
  }
}
