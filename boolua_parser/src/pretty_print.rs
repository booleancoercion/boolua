use std::fmt::Debug;

impl Debug for super::Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Name({:?})", self.0)
    }
}

impl Debug for super::LuaStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = String::from_utf8_lossy(&self.0);
        write!(f, "{:?}", &*string)
    }
}
