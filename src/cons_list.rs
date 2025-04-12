use crate::parse::Rule;

// A (potentially) heterogenous list interface
pub trait List {
    /// get the length of the list
    fn len(&self) -> usize;

    /// map a callback over the list, returning the result as a (homogenous) vec
    fn map<F, V>(&self, callback: F) -> Vec<V>
    where
        F: Fn(&dyn Rule) -> V;

    /// run a callback on each item in the list, exiting early if the callback returns an error
    fn for_each<F, E>(&self, callback: F) -> Result<(), E>
    where
        F: FnMut(&dyn Rule) -> Result<(), E>;
}

/// An empty struct to represent the end of a heterogenous list
#[derive(Clone)]
pub struct Tail {}

pub fn tail() -> Tail {
    Tail {}
}

impl List for Tail {
    fn len(&self) -> usize {
        0
    }

    fn map<F, V>(&self, _callback: F) -> Vec<V>
    where
        F: Fn(&dyn Rule) -> V,
    {
        vec![]
    }

    fn for_each<F, E>(&self, _callback: F) -> Result<(), E>
    where
        F: FnMut(&dyn Rule) -> Result<(), E>,
    {
        Ok(())
    }
}

/// A Lisp-style cons cell that holds one list item, and the rest of the list
/// Each cons cell may contain a different type of object, allowing heterogenous lists
#[derive(Clone)]
pub struct Cons<H, T>
where
    H: Rule + Clone,
    T: List,
{
    pub head: H,
    pub tail: T,
}

pub fn cons<H, T>(head: H, tail: T) -> Cons<H, T>
where
    H: Rule + Clone,
    T: List,
{
    Cons { head, tail }
}

impl<H, T> List for Cons<H, T>
where
    H: Rule + Clone,
    T: List,
{
    fn len(&self) -> usize {
        1 + self.tail.len()
    }

    fn map<F, V>(&self, callback: F) -> Vec<V>
    where
        F: Fn(&dyn Rule) -> V,
    {
        let mut v = vec![(callback)(&self.head)];
        v.append(&mut self.tail.map(callback));
        v
    }

    fn for_each<F, E>(&self, mut callback: F) -> Result<(), E>
    where
        F: FnMut(&dyn Rule) -> Result<(), E>,
    {
        let r = (callback)(&self.head);
        match r {
            Ok(_) => self.tail.for_each(callback),
            e => e,
        }
    }
}

impl<H, T> Cons<H, T>
where
    H: Rule + Clone,
    T: List,
{
}

// Also implement the List interface for Vec<T>, so
// that we can construct homogenous lists at runtime
impl<T> List for Vec<T>
where
    T: Rule,
{
    fn len(&self) -> usize {
        self.len()
    }

    fn map<F, V>(&self, callback: F) -> Vec<V>
    where
        F: Fn(&dyn Rule) -> V,
    {
        self.iter().map(|x| callback(x)).collect()
    }

    fn for_each<F, E>(&self, mut callback: F) -> Result<(), E>
    where
        F: FnMut(&dyn Rule) -> Result<(), E>,
    {
        for x in self {
            callback(x)?;
        }

        Ok(())
    }
}

/// Convenience macro to chain together cons cells to build a heterogenous list
#[macro_export]
macro_rules! list {
    () => {
        $crate::cons_list::Tail {}
    };
    ($a:expr) => {
        $crate::list![$a,]
    };
    ($a:expr, $($tok:tt)*) => {
        $crate::cons_list::Cons {
            head: $a,
            tail: $crate::list![$($tok)*],
        }
    }
}
