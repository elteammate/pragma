use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub trait IIndex {
    fn index(&self) -> usize;
    fn from_index(index: usize) -> Self;
    fn string_name() -> &'static str;
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct IVec<I: IIndex, T>(Vec<T>, PhantomData<I>);

pub struct ISource<I: IIndex>(usize, PhantomData<I>);

#[macro_export]
macro_rules! create_index {
    ($name:ident) => {
        create_index!(@module $name);
        paste!(use [<__id_ $name:snake>]::$name;);
    };

    (pub $name:ident) => {
        create_index!(@module $name);
        paste!(pub use [<__id_ $name:snake>]::$name;);
    };

    (@module $name:ident) => {
        paste! {
            mod [<__id_ $name:snake>] {
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                pub struct $name(usize);

                impl $crate::ivec::IIndex for $name {
                    fn index(&self) -> usize {
                        self.0
                    }

                    fn from_index(index: usize) -> Self {
                        Self(index)
                    }

                    fn string_name() -> &'static str {
                        stringify!($name)
                    }
                }
            }
        }
    };
}

#[macro_export]
macro_rules! ivec {
    [$($tt:tt)*] => {
        $crate::ivec::IVec::from(vec![$($tt)*])
    }
}

impl<T, I: IIndex> IVec<I, T> {
    pub fn new() -> Self {
        Self(Vec::new(), PhantomData)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push(&mut self, value: T) -> I {
        let index = self.0.len();
        self.0.push(value);
        I::from_index(index)
    }

    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }

    pub fn for_each(&self, mut f: impl FnMut(I, &T)) {
        self.indexed_iter().for_each(|(index, value)| f(index, value));
    }

    pub fn iter(&self) -> <&IVec<I, T> as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn indexed_iter(&self) -> impl Iterator<Item=(I, &T)> {
        self.0.iter().enumerate().map(|(index, value)| (I::from_index(index), value))
    }

    pub fn iter_mut(&mut self) -> <&mut IVec<I, T> as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn indexed_iter_mut(&mut self) -> impl Iterator<Item=(I, &mut T)> {
        self.0.iter_mut().enumerate().map(|(index, value)| (I::from_index(index), value))
    }
    
    pub fn indices(&self) -> ISource<I> {
        ISource(self.0.len(), PhantomData)
    }
}

impl<T, I: IIndex> Default for IVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, I: IIndex> Index<I> for IVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index.index()]
    }
}

impl<T, I: IIndex> IndexMut<I> for IVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index.index()]
    }
}

impl<T, I: IIndex> IntoIterator for IVec<I, T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T, I: IIndex> IntoIterator for &'a IVec<I, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, T, I: IIndex> IntoIterator for &'a mut IVec<I, T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<T, I: IIndex> FromIterator<T> for IVec<I, T> {
    fn from_iter<U: IntoIterator<Item=T>>(iter: U) -> Self {
        Self(iter.into_iter().collect(), PhantomData)
    }
}

impl<T, I: IIndex> From<Vec<T>> for IVec<I, T> {
    fn from(vec: Vec<T>) -> Self {
        Self(vec, PhantomData)
    }
}

impl<T: Debug, I: IIndex> Debug for IVec<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IVec<{}>", I::string_name())?;
        self.0.fmt(f)
    }
}

impl<I: IIndex> ISource<I> {
    pub fn new() -> Self {
        Self(0, PhantomData)
    }

    pub fn next(&mut self) -> I {
        let index = self.0;
        self.0 += 1;
        I::from_index(index)
    }

    pub fn iter(&self) -> ISourceIter<I> {
        self.into_iter()
    }
    
    pub fn len(&self) -> usize {
        self.0
    }
}

impl<I: IIndex> Default for ISource<I> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ISourceIter<I: IIndex> {
    current: usize,
    count: usize,
    _phantom: PhantomData<I>,
}

impl<I: IIndex> IntoIterator for ISource<I> {
    type Item = I;
    type IntoIter = ISourceIter<I>;

    fn into_iter(self) -> Self::IntoIter {
        ISourceIter {
            current: 0,
            count: self.0,
            _phantom: PhantomData,
        }
    }
}

impl<'a, I: IIndex> IntoIterator for &'a ISource<I> {
    type Item = I;
    type IntoIter = ISourceIter<I>;

    fn into_iter(self) -> Self::IntoIter {
        ISourceIter {
            current: 0,
            count: self.0,
            _phantom: PhantomData,
        }
    }
}

impl<I: IIndex> Debug for ISource<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ISource<{}>({})", I::string_name(), self.0)
    }
}

impl<I: IIndex> Iterator for ISourceIter<I> {
    type Item = I;

    fn next(&mut self) -> Option<I> {
        if self.current == self.count {
            None
        } else {
            let index = self.current;
            self.current += 1;
            Some(I::from_index(index))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.count - self.current;
        (remaining, Some(remaining))
    }
}
