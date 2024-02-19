use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::sync::atomic::AtomicU64;

pub use lintree_derive::TreeNode;

pub unsafe trait RawNode {
    fn size(&self) -> usize;
}

static mut TREE_ID_DISCRIMINATOR: AtomicU64 = AtomicU64::new(0);

pub struct Tree<T: RawNode> {
    buffer: *mut u8,
    cap: usize,
    used: usize,
    discriminator: u32,
    _phantom: PhantomData<T>,
}

pub struct TreeAlloc<'a, T: RawNode> {
    tree: &'a mut Tree<T>,
}

pub struct Tr<T> {
    offset: usize,
    discriminator: u32,
    _phantom: PhantomData<T>,
}

pub struct TrView {
    offset: usize,
}

impl<T: RawNode> Clone for Tr<T> {
    fn clone(&self) -> Self {
        Self {
            offset: self.offset,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }
}

impl<T: RawNode> Copy for Tr<T> {}

impl<T: RawNode> Tree<T> {
    const MAX_CAPACITY: usize = isize::MAX as usize;
    const DEFAULT_CAPACITY: usize = 256;

    pub fn with_capacity(cap: usize) -> Self {
        assert!(cap > 0, "cap must be greater than 0");
        assert!(
            cap <= Self::MAX_CAPACITY,
            "cap must be less than isize::MAX"
        );

        let layout = std::alloc::Layout::from_size_align(cap, std::mem::align_of::<T>()).unwrap();

        let buffer = unsafe { std::alloc::alloc(layout) };
        if buffer.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        let discriminator =
            unsafe { TREE_ID_DISCRIMINATOR.fetch_add(1, std::sync::atomic::Ordering::Relaxed) };

        if discriminator > u32::MAX as u64 {
            panic!("Discriminator overflow");
        }

        let discriminator = discriminator as u32;

        Self {
            buffer,
            cap,
            used: 0,
            discriminator,
            _phantom: PhantomData,
        }
    }

    pub fn new() -> Self {
        Self::with_capacity(Self::DEFAULT_CAPACITY)
    }

    pub fn realloc(&mut self, new_cap: usize) {
        assert!(new_cap > 0, "new_cap must be greater than 0");
        assert!(
            new_cap <= Self::MAX_CAPACITY,
            "new_cap must be less than isize::MAX"
        );
        let layout =
            std::alloc::Layout::from_size_align(new_cap, std::mem::align_of::<T>()).unwrap();

        let new_buffer = unsafe { std::alloc::realloc(self.buffer, layout, new_cap) };
        if new_buffer.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        self.buffer = new_buffer;
        self.cap = new_cap;
    }

    unsafe fn get_unchecked(&self, offset: usize) -> &T {
        &*(self.buffer.add(offset) as *const T)
    }

    unsafe fn get_mut_unchecked(&mut self, offset: usize) -> &mut T {
        &mut *(self.buffer.add(offset) as *mut T)
    }

    pub fn reserve(&mut self, additional: usize) {
        let new_cap = self.cap.checked_add(additional).expect("Capacity overflow");
        if new_cap > self.cap {
            // self.cap * 2 can't overflow because self.cap <= isize::MAX
            let new_cap = std::cmp::max(new_cap, self.cap * 2);
            self.realloc(new_cap);
        }
    }

    pub fn alloc_with(&mut self, f: impl FnOnce(TreeAlloc<T>) -> T) -> Tr<T> {
        let size = std::mem::size_of::<T>();
        if self.used + size > self.cap {
            let new_cap = self.cap * 2;
            self.realloc(new_cap);
        }

        let offset = self.used;
        self.used += size;
        let alloc = TreeAlloc { tree: self };
        let node = f(alloc);
        unsafe {
            std::ptr::write(self.buffer.add(offset) as *mut T, node);
        }

        Tr {
            offset,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }

    pub fn alloc(&mut self, node: T) -> Tr<T> {
        self.alloc_with(move |_| node)
    }

    fn ensure_discriminator(&self, node: Tr<T>) {
        if node.discriminator != self.discriminator {
            panic!(
                "Accessing node from different tree: expected {}, got {}",
                self.discriminator, node.discriminator
            );
        }
    }

    pub fn get(&self, node: Tr<T>) -> &T {
        self.ensure_discriminator(node);
        unsafe { self.get_unchecked(node.offset) }
    }

    pub fn get_mut(&mut self, node: Tr<T>) -> &mut T {
        self.ensure_discriminator(node);
        unsafe { self.get_mut_unchecked(node.offset) }
    }
}

impl<T: RawNode> Index<Tr<T>> for Tree<T> {
    type Output = T;

    fn index(&self, index: Tr<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T: RawNode> IndexMut<Tr<T>> for Tree<T> {
    fn index_mut(&mut self, index: Tr<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T: RawNode> Drop for Tree<T> {
    fn drop(&mut self) {
        if std::mem::needs_drop::<T>() {
            let mut offset: usize = 0;

            while offset < self.used {
                let node = unsafe { self.get_mut_unchecked(offset) };
                let size = node.size();
                unsafe {
                    std::ptr::drop_in_place(node as *mut T);
                }
                offset += size;
            }
        }

        let layout =
            std::alloc::Layout::from_size_align(self.cap, std::mem::align_of::<T>()).unwrap();

        unsafe {
            std::alloc::dealloc(self.buffer, layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    struct TestNode<'a> {
        drop_info: &'a RefCell<Vec<usize>>,
        a: u32,
        id: usize,
    }

    unsafe impl<'a> RawNode for TestNode<'a> {
        fn size(&self) -> usize {
            std::mem::size_of::<Self>()
        }
    }

    impl<'a> Drop for TestNode<'a> {
        fn drop(&mut self) {
            self.drop_info.borrow_mut()[self.id] += 1;
        }
    }

    #[test]
    fn test_alloc() {
        let drop_info = RefCell::new(vec![0]);
        let mut tree = Tree::<TestNode>::new();
        let node = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 42,
            id: 0,
        });

        assert_eq!(tree[node].a, 42);
        drop(tree);
        assert!(drop_info.into_inner().into_iter().all(|x| x == 1));
    }

    #[test]
    fn test_alloc_multiple() {
        let drop_info = RefCell::new(vec![0, 0, 0]);
        let mut tree = Tree::<TestNode>::new();
        let node1 = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 42,
            id: 0,
        });
        let node2 = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 43,
            id: 1,
        });
        let node3 = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 44,
            id: 2,
        });

        assert_eq!(tree[node1].a, 42);
        assert_eq!(tree[node2].a, 43);
        assert_eq!(tree[node3].a, 44);
        drop(tree);
        assert!(drop_info.into_inner().into_iter().all(|x| x == 1));
    }

    #[test]
    fn test_mutation() {
        let drop_info = RefCell::new(vec![0, 0, 0]);
        let mut tree = Tree::<TestNode>::new();
        let node1 = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 42,
            id: 0,
        });
        let node2 = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 43,
            id: 1,
        });
        let node3 = tree.alloc(TestNode {
            drop_info: &drop_info,
            a: 44,
            id: 2,
        });

        assert_eq!(tree[node1].a, 42);
        assert_eq!(tree[node2].a, 43);
        assert_eq!(tree[node3].a, 44);

        tree[node1].a = 100;
        tree[node2].a = 101;
        tree[node3].a = 102;

        assert_eq!(tree[node1].a, 100);
        assert_eq!(tree[node2].a, 101);
        assert_eq!(tree[node3].a, 102);
        drop(tree);
        assert!(drop_info.into_inner().into_iter().all(|x| x == 1));
    }

    #[test]
    #[should_panic]
    fn access_different_tree() {
        let drop_info = RefCell::new(vec![0]);
        let mut tree1 = Tree::<TestNode>::new();
        let tree2 = Tree::<TestNode>::new();
        let node = tree1.alloc(TestNode {
            drop_info: &drop_info,
            a: 42,
            id: 0,
        });

        let _ = tree2[node].a;
    }
}

