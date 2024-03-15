use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::sync::atomic::AtomicU64;

pub use lintree_derive::tree_node;

pub trait TreeNode: Sized {
    type Container: RawNode;
    type View<'t, R: TreeNode>: NodeView<'t, R, Node=Self>
        where Self: 't, R: 't, Self::Container: 't;
    type ViewMut<'t, R: TreeNode>: NodeViewMut<'t, R, Node=Self>
        where Self: 't, R: 't, Self::Container: 't;

    fn into_container(self, tree: TreeDiscriminator) -> Self::Container;
}

pub unsafe trait RawNode {
    fn extra_size(&self) -> usize;
}

pub trait NodeView<'t, R: TreeNode> {
    type Node: TreeNode;

    fn from_tr(raw_view: RawTreeView<'t, R>, tr: Tr<Self::Node>) -> Self;
}

pub trait NodeViewMut<'t, R: TreeNode> {
    type Node: TreeNode;

    fn from_tr(raw_view: RawTreeViewMut<'t, R>, tr: Tr<Self::Node>) -> Self;
}

static mut TREE_ID_DISCRIMINATOR: AtomicU64 = AtomicU64::new(0);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct TreeDiscriminator(u32);

pub struct Tree<T: TreeNode> {
    buffer: *mut u8,
    cap: usize,
    used: usize,
    discriminator: TreeDiscriminator,
    _phantom: PhantomData<T::Container>,
}

#[derive(Clone)]
pub struct RawTreeView<'t, T: TreeNode> {
    buffer: *mut u8,
    discriminator: TreeDiscriminator,
    _phantom: PhantomData<&'t Tree<T>>,
}

pub struct RawTreeViewMut<'t, T: TreeNode> {
    buffer: *mut u8,
    discriminator: TreeDiscriminator,
    _phantom: PhantomData<&'t Tree<T>>,
}

pub struct TreeAlloc<'t, T: TreeNode> {
    tree: &'t mut Tree<T>,
}

pub struct Tr<T: TreeNode> {
    offset: usize,
    discriminator: TreeDiscriminator,
    _phantom: PhantomData<T::Container>,
}

pub struct TrContainer {
    offset: usize,
}

pub struct TrView<'t, R: TreeNode, T: TreeNode> {
    raw: RawTreeView<'t, R>,
    data: &'t <Tr<T> as TreeNode>::Container,
}

pub struct TrViewMut<'t, R: TreeNode, T: TreeNode> {
    raw: RawTreeViewMut<'t, R>,
    data: &'t mut <Tr<T> as TreeNode>::Container,
}

impl<T: TreeNode> Clone for Tr<T> {
    fn clone(&self) -> Self {
        Self {
            offset: self.offset,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }
}

impl<T: TreeNode> Copy for Tr<T> {}

impl<T: TreeNode> TreeNode for Tr<T> {
    type Container = TrContainer;
    type View<'t, R: TreeNode> = TrView<'t, R, T>
        where Self: 't, R: 't, Self::Container: 't;
    type ViewMut<'t, R: TreeNode> = TrViewMut<'t, R, T>
        where Self: 't, R: 't, Self::Container: 't;

    fn into_container(self, discriminator: TreeDiscriminator) -> Self::Container {
        assert_eq!(self.discriminator, discriminator, "Storing a node from a different tree");
        TrContainer {
            offset: self.offset,
        }
    }
}

impl<'t, R: TreeNode, T: TreeNode> NodeView<'t, R> for TrView<'t, R, T> {
    type Node = Tr<T>;

    fn from_tr(raw_tree: RawTreeView<'t, R>, tr: Tr<Self::Node>) -> Self {
        let data = raw_tree.get(tr);
        Self {
            raw: raw_tree,
            data
        }
    }
}

impl<'t, R: TreeNode, T: TreeNode> NodeViewMut<'t, R> for TrViewMut<'t, R, T> {
    type Node = Tr<T>;

    fn from_tr(raw_tree: RawTreeViewMut<'t, R>, tr: Tr<Self::Node>) -> Self {
        let data = raw_tree.get_mut(tr);
        Self {
            raw: raw_tree,
            data
        }
    }
}

impl<T: TreeNode> Tr<T> {
    pub unsafe fn from_container(container: TrContainer, discriminator: TreeDiscriminator) -> Self {
        Self {
            offset: container.offset,
            discriminator,
            _phantom: PhantomData,
        }
    }
}

unsafe impl RawNode for TrContainer {
    fn extra_size(&self) -> usize {
        0
    }
}

impl<T: TreeNode> Tree<T> {
    const MAX_CAPACITY: usize = isize::MAX as usize;
    const DEFAULT_CAPACITY: usize = 256;

    pub fn with_capacity(cap: usize) -> Self {
        assert!(cap > 0, "cap must be greater than 0");
        assert!(
            cap <= Self::MAX_CAPACITY,
            "cap must be less than isize::MAX"
        );

        let layout = std::alloc::Layout::from_size_align(cap, 
            std::mem::align_of::<T::Container>()).unwrap();

        let buffer = unsafe { std::alloc::alloc(layout) };
        if buffer.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        let discriminator =
            unsafe { TREE_ID_DISCRIMINATOR.fetch_add(1, std::sync::atomic::Ordering::Relaxed) };

        if discriminator > u32::MAX as u64 {
            panic!("Discriminator overflow");
        }

        let discriminator = TreeDiscriminator(discriminator as u32);

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
            std::alloc::Layout::from_size_align(new_cap, 
            std::mem::align_of::<T::Container>())
                .unwrap();

        let new_buffer = unsafe { std::alloc::realloc(self.buffer, layout, new_cap) };
        if new_buffer.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        self.buffer = new_buffer;
        self.cap = new_cap;
    }

    unsafe fn get_unchecked(&self, offset: usize) -> &T::Container {
        &*(self.buffer.add(offset) as *const T::Container)
    }

    unsafe fn get_mut_unchecked(&mut self, offset: usize) -> &mut T::Container {
        &mut *(self.buffer.add(offset) as *mut T::Container)
    }

    pub fn reserve(&mut self, additional: usize) {
        let new_cap = self.cap.checked_add(additional).expect("Capacity overflow");
        if new_cap > self.cap {
            // self.cap * 2 can't overflow because self.cap <= isize::MAX
            let new_cap = std::cmp::max(new_cap, self.cap * 2);
            self.realloc(new_cap);
        }
    }

    pub fn alloc_with(&mut self, f: impl FnOnce(TreeAlloc<T>) -> T::Container) -> Tr<T> {
        let size = std::mem::size_of::<T::Container>();
        self.reserve(size);

        let offset = self.used;
        self.used += size;
        let alloc = TreeAlloc { tree: self };
        let node = f(alloc);
        unsafe {
            std::ptr::write(self.buffer.add(offset) as *mut T::Container, node);
        }

        Tr {
            offset,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }

    pub fn alloc(&mut self, node: T::Container) -> Tr<T> {
        self.alloc_with(|_| node)
    }
    
    pub fn push(&mut self, node: T) -> Tr<T> {
        self.alloc(node.into_container(self.discriminator))
    }

    fn ensure_discriminator(&self, node: Tr<T>) {
        if node.discriminator != self.discriminator {
            panic!(
                "Accessing node from different tree: expected {:?}, got {:?}",
                self.discriminator, node.discriminator
            );
        }
    }

    pub fn get(&self, node: Tr<T>) -> &T::Container {
        self.ensure_discriminator(node);
        unsafe { self.get_unchecked(node.offset) }
    }

    pub fn get_mut(&mut self, node: Tr<T>) -> &mut T::Container {
        self.ensure_discriminator(node);
        unsafe { self.get_mut_unchecked(node.offset) }
    }
    
    pub fn view<'t>(&'t self, node: Tr<T>) -> T::View<'t, T> {
        T::View::from_tr(self.get_raw_view(), node)
    }

    pub fn view_mut<'t>(&'t mut self, node: Tr<T>) -> T::ViewMut<'t, T> {
        T::ViewMut::from_tr(self.get_raw_view_mut(), node)
    }
    
    pub fn get_raw_view(&self) -> RawTreeView<T> {
        RawTreeView {
            buffer: self.buffer,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }
    
    pub fn get_raw_view_mut(&mut self) -> RawTreeViewMut<T> {
        RawTreeViewMut {
            buffer: self.buffer,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }
}

impl<T: TreeNode> Index<Tr<T>> for Tree<T> {
    type Output = T::Container;

    fn index(&self, index: Tr<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T: TreeNode> IndexMut<Tr<T>> for Tree<T> {
    fn index_mut(&mut self, index: Tr<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T: TreeNode> Drop for Tree<T> {
    fn drop(&mut self) {
        if std::mem::needs_drop::<T>() {
            let mut offset: usize = 0;

            while offset < self.used {
                let node = unsafe { self.get_mut_unchecked(offset) };
                let size = std::mem::size_of::<T::Container>() + node.extra_size();
                unsafe {
                    std::ptr::drop_in_place(node as *mut T::Container);
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

impl<'t, R: TreeNode> RawTreeView<'t, R> {
    fn ensure_discriminator<T: TreeNode>(&self, node: Tr<T>) {
        if node.discriminator != self.discriminator {
            panic!(
                "Accessing node from different tree: expected {:?}, got {:?}",
                self.discriminator, node.discriminator
            );
        }
    }
    
    unsafe fn get_unchecked<T: TreeNode>(&self, offset: usize) -> &'t T::Container {
        &*(self.buffer.add(offset) as *const T::Container)
    }
    
    pub fn get<T: TreeNode>(&self, node: Tr<T>) -> &'t T::Container {
        self.ensure_discriminator(node);
        unsafe { self.get_unchecked::<T>(node.offset) }
    }
}

impl<'t, R: TreeNode> RawTreeViewMut<'t, R> {
    fn ensure_discriminator<T: TreeNode>(&self, node: Tr<T>) {
        if node.discriminator != self.discriminator {
            panic!(
                "Accessing node from different tree: expected {:?}, got {:?}",
                self.discriminator, node.discriminator
            );
        }
    }
    
    pub fn downcast(self) -> RawTreeView<'t, R> {
        RawTreeView {
            buffer: self.buffer,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        } 
    }
    
    pub unsafe fn get_unchecked<T: TreeNode>(&self, offset: usize) -> &'t mut T::Container {
        &mut *(self.buffer.add(offset) as *mut T::Container)
    }
    
    pub fn get_mut<T: TreeNode>(&self, node: Tr<T>) -> &'t mut T::Container {
        self.ensure_discriminator(node);
        unsafe { self.get_unchecked::<T>(node.offset) }
    }
    
    pub unsafe fn unsafe_clone(&self) -> Self {
        Self {
            buffer: self.buffer,
            discriminator: self.discriminator,
            _phantom: PhantomData,
        }
    }
}

/*
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

    impl TreeNode for TestNode<'_> {
        type Container = Self;
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
*/
