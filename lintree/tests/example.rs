use lintree::{Tr, Tree};

#[derive(lintree::TreeNode)]
enum MyTree {
    Leaf(u64),
    Branch(Tr<MyTree>, Tr<MyTree>),
}

#[test]
fn test() {
    let mut tree = Tree::<MyTree>::with_capacity(16);
    let l1 = tree.alloc(MyTree::Leaf(5));        
    let l2 = tree.alloc(MyTree::Leaf(2));
    let l3 = tree.alloc(MyTree::Leaf(7));
    let l4 = tree.alloc(MyTree::Leaf(3));
    let b1 = tree.alloc(MyTree::Branch(l1, l2));
    let b2 = tree.alloc(MyTree::Branch(l3, l4));
    let root = tree.alloc(MyTree::Branch(b1, b2));
}

