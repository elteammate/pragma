use lintree::{Tree, Tr};

#[lintree::tree_node]
enum MyTree {
    Leaf(u64),
    Branch(#[compose] Tr<MyTree>, Tr<MyTree>),
}

#[test]
fn test() {
    let mut tree = Tree::<MyTree>::with_capacity(16);
    let l1 = tree.push(MyTree::Leaf(5));
    let l2 = tree.push(MyTree::Leaf(2));
    let l3 = tree.push(MyTree::Leaf(7));
    let l4 = tree.push(MyTree::Leaf(3));
    let b1 = tree.push(MyTree::Branch(l1, l2));
    let b2 = tree.push(MyTree::Branch(l3, l4));
    let root = tree.push(MyTree::Branch(b1, b2));
}
