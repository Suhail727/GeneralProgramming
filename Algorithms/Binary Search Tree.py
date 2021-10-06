class Node: 
  
    # Constructor to create a new node 
    def __init__(self, key): 
        self.val = key  
        self.left = None
        self.right = None
  
# A utility function to search a given key in BST 
def search(root,key): 
      
    # Base Cases: root is null or key is present at root 
    if root is None or root.val == key: 
        return root 
  
    # Key is greater than root's key 
    if root.val < key: 
        return search(root.right,key) 
    
    # Key is smaller than root's key 
    return search(root.left,key) 

# A utility function to do inorder traversal of BST 
def inorder(root): 
    if root is not None: 
        inorder(root.left) 
        print(root.val) 
        inorder(root.right) 
  
  
# A utility function to insert a new node with given val in BST 
def insert( node, key): 
  
    # If the tree is empty, return a new node 
    if node is None: 
        return Node(key) 
  
    # Otherwise recur down the tree 
    if key < node.val: 
        node.left = insert(node.left, key) 
    else: 
        node.right = insert(node.right, key) 
  
    # return the (unchanged) node pointer 
    return node 
  
# Given a non-empty binary search tree, return the node 
# with minum key value found in that tree. Note that the 
# entire tree does not need to be searched  
def minValueNode( node): 
    current = node 
  
    # loop down to find the leftmost leaf 
    while(current.left is not None): 
        current = current.left  
  
    return current  
  
# Given a binary search tree and a key, this function 
# delete the key and returns the new root 
def deleteNode(root, key): 
  
    # Base Case 
    if root is None: 
        return root  
  
    # If the key to be deleted is smaller than the root's 
    # key then it lies in  left subtree 
    if key < root.val: 
        root.left = deleteNode(root.left, key) 
  
    # If the kye to be delete is greater than the root's key 
    # then it lies in right subtree 
    elif(key > root.val): 
        root.right = deleteNode(root.right, key) 
  
    # If key is same as root's value, then this is the node 
    # to be deleted 
    else: 
          
        # Node with only one child or no child 
        if root.left is None : 
            temp = root.right  
            root = None 
            return temp  
              
        elif root.right is None : 
            temp = root.left  
            root = None
            return temp 
  
        # Node with two children: Get the inorder successor 
        # (smallest in the right subtree) 
        temp = minValueNode(root.right) 
  
        # Copy the inorder successor's content to this node 
        root.val = temp.val 
  
        # Delete the inorder successor 
        root.right = deleteNode(root.right , temp.val) 
  
  
    return root  
  
# Driver program to test above functions 
""" Let us create following BST 
              50 
           /     \ 
          30      70 
         /  \    /  \ 
       20   40  60   80 """
  
root = None
root = insert(root, 50) 
root = insert(root, 30) 
root = insert(root, 20) 
root = insert(root, 40) 
root = insert(root, 70) 
root = insert(root, 60) 
root = insert(root, 80) 
  
print ("Inorder traversal of the given tree")
inorder(root) 
  
print ("\nDelete 20")
root = deleteNode(root, 20) 
print ("Inorder traversal of the modified tree")
inorder(root) 
  
print ("\nDelete 30")
root = deleteNode(root, 30) 
print ("Inorder traversal of the modified tree")
inorder(root) 
  
print ("\nDelete 50")
root = deleteNode(root, 50) 
print ("Inorder traversal of the modified tree")
inorder(root)
print("Is 70 present in the tree", search(root,70).val)
