package coursera.algorithms

import scala.collection.mutable

/**
 * Implementing Binary Search Tree using Scala Map
 */
class BSTMap[K <% Ordered[K], V] extends mutable.Map[K, V] {
  
    private class Node(var key:K, var value:V, var left:Node, var right:Node)
    
    private var root: Node = null
    
    def +=(kv: (K, V)) = {
      val (key, value) = kv
      
      def recurAndSet(n: Node): Node = {
        if (n == null) new Node(key, value, null, null)
        else {
          if (n.key == key) n.value = value
          else if (key < n.key) n.left = recurAndSet(n.left)
          else n.right = recurAndSet(n.right)
          n
        }
      }
      
      root = recurAndSet(root)
      this
    }
    
    def -=(key: K) = {
        def removeMax(n: Node): (K, V, Node) = {
            if (n.right == null) {
                (n.key, n.value, n.left)
            } else {
              val (k, v, node) = removeMax(n.right)
              n.right = node
              (k, v, n)
            }
        }
        
        def recur(n: Node): Node = {
            if (n == null) null 
            else {
                if (n.key == key) {
                    // remove operation
                    if (n.left == null) n.right
                    else if (n.right == null) n.left
                    else {
                        // the node has two children, it'll
                        // be replaced by max child under 
                        // sub-tree
                        val (k, v, node) = removeMax(n.left)
                        n.left = node
                        n.key = k
                        n.value = v
                        n
                    }
                } else if (key < n.key) { 
                  n.left = recur(n.left)
                  n
                }
                else {
                  n.right = recur(n.right)
                  n
                }
                
            }
        }
      
        root = recur(root)
        this
    }
    
    def get(key:K):Option[V] = {
		var rover=root
		
		while(rover!=null && rover.key!=key) {
			rover = if (key<rover.key) rover.left else rover.right
		}
		
		if (rover==null) None else Some(rover.value)
	}
    
    // iterator is doing in-order traverse of the BST
    def iterator = new Iterator[(K, V)] {
        val stack = new mutable.ArrayStack[Node]

        def pushAllLeft(n:Node) {
            if (n != null) {
                stack.push(n)
                pushAllLeft(n.left)
            }
        }
          
        def hasNext:Boolean = stack.nonEmpty
          
        def next():(K,V) = {
            val n = stack.pop()
            pushAllLeft(n.right)
            (n.key,n.value)
        }

        pushAllLeft(root)   
    }
    
    def height:Int = {
        def helper(n:Node):Int = {
            if (n == null) -1
            else (helper(n.left) max helper(n.right)) + 1
        }
        
        helper(root)
    }
}

object BSTMap {

}