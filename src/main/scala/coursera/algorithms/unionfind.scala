package coursera.algorithms

/**
 * Given a set of N objects.
 * -- Union command: connect two objects.
 * -- Find/connected query: is there a path connecting the two objects?
 * 
 * Weighted Quick Union Find
 * -- Modify quick-union to avoid tall trees.
 * -- Keep track of size of each tree (number of objects).
 * -- Balance by linking root of smaller tree to root of larger tree.
 */
object unionfind {
  def main(args: Array[String]) {
        val wquf = new WeightQuickUnionFind(10)
        wquf.union(4, 3)
    	wquf.union(3, 8)
    	wquf.union(6,  5)
    	wquf.union(9, 4)
    	wquf.union(2, 1)
    	wquf.union(8, 9)
    	wquf.union(5, 0)
    	wquf.union(7, 2)
    	wquf.union(6, 1)
    	wquf.union(1, 0)
    	wquf.union(6, 7)
    	
    	println("Count is " + wquf.count)
    }
}

class WeightQuickUnionFind(val numOfNodes: Int) {
    private val ids = new Array[Int](numOfNodes)
	private val szs = new Array[Int](numOfNodes)
	
	for (i <- 0 until numOfNodes) {
	    ids(i) = i
	    szs(i) = 1
	}
	
    var count: Int = numOfNodes;
    
    def find(p: Int): Int = {
        var vp = p
        while (vp != ids(vp)) {
            vp = ids(vp)  
        }

        vp
    }
    
    def connected(p: Int, q: Int): Boolean = find(p) == find(q)
	
    def union(p: Int, q: Int): Unit = {
        var i = find(p)
        var j = find(q)
        
        if (i != j) {
            if (szs(i) < szs(j)) {
               ids(i) = j
               szs(j) += szs(i)
            } else {
               ids(j) = i
               szs(i) += szs(j)
            }
            
            count -= 1
         }
    }
}