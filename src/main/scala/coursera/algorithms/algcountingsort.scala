package coursera.algorithms

object algcountingsort {
    /*
     * Counting Sort or Key-indexed counting is based on the assumptions about keys, 
     * it is assumed that keys are integers between 0 and R - 1. 
     */
    def countingSort(a: Array[Int], R: Int): Array[Int] = {
        val N = a.length
	    val count = Array.fill[Int](R + 1)(0)
	    val b = new Array[Int](N)
	    
	    // Count frequencies of each integer using key as index
	    for (i <- 0 to N - 1) {
	        val x = a(i)
	        count(x + 1) += 1
	    }
	    
        // Compute frequency cumulates which specify destinations
        for (j <- 0 to R - 1) {
            count(j + 1) = count(j + 1) + count(j)
        }
	   
        // Access cumulates using key as index to move items to correct sorted positions
        for (k <- 0 to N - 1) {
            val bPos = count(a(k))
            b(bPos) = a(k)
            count(a(k)) += 1
        }
        
	    b
    }
}