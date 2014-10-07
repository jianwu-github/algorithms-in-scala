package coursera.algorithms

object algselectionsort {
	def selectionSort[T <% Ordered[T]](arr: Array[T]) {
	  for (i <- 0 until arr.length - 1) {
	    var min = i
	    for (j <- i+1 until arr.length) {
	      if (arr(j) < arr(min)) min = j
	    }
	    
	    // Selection Sort is better than Bubble Sort 
	    // as it only does swap in the outer loop 
	    if (min != i) {
	      val tmp = arr(i)
	      arr(i) = arr(min)
	      arr(min) = tmp
	    }
	  }
	}
}