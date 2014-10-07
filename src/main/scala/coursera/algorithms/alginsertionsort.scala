package coursera.algorithms

object alginsertionsort {
  def insertionSort[T <% Ordered[T]](arr: Array[T]) {
    for ( i <- 1 until arr.length) {
      val tmp = arr(i)
      var j = i-1
      while ( j > -1 && arr(j) > tmp) {
        arr(j+1) = arr(j)
        j -= 1
      }
      
      // For half sorted array, insertion sort has 
      // big advantage over selection sort as its
      // inner while loop will stop very quickly
      
      arr(j+1) = tmp
    }
  }

}