package coursera.algorithms

object alginsertionsort {
  /**
   * Insertion Sort in iterative style
   */
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
  
  /**
   * Insertion Sort in recursive style utilizing Scala Pattern Matching 
   */
  def insert[T <% Ordered[T]](x: T, lst: List[T]): List[T] = lst match {
    case Nil => List(x)
    case h :: tail => if (x <= h) x :: lst else h :: insert(x, tail)
  }
  
  def insertionSort[T <% Ordered[T]](lst: List[T]): List[T] = lst match {
    case Nil => Nil
    case h :: tail => insert(h, insertionSort(tail))
  }
  
}