package coursera.algorithms

object algshellsort {
  def shellSort[T <% Ordered[T]](arr: Array[T]) {
    val size = arr.length
    
    // calculate 3x+1 increment sequence from Knuth
    var gap = 1
    while (gap < (size / 3)) gap = gap * 3 + 1 
    
    while (gap >= 1) {
      for ( i <- gap until size) {
        val tmp = arr(i)
        var j = i
        while ( j >= gap  && arr(j - gap) > tmp) {
          arr(j) = arr(j - gap)
          j -= gap
        }
      
        arr(j) = tmp
      }
      
      gap = gap / 3
    }
  }
}