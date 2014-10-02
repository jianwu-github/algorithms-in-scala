package coursera.algorithms

object algbubblesort {
  def bubbleSort(arr:Array[Double]) {
    for (i <- 0 until arr.length-1; j <- 0 until arr.length-1-i) {
      if (arr(j) > arr(j+1)) {
        val tmp = arr(j)
        arr(j) = arr(j+1)
        arr(j+1) = tmp
      }
    }
  }  
}