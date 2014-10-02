package coursera.algorithms

object algquicksort {
  def quickSort(lst: List[Double]): List[Double] = {
    if (lst.isEmpty || lst.tail.isEmpty) lst
    else { 
      val pivot = lst.head
      val (beforePivot, afterPivot) = lst.tail.partition(_ < pivot)
      quickSort(beforePivot) ++ (pivot :: quickSort(afterPivot))
    }
  } 
}