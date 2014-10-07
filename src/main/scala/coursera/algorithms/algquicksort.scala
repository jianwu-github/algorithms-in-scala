package coursera.algorithms

object algquicksort {
  def quickSort[T <% Ordered[T]](lst: List[T]): List[T] = {
    if (lst.isEmpty || lst.tail.isEmpty) lst
    else { 
      val pivot = lst.head
      val (beforePivot, afterPivot) = lst.tail.partition(_ < pivot)
      quickSort(beforePivot) ++ (pivot :: quickSort(afterPivot))
    }
  } 
}