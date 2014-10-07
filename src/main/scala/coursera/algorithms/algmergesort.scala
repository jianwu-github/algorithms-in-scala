package coursera.algorithms

object algmergesort {
  private def merge[T <% Ordered[T]](a: List[T], b: List[T]): List[T] = (a,b) match {
    case(Nil, _) => b
    case(_, Nil) => a
    case(x::xs, y::ys) =>
      if (x <= y) x :: merge(xs, b)
      else y :: merge(a, ys)
  }
  
  def mergeSort[T <% Ordered[T]](lst: List[T]): List[T] = {
    if (lst.length < 2) lst
    else {
      val (first, second) = lst.splitAt(lst.length / 2)
      merge(mergeSort(first), mergeSort(second))
    }    
  }
}