package coursera.algorithms

object algmergesort {
  private def merge(a: List[Double], b: List[Double]): List[Double] = (a,b) match {
    case(Nil, _) => b
    case(_, Nil) => a
    case(x::xs, y::ys) =>
      if (x <= y) x :: merge(xs, b)
      else y :: merge(a, ys)
  }
  
  def mergeSort(lst: List[Double]): List[Double] = {
    if (lst.length < 2) lst
    else {
      val (first, second) = lst.splitAt(lst.length / 2)
      merge(mergeSort(first), mergeSort(second))
    }    
  }
}