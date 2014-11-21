package coursera.algorithms

object algradixsort {
   /**
    * This implementation of radix sort algorithm will sort positive numbers 
    */
   def radixSort(a:Array[Int]) {
       assert(a.forall(_ >= 0))
       
       var maxina = a.max
       var powerOfTen = 1
       
       while (maxina > 0) {
           val byDigit = Array.fill(10)(List[Int]())
           for (num <- a) {
               val digit = (num / powerOfTen) % 10
               byDigit(digit) ::= num
           }
           
           // shuffle the numbers in a array based on sorting result using current digit 
           var i = 0
           for (bin <- byDigit; num <- bin.reverse) {
               a(i) = num
               i += 1
           }
           
           powerOfTen *= 10
           maxina = maxina / 10
       }
       
   }
}