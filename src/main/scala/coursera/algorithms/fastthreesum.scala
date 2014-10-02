package coursera.algorithms

import scala.annotation.tailrec

/**
 * 3-SUM Problem: Given N distinct integers, how many triples sum to exactly zero?
 * 
 * Fast 3-SUM is Sorting-based algorithm:
 * (1). Sort the N (distinct) numbers.
 * (2). For each pair of numbers a[i] and a[j], binary search for -(a[i] + a[j]).
 */
object fastthreesum {
    type Index = Int

    def binarySearch(ds: Array[Int], key: Int): Option[Index] = {
    	@tailrec
    	def go(lo: Index, hi: Index): Option[Index] = {
    		if (lo > hi)
    			None
    		else {
    			val mid: Index = lo + (hi - lo) / 2
    			ds(mid) match {
    				case mv if (mv == key) => Some(mid)
    				case mv if (mv <= key) => go(mid + 1, hi)
    				case _ => go(lo, mid - 1)
    			}
    		}
    	}
      
    	go(0, ds.size - 1)
    } 
    
    def countZeroSum(dataSet: List[Int]) : Int = {
    	val sortedDataSet = dataSet.sortWith(_ < _)
    	val sortedDataArray = sortedDataSet.toArray
    	val numOfData = sortedDataSet.length
    	
    	var count: Int = 0
    	for ( 
    	    i <- 0 until numOfData;
    	    j <- (i + 1) until numOfData) {
    	    val ngval = -(sortedDataArray(i) + sortedDataArray(j))
    		binarySearch(sortedDataArray, ngval) match {
    	    	case Some(k) => {
    	    	    if (k > j) 
    	    	      count += 1 
    	    	    else count += 0
    	    	}
    	    	case None => count += 0
    	    }	
    	}
    	
    	count
   }
  
   def main(args: Array[String]) {
      val testDataSet = List(30, -40, -20, -10, 40, 0, 10, 5)
      
      println("count is " + countZeroSum(testDataSet))
   }
}
