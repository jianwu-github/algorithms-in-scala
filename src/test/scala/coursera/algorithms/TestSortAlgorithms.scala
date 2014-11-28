package coursera.algorithms

import org.scalatest.FunSuite

class TestSortAlgorithms extends FunSuite {
  implicit def arrayToList[A](a: Array[A]) = a.toList 
  
  trait TestSets {
    val numOfTestData = 20
    
    def createTestDataArray: Array[Double] = Array.fill(numOfTestData)(math.random)
    def createTestDataList: List[Double] = List.fill(numOfTestData)(math.random)
    
    def testSorted(arr:Array[Double]):Boolean = {
        (arr.zip(arr.tail)).forall(t => t._1<=t._2)
    }
    
    def testSorted(lst:List[Double]):Boolean = {
      (lst.zip(lst.tail)).forall(t => t._1<=t._2)
    } 
  }
  
  test("Bubble Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestDataArray
      
      algbubblesort.bubbleSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
  
  test("Selection Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestDataArray
      
      algselectionsort.selectionSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
  
  test("Insertion Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestDataArray
      
      alginsertionsort.insertionSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
  
  test("Inserting Sort Algorithm using Pattern Matching is correctly implemented") {
    new TestSets {
      val testData = createTestDataList
      
      val sortedData =  alginsertionsort.insertionSort(testData)
      
      assert(testSorted(sortedData), true)
    }
  }
  
  test("Shell Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestDataArray
      
      algshellsort.shellSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
  
  test("Merge Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestDataList
      
      val sortedData = algmergesort.mergeSort(testData)
      
      assert(testSorted(sortedData), true)
    }
  }
  
  test("Quick Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestDataList
      
      val sortedData = algquicksort.quickSort(testData)
      
      assert(testSorted(sortedData), true)
    }
  }
  
  def testSorted(lst:List[Int]): Boolean = {
      (lst.zip(lst.tail)).forall(t => t._1<=t._2)
  }
  
  test("Counting Sort or Key-indexed Counting is correctly implemented") {
      val a = Array(2, 5, 3, 0, 2, 3, 0 ,3)
      val R = 6
      
      info("before counting sort: " + scala.runtime.ScalaRunTime.stringOf(a))
      val sortedData = algcountingsort.countingSort(a, R)
      info("after counting sort: " + scala.runtime.ScalaRunTime.stringOf(sortedData))
      
      assert(testSorted(sortedData.toList))
  }
  
  test("Radix Sort is working with positive numbers") {
      val a = Array(123, 431, 346, 23, 0, 543, 972, 452)
      info("before radix sort: " + scala.runtime.ScalaRunTime.stringOf(a))
      algradixsort.radixSort(a)
      info("after radix sort: " + scala.runtime.ScalaRunTime.stringOf(a))
      
      assert(testSorted(a.toList))
  }  
}