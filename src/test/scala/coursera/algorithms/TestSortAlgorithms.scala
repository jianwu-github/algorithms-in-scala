package coursera.algorithms

import org.scalatest.FunSuite

class TestSortAlgorithms extends FunSuite {
  
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
}