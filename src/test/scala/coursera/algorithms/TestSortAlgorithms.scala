package coursera.algorithms

import org.scalatest.FunSuite

class TestSortAlgorithms extends FunSuite {
  
  trait TestSets {
    val numOfTestData = 20
    
    def createTestData: Array[Double] = Array.fill(numOfTestData)(math.random)
    
    def testSorted(arr:Array[Double]):Boolean = {
      (arr.zip(arr.tail)).forall(t => t._1<=t._2)
    }       
  }
  
  test("Bubble Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestData
      
      algbubblesort.bubbleSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
  
  test("Selection Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestData
      
      algselectionsort.selectionSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
  
  test("Insertion Sort Algorithm is correctly implemented") {
    new TestSets {
      val testData = createTestData
      
      alginsertionsort.insertionSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
}