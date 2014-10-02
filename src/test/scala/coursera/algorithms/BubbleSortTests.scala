package coursera.algorithms

import org.scalatest.FunSuite

class BubbleSortTests extends FunSuite {
  
  trait TestSets {
    def testSorted(arr:Array[Double]):Boolean = {
      (arr.zip(arr.tail)).forall(t => t._1<=t._2)
    }
    
    val testData = Array.fill(10)(math.random)        
  }
  
  test("Bubble Sort Algorithm is correctly implemented") {
    new TestSets {
      algbubblesort.bubbleSort(testData)
      
      assert(testSorted(testData), true)
    }
  }
}