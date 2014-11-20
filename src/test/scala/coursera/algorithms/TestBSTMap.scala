package coursera.algorithms

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import coursera.algorithms.BSTMap._
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestBSTMap extends FunSuite {
    trait TestBST {
        var map = new BSTMap[String, Int]()
        
        def isOrdered(l:List[String]): Boolean = l match {
            case Nil => true
            case x :: Nil => true
            case x :: xs => (x <= xs.head) && isOrdered(xs)
        }
    }

    test("get None with Empty BST") {
	    new TestBST {
	        assert(map.get("5") === None)
	    }
    }
    
    test("add and get with BST") {
	    new TestBST {
	        map += "5" -> 5
	        map += "8" -> 8
	        map += "3" -> 3
	       
	        assert(map.get("3") === Some(3))
	        assert(map.get("5") === Some(5))
	        assert(map.get("8") === Some(8))       
	    }
    }
    
    test("inOrderTravel produces a sorted list") {
        new TestBST {
            map += "9" -> 9
	        map += "8" -> 8
	        map += "5" -> 5
	        map += "3" -> 3
	        
	        val inOrderList = ArrayBuffer[String]()
	        def createList(k: String, v: Int) {
	            inOrderList += k
	        }
	        
	        map.inOrderTravel(createList)
	        
	        val sortedList = inOrderList.toList
	        info(sortedList mkString ", ")
	        
	        assert(isOrdered(sortedList))
        }
    }
    
    test("Verify two BST have the identical corresponding elements regardless their shapes") {
        new TestBST {
            map += "9" -> 9
	        map += "8" -> 8
	        map += "5" -> 5
	        map += "3" -> 3
	        
	        var anotherMap = new BSTMap[String, Int]()
	        anotherMap += "3" -> 3
	        anotherMap += "8" -> 8
	        anotherMap += "9" -> 9
	        anotherMap += "5" -> 5
	        
	        val inOrderList = ArrayBuffer[String]()
	        def createList(k: String, v: Int) {
	            inOrderList += k
	        }
	        
	        map.inOrderTravel(createList)
	        val sortedList = inOrderList.toList
	        
	        var hasSameElements = true
	        var pos = 0;
	        
	        def checkCorrespondingElement(k: String, v: Int) = {
	            if (hasSameElements) {
	               hasSameElements = (k == sortedList(pos)) && hasSameElements
	               pos += 1
	            }
	        }
	        
	        anotherMap.inOrderTravel(checkCorrespondingElement)
	        
	        assert(hasSameElements)
        }
    }
    
}