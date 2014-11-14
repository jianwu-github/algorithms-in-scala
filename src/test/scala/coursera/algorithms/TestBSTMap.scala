package coursera.algorithms

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import coursera.algorithms.BSTMap._

@RunWith(classOf[JUnitRunner])
class TestBSTMap extends FunSuite {
    trait TestBST {
        var map = new BSTMap[String, Int]
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
}