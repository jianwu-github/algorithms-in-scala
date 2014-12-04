package coursera.algorithms

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import coursera.algorithms._
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestSubstringSearch extends FunSuite {
    trait TestSearch {
        val text = "abacadabrabracabracadabrabrabracad"
        val pattern = "abracadabra"
        val found = 14
    }
    
    test("Brute-force Substring Search is correctly implemented")  {
        new TestSearch {
            val result = SubstringSearch.naiveSearch(pattern, text)
            assert(result == found)
        }
    }
    
    test("Substring Search using Knuth-Morris-Pratt Algorithm is correctly implemented")  {
        new TestSearch {
            val result = SubstringSearch.searchWithKMP(pattern, text)
            assert(result == found)
        }
    }
    
    test("Substring Search using Boyer-Moore Algorithm is correctly implemented")  {
        new TestSearch {
            val result = SubstringSearch.searchWithBM(pattern, text)
            assert(result == found)
        }
    }
}