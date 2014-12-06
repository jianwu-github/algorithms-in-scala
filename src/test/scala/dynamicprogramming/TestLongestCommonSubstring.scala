package dynamicprogramming

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestLongestCommonSubstring extends FunSuite {
    trait TestLCSubstring {
        val s1 = "Please, peter go swimming!"
        val s2 = "I’m peter goliswi"
        val maxLength = 9
    }
    
    test("Longest Common Substring with Brute-force is correctly implemented") {
        new TestLCSubstring {
            val result = LongestCommonSubstring.longestCommonSubstringNaive(s1, s2)  
            info("\"" + result + "\"")
            assert(result != null && result.length() == maxLength)
        }
    }
    
    test("Longest Common Substring using Dynamic Programming is correctly implemented") {
        new TestLCSubstring {
            val result = LongestCommonSubstring.longestCommonSubstringDP(s1, s2)  
            info("\"" + result + "\"")
            assert(result != null && result.length() == maxLength)
        }
    }
}