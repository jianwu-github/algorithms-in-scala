package dynamicprogramming

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestLongestCommonSubsequence extends FunSuite {
    trait TestLCSubsequence {
        val s1 = "thisisaatest"
        val s2 = "testing123testing"
        val maxLength = 7
    }
    
    test("Longest Common Subsequence using recursion is correctly implemented") {
        new TestLCSubsequence {
            val result = LongestCommonSubsequence.longestCommonSubsequenceNaive(s1, s2)  
            info("\"" + result + "\"")
            assert(result != null && result.length() == maxLength)
        }
    }
    
    test("Longest Common Subsequence using Dynamic Programming is correctly implemented") {
        new TestLCSubsequence {
            val result = LongestCommonSubsequence.longestCommonSubsequenceDP(s1, s2)  
            info("\"" + result + "\"")
            assert(result != null && result.length() == maxLength)
        }
    }
}