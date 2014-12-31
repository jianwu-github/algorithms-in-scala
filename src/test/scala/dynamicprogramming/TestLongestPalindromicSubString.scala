package dynamicprogramming

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestLongestPalindromicSubString extends FunSuite {
    trait TestLPSubstring {
        val s1 = "babcbabcbaccba"
        val maxLength = 9
    }

    test("Longest Palindromic Substring with Brute-force is correctly implemented") {
        new TestLPSubstring {
            val result = LongestPalindromicSubstring.longestPalindromicSubstringNaive(s1)
            info("longest palindromic substring of \"babcbabcbaccba\" is \"" + result + "\"")
            assert(result != null && result.length() == maxLength)
        }
    }

    test("Longest Palindromic Substring using Dynamic Programming is correctly implemented") {
        new TestLPSubstring {
            val result = LongestPalindromicSubstring.longestPalindromicSubstringDP(s1)
            info("longest palindromic substring of \"babcbabcbaccba\" is \"" + result + "\"")
            assert(result != null && result.length() == maxLength)
        }
    }
}