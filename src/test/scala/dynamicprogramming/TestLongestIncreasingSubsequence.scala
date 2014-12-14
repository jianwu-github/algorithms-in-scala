package dynamicprogramming

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestLongestIncreasingSubsequence extends FunSuite {
    trait TestLISubsequence {
         val shortSeq = Array(3, 2, 6, 4, 5, 1)
         val shortLISLength = 3

         val longSeq = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
         val longLISLength = 6
    }

    test("Longest Increasing Subsequence using Dynamic Programming is correctly implemented") {
        new TestLISubsequence {
            val shortLIS = LongestIncreasingSubsequence.longestIncreasingSubsequenceDP(shortSeq)

            assert(shortLIS != null && shortLIS.length == shortLISLength)

            info("Short Seq: " + (shortSeq mkString ", "))
            info("Short LIS: " + (shortLIS mkString ", "))

            val longLIS = LongestIncreasingSubsequence.longestIncreasingSubsequenceDP(longSeq)

            assert(longLIS != null && longLIS.length == longLISLength)

            info("Long Seq: " + (longSeq mkString ", "))
            info("Long LIS: " + (longLIS mkString ", "))
        }
    }

    test("Longest Increasing Subsequence using Patience Sorting is correctly implemented") {
        new TestLISubsequence {
            val shortLIS = LongestIncreasingSubsequence.longestIncreasingSubsequencePS(shortSeq)

            assert(shortLIS != null && shortLIS.length == shortLISLength)

            info("Short Seq: " + (shortSeq mkString ", "))
            info("Short LIS: " + (shortLIS mkString ", "))

            val longLIS = LongestIncreasingSubsequence.longestIncreasingSubsequencePS(longSeq)

            assert(longLIS != null && longLIS.length == longLISLength)

            info("Long Seq: " + (longSeq mkString ", "))
            info("Long LIS: " + (longLIS mkString ", "))
        }
    }

}