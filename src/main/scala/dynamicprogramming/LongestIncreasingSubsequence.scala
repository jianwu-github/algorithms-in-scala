package dynamicprogramming

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

/**
 * "The longest increasing subsequence problem is to find a subsequence of a given
 *  sequence in which the subsequence's elements are in sorted order, lowest to
 *  highest, and in which the subsequence is as long as possible. This subsequence
 *  is not necessarily contiguous, or unique."
 */
object LongestIncreasingSubsequence {

    /**
     * Solving Longest Increasing Subsequence using Dynamic Programming, this
     * is a O(N^2) solution, N is the length of the given sequence.
     */
    def longestIncreasingSubsequenceDP(seq: Array[Int]): Array[Int] = {
        if (seq.length < 2) seq
        else {
            def buildingIncSeqs(done: Array[Int], remaining: Array[Int]): List[Array[Int]] =
                if (remaining.length == 0) List(done)
                else if (remaining.head > done.last)
                	buildingIncSeqs(done :+ remaining.head, remaining.tail) ++ buildingIncSeqs(done, remaining.tail)
                else buildingIncSeqs(done, remaining.tail)

            // building all increasing subsequences from seq
            var allIncSeqs: List[Array[Int]] = List()
            for ( i <- 0 until seq.length) {
                 val subseq = seq.drop(i)
                 allIncSeqs = allIncSeqs ::: buildingIncSeqs(Array(subseq.head), subseq.tail)
            }

            // find the max length of the all increasing subsequences
            var maxLengthOfLIS = 0
            allIncSeqs.foreach {s => if (s.length > maxLengthOfLIS) maxLengthOfLIS = s.length}

            // pick the first increasing subsequnce with max LIS length
            allIncSeqs.filter(s => s.length == maxLengthOfLIS).head
        }
    }

    case class Node(element: Int, prevStackNode: Option[Node]) {
        override def toString: String = prevStackNode match {
            case Some(p) => "v: " + element + ", prev: " + p.element
            case _ => "v: " + element + ", prev: None"
        }
    }

    /**
     * Solving Longest Increasing Subsequence using Patience Sorting, this
     * is a O(NlogN) solution, N is the length of the given sequence.
     */
    def longestIncreasingSubsequencePS(seq: Array[Int]): Array[Int] = {
        val seqLength = seq.length

        if (seqLength < 2) seq
        else {
        	val piles = ListBuffer.empty[Stack[Node]]

        	def prevPile(currPile: Stack[Node]): Option[Stack[Node]] = {
        	    if (piles.length > 1) {
        	        var prevPile = piles.head
        	        for (pile <- piles.tail) {
        	            if (pile.top.element == currPile.top.element) {
        	                return Some(prevPile)
        	            }

        	            prevPile = pile
        	        }
        	    }

        	    None
        	}

        	for (i <- 0 until seqLength) {
        	    val toAddPile = piles.find(p => p.top.element >= seq(i))

        	    toAddPile match {
                    case Some(p) => {
                        val prev = prevPile(p)
                        prev match {
                          case Some(pp) => p.push(Node(seq(i), Some(pp.top)))
                          case None => p.push(Node(seq(i), None))
                        }
                    }
                    case None => {
                        val prev = if (piles.isEmpty) None else Some(piles.last.top)
                        piles += Stack(Node(seq(i), prev))
                    }
                }
        	}

        	// Building LIS by backtracking
            val subSeq = ListBuffer.empty[Int]
            var lastNode: Option[Node] = Some(piles.last.top)
            while (lastNode != None) {
                subSeq += lastNode.get.element
                lastNode = lastNode.get.prevStackNode
            }

            subSeq.reverse.toArray
        }
    }
}