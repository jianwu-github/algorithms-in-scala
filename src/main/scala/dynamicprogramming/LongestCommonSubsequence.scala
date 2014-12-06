package dynamicprogramming

/**
 * Solving the longest common subsequence (LCS) problem, which is the problem of finding 
 * the longest subsequence common to two sequences; the subsequence are not required to 
 * occupy consecutive positions within the original sequences. 
 */
object LongestCommonSubsequence {
  
    /**
     * Find longest common subsequence using recursion
     */
    def longestCommonSubsequenceNaive(s1: String, s2: String): String = {
        if (s1 == null || s1.length() == 0 || s2 == null || s2.length() == 0) ""
        else if (s1 == s2) s1
        else {
            if (s1.head == s2.head) s1.head + longestCommonSubsequenceNaive(s1.substring(1), s2.substring(1))
            else {
                val lcs1 = longestCommonSubsequenceNaive(s1, s2.substring(1))
                val lcs2 = longestCommonSubsequenceNaive(s1.substring(1), s2)
                
                if (lcs1.length() > lcs2.length()) lcs1
                else lcs2
            }
        }
    }
    
    /**
     * Find longest common subsequence using Dynamic Programming
     */
    def longestCommonSubsequenceDP(s1: String, s2: String): String = {
        if (s1 == null || s1.length() == 0 || s2 == null || s2.length() == 0) ""
        else if (s1 == s2) s1
        else {
             val up = 1
             val left = 2
             val charMatched = 3
             
             val s1Length = s1.length()
             val s2Length = s2.length()
             
             val lcsLengths = Array.fill[Int](s1Length + 1, s2Length + 1)(0)
             
             for (i <- 0 until s1Length) {
                 for (j <- 0 until s2Length) {
	                 if (s1.charAt(i) == s2.charAt(j)) {
	                     lcsLengths(i + 1)(j + 1) = lcsLengths(i)(j) + 1
	                 } else {
	                     if (lcsLengths(i)(j + 1) >= lcsLengths(i + 1)(j)) {
	                         lcsLengths(i + 1)(j + 1) = lcsLengths(i)(j + 1)
	                     } else {
	                         lcsLengths(i + 1)(j + 1) = lcsLengths(i + 1)(j)
	                     }
	                 }
                 }
             }
             
             val subSeq = new StringBuilder()
             var s1Pos = s1Length
             var s2Pos = s2Length
             
             // build longest subsequence by backtracking
             do {
                if (lcsLengths(s1Pos)(s2Pos) == lcsLengths(s1Pos -1)(s2Pos)) {
                    s1Pos -= 1
                } else if (lcsLengths(s1Pos)(s2Pos) == lcsLengths(s1Pos)(s2Pos - 1)) {
                    s2Pos -= 1
                } else {
                    assert(s1.charAt(s1Pos - 1) == s2.charAt(s2Pos - 1))
                    subSeq += s1.charAt(s1Pos - 1)
                    s1Pos -= 1
                    s2Pos -= 1
                }
               
             } while (s1Pos > 0 && s2Pos > 0)
               
             subSeq.toString.reverse  
        }
    }

}