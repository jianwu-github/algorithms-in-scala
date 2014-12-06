package dynamicprogramming

import util.control.Breaks._

/**
 * Solving the longest common substring problem, which is to find the longest string (or strings) 
 * that is a substring (or are substrings) of two strings.
 */
object LongestCommonSubstring {
    
    /**
     * Find longest common substring with Brute-force 
     */
    def longestCommonSubstringNaive(s1: String, s2: String): String = {
        val s1Length = s1.length()
        val s2Length = s2.length()
        
        if (s1Length == 0 || s2Length == 0) ""
        else {  
             var start = 0
             var maxLength = 0
             var subLength = 0
             for (i <- 0 until s1Length; j <- 0 until s2Length) {
                 subLength = 0
                 
                 breakable {
                     while (s1.charAt(i + subLength) == s2.charAt(j + subLength)) {
                        subLength += 1
                        if ((i + subLength) >= s1Length || (j + subLength) >= s2Length) break // reached the end of one string
                     }  
                 }
                 
                 if (subLength > maxLength) {
                    start = i
                    maxLength = subLength
                 }
             }
             
             if (maxLength > 0) s1.substring(start, (start + maxLength))
             else ""
        }
    }
    
    /**
     * Find longest common substring using Dynamic Programming
     */
    def longestCommonSubstringDP(s1: String, s2: String): String = {
        if (s1 == null || s1.length() == 0 || s2 == null || s2.length() == 0) ""
        else if (s1 == s2) s1
        else {
             val up = 1
             val left = 2
             val charMatched = 3
             
             val s1Length = s1.length()
             val s2Length = s2.length()
             
             val lcsLengths = Array.fill[Int](s1Length + 1, s2Length + 1)(0)
             var lcsMaxLength = 0
             
             for (i <- 0 until s1Length) {
                 for (j <- 0 until s2Length) {
	                 if (s1.charAt(i) == s2.charAt(j)) {
	                     if (i == 0 || j == 0) lcsLengths(i + 1)(j + 1) = 1
	                     else lcsLengths(i + 1)(j + 1) = lcsLengths(i)(j) + 1
	                 } 
	                 
	                 if (lcsLengths(i + 1)(j + 1) > lcsMaxLength) lcsMaxLength = lcsLengths(i + 1)(j + 1)
                 }
             }
             
             if (lcsMaxLength == 0) ""
             else {      
	             var s1Pos = 0
	             var s2Pos = 0
	             
	             breakable {
	                 for ( i <- s1Length to 0 by -1) {
	                     for (j <- s2Length to 0 by -1) {
	                         if (lcsLengths(i)(j) == lcsMaxLength) {
	                             s1Pos = i
	                             s2Pos = j
	                             break
	                         }
	                     }
	                 }
	             }
             
	             s1.substring(s1Pos - lcsMaxLength, s1Pos)
             }
        }
    }
}