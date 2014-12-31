package dynamicprogramming

/**
 * Longest palindromic substring.
 *
 * Given a string s, find the longest substring that is a palindrome
 * in expected linearithmic time.
 */
object LongestPalindromicSubstring {
    // def isPalindrome(s: String) = s.length >= 1 && s.equals(s.reverse)

    def isPalindrome[T](l: List[T]) : Boolean = l match {
        case Nil => true
        case _::Nil => true
        case h::t => (h == t.last) && isPalindrome (t.init)
    }

    def isPalindromicStr(s: String): Boolean = if (s == null) true else isPalindrome[Char](s.toList)

    def getSubstrings(s: String): Seq[String] =
        for {
            b <- 0 to (s.length - 1);
            e <- b to s.length
        } yield (s.substring(b, e))


    /**
     * Find longest palindromic substring with Brute-force
     */
    def longestPalindromicSubstringNaive(s: String): String = {
        var lps: String = null
        for (s <- getSubstrings(s) if isPalindromicStr(s)) {
            if (lps == null) lps = s
            else if (lps.length < s.length) lps = s
        }

        lps
    }

    /**
     * Find longest palindromic substring using Dynamic Programming
     */
    def longestPalindromicSubstringDP(s: String): String = {
        if (s == null || s.length <= 1) s
        else if (isPalindromicStr(s)) s
        else {
        	val sLength = s.length()
        	val table = Array.fill[Int](sLength, sLength)(0)

        	// every single letter is palindromic substring
        	for (i <- 0 until sLength) {
        	    table(i)(i) = 1
        	}

        	var longestPalindromicSubstr = s.substring(0, 1)

        	// two consecutive same letters are palindromic substring too
        	for (i <- 0 until (sLength - 1)) {
        		if (s.charAt(i) == s.charAt(i + 1)) {
        		  table(i)(i + 1) = 1;
        		  longestPalindromicSubstr = s.substring(i, i + 2)
        		}
        	}

        	// calculate whole table by checking longer substring
        	for (substrlen <- 3 until sLength) {
        	    for (b <- 0 until (sLength - substrlen)) {
        	        val e = b + substrlen - 1
        	        if (s.charAt(b) == s.charAt(e)) {
        	            table(b)(e) = table(b + 1)(e - 1)

        	            if (table(b)(e) == 1) {
        	                // a palindromic substring
        	                if (substrlen > longestPalindromicSubstr.length) {
        	                    // longer palindromic substring
        	                    longestPalindromicSubstr = s.substring(b, e + 1)
        	                }
        	            }
        	        }
        	    }
        	}


            longestPalindromicSubstr
        }
    }
}