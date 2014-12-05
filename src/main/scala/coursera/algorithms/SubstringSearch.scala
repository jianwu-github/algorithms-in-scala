package coursera.algorithms

import util.control.Breaks._

object SubstringSearch {
    val DEFAULT_RADIX = 256 // ASCII String 
  
    /**
	 * Brute-force Substring Search for pattern from text
	 *
	 * @param pattern 
	 * @param text
	 * @return start position of the pattern string found in text, 
	 *         -1 indicates no pattern found  
	 */
    def naiveSearch(pattern: String, text: String): Int = {
        val patternLength = pattern.length()
        val textLength = text.length()
      
        def scanPattern(textPos: Int, patternPos: Int): Int = 
            if (patternPos == patternLength) textPos - patternLength
            else if (text.charAt(textPos) != pattern.charAt(patternPos)) -1
            else scanPattern(textPos + 1, patternPos + 1)
      
        def scan(textPos: Int): Int = 
            if (textPos == (textLength - patternLength)) -1 
            else {
                val result = scanPattern(textPos, 0)
                if (result != -1) result // found 
                else scan(textPos + 1)
            }
        
        if (patternLength > textLength) -1
        else scan(0)
    }
    
    /**
	 * Substring Search using Knuth-Morris-Pratt Algorithm for pattern from text
	 *
	 * @param pattern 
	 * @param text
	 * @return start position of the pattern string found in text, 
	 *         -1 indicates no pattern found  
	 */
    def searchWithKMP(pattern: String, text: String): Int = {
        val patternLength = pattern.length()
        val textLength = text.length()
        
        def createDFA: Array[Array[Int]] = {
            val dfa = Array.fill[Int](DEFAULT_RADIX, patternLength)(0)
            dfa(pattern.charAt(0))(0) = 1
            
            var X = 0
            for (j <- 1 until patternLength) {
                // Copy mismatch cases
                for (c <- 0 until DEFAULT_RADIX) {
                    dfa(c)(j) = dfa(c)(X)
                }
                
                // Set match case
                dfa(pattern.charAt(j))(j) = j + 1
                
                // Update restart state
                X = dfa(pattern.charAt(j))(X) 
            }
            
            dfa
        } 
        
        if (patternLength > textLength) -1
        else {
        	val dfa = createDFA
        	
        	var patternPos = 0
        	var textPos = 0
        	while (textPos < textLength && patternPos < patternLength) {
        	    patternPos = dfa(text.charAt(textPos))(patternPos)
        	    textPos += 1
        	}
        	
        	if (patternPos == patternLength)  return textPos - patternLength // found pattern
        	else -1 // pattern not found
        }
    }
    
    /**
	 * Substring Search using Boyer-Moore Algorithm for pattern from text
	 *
	 * @param pattern 
	 * @param text
	 * @return start position of the pattern string found in text, 
	 */  
    def searchWithBM(pattern: String, text: String): Int = {
        val patternLength = pattern.length()
        val textLength = text.length()
        
        def computerRightmostOccurrence: Array[Int] = {
            val rightmostOccurrence = Array.fill[Int](DEFAULT_RADIX)(-1) // Init with -1 for char not in pattern
            for (j <- 0 until patternLength) {
                // Set rightmost position for chars in pattern
                rightmostOccurrence(pattern.charAt(j)) = j
            }
            
            rightmostOccurrence
        }
        
        if (patternLength > textLength) -1
        else {
            val rightmostOccurrence = computerRightmostOccurrence
            
            var skip = 0
            var textPos = 0
            
            
            while (textPos <= textLength - patternLength) {
                skip = 0
                
                breakable {
                    for (patternPos <- patternLength - 1 to 0 by -1) { 
                        if (pattern.charAt(patternPos) != text.charAt(textPos + patternPos)) {
							// mismatch, decide how many characters can be skipped based on
							// mismatched character heuristic
							skip = Math.max(1, patternPos - rightmostOccurrence(text.charAt(textPos + patternPos)))
							break
					    }
                    }
                }
                
                if (skip == 0) return textPos // found pattern
                else textPos += skip
            }
            
            -1 // pattern not found
        }
    }

}