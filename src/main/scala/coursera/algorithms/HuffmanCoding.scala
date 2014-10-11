package coursera.algorithms

/**
 * Implementing Huffman Coding using Scala
 */
object HuffmanCoding {
  
  type Bit = Int
  type HuffmanTable = List[(Char, List[Bit])]
  
  abstract class TreeNode
  case class InnerNode(left: TreeNode, right: TreeNode, chars: List[Char], weight: Int) extends TreeNode
  case class LeafNode(char: Char, weight: Int) extends TreeNode

  /**
   * Basic Functions
   */
  
  def weight(tree: TreeNode): Int = tree match {
    case LeafNode(char, weight) => weight
    case InnerNode(left, right, chars, weight) => weight
  }

  def chars(tree: TreeNode): List[Char] = tree match {
    case LeafNode(char, weight) => List(char)
    case InnerNode(left, right, chars, weight) => chars
  }

  def string2Chars(str: String): List[Char] = str.toList

  def calcFreqs(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => Nil
    case c :: Nil => List((c, 1))
    case h :: tail => { 
      val (l1, l2) = chars.partition(_ == h)
      (h, l1.length) :: calcFreqs(l2)
    }
  }
  
  def makeLeafNode(charFreq: (Char, Int)): LeafNode = LeafNode(charFreq._1, charFreq._2)
  
  def makeInnerNode(left: TreeNode, right: TreeNode) =
    InnerNode(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
    
   def insertIntoLeafNodeList(charFreq: (Char, Int), charFreqLst: List[LeafNode]): List[LeafNode] = charFreqLst match {
    case Nil => List(makeLeafNode(charFreq))
    case h :: tail => if (charFreq._2 < h.weight) makeLeafNode(charFreq) :: charFreqLst else h :: insertIntoLeafNodeList(charFreq, tail)
  }   
    
  def makeOrderedLeafNodeList(freqs: List[(Char, Int)]): List[LeafNode] = freqs match {
    case Nil => Nil
    case h :: tail => insertIntoLeafNodeList(h, makeOrderedLeafNodeList(tail))
  }
  
  def singleton(treeLst: List[TreeNode]): Boolean = treeLst match {
    case Nil => false
    case h :: tail => tail.isEmpty
  }
  
  def insertIntoOrderedNodeLst(treeNode: TreeNode, orderedNodeLst: List[TreeNode]): List[TreeNode] = orderedNodeLst match {
    case Nil => treeNode :: Nil
    case h :: tail => if (weight(treeNode) < weight(h)) treeNode :: h :: tail else h :: insertIntoOrderedNodeLst(treeNode, tail)
  }

  /**
   * This function takes the first two elements of the `orderedNodeList` and merge
   * them into a single `InnerNode`. This node is then added back into the remaining 
   * elements of `orderedNodeList` at a position such that the ordering by weights
   * is preserved.
   */
  def mergeNodes(orderedNodeLst: List[TreeNode]): List[TreeNode] = orderedNodeLst match {
    case Nil => Nil
    case h :: Nil => orderedNodeLst
    case h :: tail => {
      val combineFirstAndSecond = makeInnerNode(h, tail.head)
      insertIntoOrderedNodeLst(combineFirstAndSecond, tail.tail)
    }
  }

  def listToTree(singletonFunc: List[TreeNode] => Boolean, mergeFunc: List[TreeNode] => List[TreeNode])(trees: List[TreeNode]): List[TreeNode] = 
    if (singletonFunc(trees)) trees 
    else listToTree(singletonFunc, mergeFunc)(mergeFunc(trees))

  /**
   * This function creates a huffman tree which is optimal to encode the text `chars`.
   */
  def createHuffmanTree(chars: List[Char]): TreeNode = chars match {
    case Nil => throw new Error("Empty chars")
    case c :: Nil => LeafNode(c, 1)
    case c :: tail =>
      val orderedLeafNodeLst = makeOrderedLeafNodeList(calcFreqs(chars))  
      val codeTreeList = listToTree(singleton, mergeNodes)(orderedLeafNodeLst)
    
      codeTreeList.head
  } 
  
  /**
   * This function creates a Huffman Table from a given Huffman tree.
   */
  def createHuffmanTable(tree: TreeNode): HuffmanTable = {
    def walkHuffmanTree(tree: TreeNode, curr: TreeNode, path: List[Bit], table: HuffmanTable): HuffmanTable = curr match {
      case LeafNode(char, weight) => (char, path) :: table
      case InnerNode(left, right, chars, weight) => 
        val leftCodeTable = walkHuffmanTree(tree, left, path ::: List(0), table)
        walkHuffmanTree(tree, right, path ::: List(1), leftCodeTable)
    }
    
    walkHuffmanTree(tree, tree, List(), List())
  }

  /**
   * This function encode char to the bit sequence that represents the character `char` 
   * based on the huffman table `table`.
   */
  def charToBits(table: HuffmanTable)(char: Char): List[Bit] = {
    val filtered = table.filter(_._1 == char)
    filtered.head._2
  }

   /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def encode(tree: TreeNode)(text: List[Char]): List[Bit] = {
    def encodeWithHuffmanTable(table: HuffmanTable, text: List[Char]): List[Bit] = text match {
      case Nil => Nil
      case c :: tail => charToBits(table)(c) ::: encodeWithHuffmanTable(table, tail)
    }
    
    encodeWithHuffmanTable(createHuffmanTable(tree), text)
  }  
}