package coursera.algorithms

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import coursera.algorithms.HuffmanCoding._

@RunWith(classOf[JUnitRunner])
class TestHuffmanCoding extends FunSuite {
  trait TestTree {
    val t1 = InnerNode(LeafNode('a',2), LeafNode('b',3), List('a','b'), 5)
  }

  test("weight of a tree is correct") {
    new TestTree {
      assert(weight(t1) === 5)
    }
  }

  test("correctly encode a very short text 'ab' using t1 tree") {
    new TestTree {
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }

}