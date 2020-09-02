package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(
      Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5),
      Leaf('d', 4),
      List('a', 'b', 'd'),
      9
    )
  }
  val hello: List[Char] =
    List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
  val hellotree: CodeTree = Fork(
    Fork(
      Leaf('o', 2),
      Fork(
        Leaf('h', 1),
        Fork(Leaf(',', 1), Leaf('e', 1), List(',', 'e'), 2),
        List('h', ',', 'e'),
        3
      ),
      List('o', 'h', ',', 'e'),
      5
    ),
    Fork(
      Leaf('l', 3),
      Fork(
        Fork(Leaf('w', 1), Leaf(' ', 1), List('w', ' '), 2),
        Fork(Leaf('d', 1), Leaf('r', 1), List('d', 'r'), 2),
        List('w', ' ', 'd', 'r'),
        4
      ),
      List('l', 'w', ' ', 'd', 'r'),
      7
    ),
    List('o', 'h', ',', 'e', 'l', 'w', ' ', 'd', 'r'),
    12
  )
  @Test def `weight of a larger tree (10pts)` : Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }

  @Test def `chars of a larger tree (10pts)` : Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(
      List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'),
      string2Chars("hello, world")
    )

  @Test def `make ordered leaf list for some frequency table (15pts)` : Unit =
    assertEquals(
      List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)),
      makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
    )

  /** Corrected test to fully check if combine works as intended* */
  @Test def `combine of some leaf list (15pts)` : Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 5))
    assertEquals(
      List(
        Fork(
          Leaf('a', 5),
          Fork(
            Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),
            Leaf('x', 4),
            List('e', 't', 'x'),
            7
          ),
          List('a', 'e', 't', 'x'),
          12
        )
      ),
      combine(combine(combine(leaflist)))
    )
  }

  /** Missing test for until * */
  @Test def `until of some leaf list`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(
      List(
        Fork(
          Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),
          Leaf('x', 4),
          List('e', 't', 'x'),
          7
        )
      ),
      until(singleton, combine)(leaflist)
    )
  }

  @Test def `NEW! create tree 'hello, world'` : Unit = {
    assertEquals(hellotree, createCodeTree(hello))
  }
  @Test def `NEW! create codeTree`: Unit = {
    println(convert(frenchCode))
  }
  @Test def `NEW! decode frenchCode with secret`: Unit = {
    val huffman =
      List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    assertEquals(huffman, decodedSecret)
  }

  @Test def `NEW! encode text with frenchCode`: Unit = {
    val huffman =
      List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    assertEquals(secret, quickEncode(frenchCode)(huffman))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`
      : Unit = {

    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
