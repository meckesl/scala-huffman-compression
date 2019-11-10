import java.math.BigInteger

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.must.Matchers

class ByteCodecTest extends AnyFunSpec with Matchers {

  def loadTestFileRaw(f: String) = scala.io.Source.fromResource(f).toList
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolSeq(binary: String): Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList

  describe("Basic 'hello world' String") {

    val tree = new HuffmanTree[Char].build("hello world".toCharArray.toList)

    it("creates encoding map") {
      assert(tree.toString == "(((r,d),( ,w)),(l,((h,e),o)))")
    }

    it("encodes 'h' char (4 bits)") {
      val encoded = asBinaryDigits(tree.encode('h'))
      assert(encoded == "1100");
    }

    it("encodes 'l' char (2 bits)") {
      val encoded = asBinaryDigits(tree.encode('l'))
      assert(encoded == "10");
    }

    it("encodes ' ' char (3 bits)") {
      val encoded = asBinaryDigits(tree.encode(' '))
      assert(encoded == "010");
    }

    it("decodes 'h' char") {
      val code = asBoolSeq("1100")
      assert(tree.decode(code).contains('h'));
    }

    it("decodes 'l' char") {
      val code = asBoolSeq("10")
      assert(tree.decode(code).contains('l'));
    }

    it("decodes ' ' char") {
      val code = asBoolSeq("010")
      assert(tree.decode(code).contains(' '));
    }

    it("encodes 'hello world' char sequence") {
      val charSeq = "hello world"
      val encoded = asBinaryDigits(tree.encodeSeq(charSeq))
      assert(encoded == "11001101101011101001111100010001");
    }

    it("decodes 'hello world' char sequence") {
      val data = asBoolSeq("11001101101011101001111100010001").toList
      assert(tree.decodeSeq(data).mkString("") == "hello world");
    }
  }
}