import java.math.BigInteger

import HuffmanCodec.Tree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ToolingTest extends AnyFunSpec with Matchers {

  def loadTestFileRaw(f: String) = scala.io.Source.fromResource(f).mkString
  def loadTestFileAlphanum(f: String) = scala.io.Source.fromResource(f).mkString.filter(_.toString.matches(Regex.alphanum))
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolSeq(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList

  describe("Huffman Tooling") {

    describe("Should generate and store Codec data") {

      val tooling = new HuffmanTooling[Char]();
      tooling.openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/pouet.txt")
      tooling.generateCodec
      tooling.saveCodec
      tooling.encodeAndSave

    }

  }


}