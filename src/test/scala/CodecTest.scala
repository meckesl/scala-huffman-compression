import java.math.BigInteger

import HuffmanCodec.Tree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.must.Matchers

class CodecTest extends AnyFunSpec with Matchers {

  def loadTestFileRaw(f: String) = scala.io.Source.fromResource(f).mkString
  def loadTestFileAlphanum(f: String) = scala.io.Source.fromResource(f).mkString.filter(_.toString.matches(Regex.alphanum))
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolSeq(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList

  describe("Huffman Codec") {

    describe("Should build with basic text data") {

      val tree = new Tree[Char].buildHuffman("hello world")

      it("should create encoding map") {
        assert(tree.toString == "((((e,h),o),l),((d,r),(w, )))")
      }

      it("should encode 'h' char on 4 bits") {
        val encoded = asBinaryDigits(tree.encode('h'))
        assert(encoded == "0001");
      }

      it("should encode 'l' char on 2 bits") {
        val encoded = asBinaryDigits(tree.encode('l'))
        assert(encoded == "01");
      }

      it("should decode 'h' char") {
        val code = asBoolSeq("0001")
        assert(tree.decode(code).contains('h'));
      }

      it("should encode 'hello world' char sequence") {
        val charSeq = "hello world"
        val encoded = asBinaryDigits(tree.encodeSeq(charSeq))
        assert(encoded == "00010000010100111111000110101100");
      }

      it("should decode 'hello world' char sequence") {
        val data = asBoolSeq("00010000010100111111000110101100")
        assert(tree.decodeSeq(data).mkString("") == "hello world");
      }
    }

    describe("should build with english sample text") {

      val data = loadTestFileAlphanum("englishSample.txt")
      val tree = new Tree[Char].buildHuffman(data)

      it("should create encoding map") {
        assert(tree.toString.equals(
          "((( ,((d,((b,((A,L),k)),c)),t)),(((((p,(0,G)),m),(l,y)),a),(((u,w)," +
            "(((((7,S),(B,q)),((4,j),(3,1))),(((O,z),(R,H)),((W,E),I)))," +
            "(((C,F),M),v))),i))),(((n,o),e),((r,s)," +
            "(h,((f,((T,(6,D)),(x,5))),g)))))"))
      }

      describe("full english sentence") {

        val sentence = "Hello I am the best string in the world of strings " +
          "and I am here to prove full sentences can be coded and decoded"

        val bits = "01101010111010100100100101001000011010111000010101000100" +
          "0001111101010000010100101110100110001101001111000111100011111000011110000000" +
          "01111101010000110011001110001001000100000100111110000011010011110001111000111111" +
          "101000010110000010000001101011100001010100010001110101110010100000111001000010" +
          "00001100100101101111010001111000110000100100100100001101101100000111011000001011" +
          "101110100000101101011000000001010010100000101110010010010100100000010110000" +
          "01000000010010100101110010010010100100"

        val bitsStdEncoding = new BigInteger(sentence.getBytes()).toString(2)

        it("should encode") {
          val encoded = asBinaryDigits(tree.encodeSeq(sentence))
          assert(encoded == bits);
        }

        it("should decode") {
          val data = asBoolSeq(bits)
          assert(tree.decodeSeq(data).mkString("") == sentence);
        }

        it(s"should have better coding compactness (${bits.length} bits) compared " +
          s"to standard encoding (${bitsStdEncoding.length} bits)") {
          assert(bits.length < (bitsStdEncoding.length / 1.5))
        }

      }

    }

  }


}