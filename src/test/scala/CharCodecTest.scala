import java.math.BigInteger

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.must.Matchers

class CharCodecTest extends AnyFunSpec with Matchers {

  def loadTestFileRaw(f: String) = scala.io.Source.fromResource(f).toList
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolSeq(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList

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

      describe("Builds codec from serialized encoding map") {

        val serializedCodec = "(((r,d),( ,w)),(l,((h,e),o)))"
        val loaded = new HuffmanTree[Char].fromString(serializedCodec)

        it("ensures the codec tree serialization is identical") {
          assert(loaded.toString == serializedCodec)
          assert(loaded.toString == tree.toString)
        }

        it("encodes 'h' char (4 bits)") {
          val encoded = asBinaryDigits(loaded.encode('h'))
          assert(encoded == "1100");
        }

        it("decodes 'h' char") {
          val code = asBoolSeq("1100")
          assert(loaded.decode(code).contains('h'));
        }

        it("encodes 'hello world' char sequence") {
          val charSeq = "hello world"
          val encoded = asBinaryDigits(loaded.encodeSeq(charSeq))
          assert(encoded == "11001101101011101001111100010001");
        }

        it("decodes 'hello world' char sequence") {
          val data = asBoolSeq("11001101101011101001111100010001").toList
          assert(loaded.decodeSeq(data).mkString("") == "hello world");
        }

      }
    }

    describe("English 1kb text file") {

      val data = loadTestFileRaw("english_1kb.txt")
      val tree = new HuffmanTree[Char].build(data)

      it ("should create codec tree") {
        assert(tree.toString ==
          "((((h,s),(r,o)),(e,(n,((((((‘,E),6),((-,R),(W,H))),v),((((7,D)," +
            "(B,S)),T),(((1,:),(z,O)),((4,q),(3,j))))),(w,(((I,L),G),(0,(“,”)" +
            "))))))),(((i,((u,y),(l,m))),(a,t)),((((c,(((A,5),k),p)),d)," +
            "(((b,.),g),((\\,,((x,(F,C)),(’,M))),(f,\n)))), )))")
      }

        val sentence = "Hello I am the best string in the world of strings " +
          "and I am here to prove full sentences can be coded and decoded"

        val bits = "011100011101010011010011000111110111110001111010100111111101100" +
          "000101111101000010000110111110001101100101000011011010111110000110111101" +
          "100000101110111100011001010011011001111001111011101110001101100101000011" +
          "011010100011111010011011001111011111000111101010011111100000100010010111" +
          "101100111111100011001000110111001010111110111010010010011010011011100010" +
          "100110101101001101100000100001111110000101001101111101000010111110000001" +
          "1110010101100111110100110110011111100101011000000111100101011001"

        val bitsStdEncoding = new BigInteger(sentence.getBytes()).toString(2)

        it("should encode an english sentence") {
          val encoded = asBinaryDigits(tree.encodeSeq(sentence))
          assert(encoded == bits);
        }

        it("should decode an english sentence") {
          val data = asBoolSeq(bits).toList
          assert(tree.decodeSeq(data).mkString("") == sentence);
        }

        it(s"should have better coding compactness (${bits.length} bits) " +
          s"than standard encoding (${bitsStdEncoding.length} bits)") {
          assert(bits.length < (bitsStdEncoding.length / 1.5))
        }

      }

    describe("Special Characters") {

      val data = "&é\"'(§,è!çà)-"
      val tree = new HuffmanTree[Char].build(data.toCharArray.toList)

    }
}