import java.math.BigInteger

import HuffmanCodec.Tree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ToolingTest extends AnyFunSpec with Matchers {

  describe("Huffman Tooling") {

    describe("Generates and stores Codec data") {
      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        val tooling = new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt.codec")

        it("then encodes some text") {
          assert(
            tooling.encode(
              "The little boy helps the man build a perfect encoder")
              == "0010001101111101000011000011100110011011000101000001010110" +
              "01011010000111110101100000101111110000001111111010000100110101" +
              "10000000010101011011011101100001000000010100000101111011100001" +
              "0000101010010001100010110000100101001010001011100"
          )
        }

        it("then decodes it") {
          assert(
            tooling.decode("0010001101111101000011000011100110011011000101000001010110" +
              "01011010000111110101100000101111110000001111111010000100110101" +
              "10000000010101011011011101100001000000010100000101111011100001" +
              "0000101010010001100010110000100101001010001011100") ==
              "The little boy helps the man build a perfect encoder")
        }

      }

    }


  }


}