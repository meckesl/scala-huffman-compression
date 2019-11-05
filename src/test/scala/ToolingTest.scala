import java.math.BigInteger

import HuffmanCodec.Tree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ToolingTest extends AnyFunSpec with Matchers {

  describe("Huffman Tooling") {

    describe("English-1kb -> Generates and stores Codec data") {

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
              == "000001010111111000101010110000100010001010111000100" +
              "001111010010111001111111001010100011011110001010011111" +
              "100010101000110100100100001110111001000010101000100010" +
              "110001000110111010110000001110000111010000111010010001" +
              "111010000101101011"
          )
        }

        it("then decodes it") {
          assert(
            tooling.decode("00000101011111100010101011000010001" +
              "0001010111000100001111010010111001111111001010100011011" +
              "11000101001111110001010100011010010010000111011100100001" +
              "01010001000101100010001101110101100000011100001110100001" +
              "11010010001111010000101101011") ==
              "The little boy helps the man build a perfect encoder")
        }

      }
    }

    describe("English-50kb -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        val tooling = new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt.codec")

        it("then encodes the file") {
          new HuffmanTooling[Char]()
            .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt.codec")
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt")
            .encodeAndSave
        }

      }
    }

    describe("SpecialChars -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/special.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        val tooling = new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/special.txt.codec")

      }

    }


    /*describe("UTF8 -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/html_UTF8_250kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        val tooling = new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/html_UTF8_250kb.txt.codec")
          .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/html_UTF8_250kb.txt")
          .encodeAndSave
      }

    }*/

  }


}