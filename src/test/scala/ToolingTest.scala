import java.math.BigInteger

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
              == "01110101000001011110011010001011101110011001011111010000011100101111000001010011011000110001111101100000101111001111010011011111010001001001000100110110011111010111110001101000101101110010110000101111101001101100000011110010100010"
          )
        }

        it("then decodes it") {
          assert(
            tooling.decode("01110101000001011110011010001011101110011001011111010000011100101111000001010011011000110001111101100000101111001111010011011111010001001001000100110110011111010111110001101000101101110010110000101111101001101100000011110010100010") ==
              "The little boy helps the man build a perfect encoder")
        }

      }
    }

    describe("English-1kb") {

      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        val tooling = new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt.codec")

        it("then encodes the file") {
          new HuffmanTooling[Char]()
            .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt.codec")
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          new HuffmanTooling[Char]()
            .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt.codec")
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_1kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-10kb -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_10kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_10kb.txt.codec")

        it("then encodes the file") {
          new HuffmanTooling[Char]()
            .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_10kb.txt.codec")
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_10kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          new HuffmanTooling[Char]()
            .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_10kb.txt.codec")
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_10kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-50kb -> Generates and stores Codec data") {

      val tooling = new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        tooling.openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt.codec")

        it("then encodes the file") {
          tooling
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          tooling
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-500kb -> Generates and stores Codec data") {

      val tooling = new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_500kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        tooling.openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_500kb.txt.codec")

        it("then encodes the file") {
          tooling
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_500kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          tooling
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_500kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("SpecialChars -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/special.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/special.txt.codec")

      }

    }

  }


}