import java.math.BigInteger

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ToolingTest extends AnyFunSpec with Matchers {

  describe("Huffman Tooling") {

    describe("English-1kb") {

      new HuffmanTooling[Char]()
        .openFile("src/test/resources/english_1kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        val tooling = new HuffmanTooling[Char]()
          .openCodec("src/test/resources/english_1kb.txt.codec")

        it("then encodes the file") {
          new HuffmanTooling[Char]()
            .openCodec("src/test/resources/english_1kb.txt.codec")
            .openFile("src/test/resources/english_1kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          new HuffmanTooling[Char]()
            .openCodec("src/test/resources/english_1kb.txt.codec")
            .openFile("src/test/resources/english_1kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-10kb -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("src/test/resources/english_10kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        new HuffmanTooling[Char]()
          .openCodec("src/test/resources/english_10kb.txt.codec")

        it("then encodes the file") {
          new HuffmanTooling[Char]()
            .openCodec("src/test/resources/english_10kb.txt.codec")
            .openFile("src/test/resources/english_10kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          new HuffmanTooling[Char]()
            .openCodec("src/test/resources/english_10kb.txt.codec")
            .openFile("src/test/resources/english_10kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-50kb -> Generates and stores Codec data") {

      val tooling = new HuffmanTooling[Char]()
        .openFile("src/test/resources/english_50kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        tooling.openCodec("src/test/resources/english_50kb.txt.codec")

        it("then encodes the file") {
          tooling
            .openFile("src/test/resources/english_50kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          tooling
            .openFile("src/test/resources/english_50kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-500kb -> Generates and stores Codec data") {

      val tooling = new HuffmanTooling[Char]()
        .openFile("src/test/resources/english_500kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        tooling.openCodec("src/test/resources/english_500kb.txt.codec")

        it("then encodes the file") {
          tooling
            .openFile("src/test/resources/english_500kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          tooling
            .openFile("src/test/resources/english_500kb.txt")
            .decodeAndSave
        }

      }
    }

    describe("English-900kb -> Generates and stores Codec data") {

      val tooling = new HuffmanTooling[Char]()
        .openFile("src/test/resources/english_900kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        tooling.openCodec("src/test/resources/english_900kb.txt.codec")

        it("then encodes the file") {
          tooling
            .openFile("src/test/resources/english_900kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          tooling
            .openFile("src/test/resources/english_900kb.txt")
            .decodeAndSave
        }

      }
    }

    /*describe("Unicode-250kb -> Generates and stores Codec data") {

      val tooling = new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/UTF8_250kb.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        tooling.openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/UTF8_250kb.txt.codec")

        it("then encodes the file") {
          tooling
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/UTF8_250kb.txt")
            .encodeAndSave
        }

        it("then decodes a file") {
          tooling
            .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/UTF8_250kb.txt")
            .decodeAndSave
        }

      }
    }*/

    describe("SpecialChars -> Generates and stores Codec data") {

      new HuffmanTooling[Char]()
        .openFile("src/test/resources/special.txt")
        .generateCodec
        .saveCodec

      describe("Loads codec data") {
        new HuffmanTooling[Char]()
          .openCodec("src/test/resources/special.txt.codec")

      }

    }

  }


}