import java.math.BigInteger

import HuffmanCodec.Tree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ToolingTest extends AnyFunSpec with Matchers {

  describe("Huffman Tooling") {

    describe("Generates and stores Codec data") {
      new HuffmanTooling[Char]()
        .openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/chars_21b.txt")
        .generateCodec
        .saveCodec
        .encodeAndSave

      describe("Loads codec data") {
        new HuffmanTooling[Char]()
          .openCodec("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/chars_21b.txt.codec")
      }

    }


  }


}