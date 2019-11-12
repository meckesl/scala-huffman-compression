import java.math.BigInteger
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.must.Matchers

class CodecTest extends AnyFunSpec with Matchers {

  describe("Full UTF-8 document") {

    val tooling = new HuffmanTooling()
      .openFile("src/test/resources/UTF8_250kb.html")

    it("generates and saves codec") {
      tooling.generateCodec.saveCodec
    }

    it("outputs compressed") {
      tooling.encodeAndSave
    }

    it("reads back from compressed") {
      tooling.openCodec("src/test/resources/UTF8_250kb.html.codec")
      tooling.decodeAndSave
    }

  }

  describe("Docx document") {

    val tooling = new HuffmanTooling()
      .openFile("src/test/resources/MikeSlinnResume.docx")

    it("generates and saves codec") {
      tooling.generateCodec.saveCodec
    }

    it("outputs compressed") {
      tooling.encodeAndSave
    }

    it("reads back from compressed") {
      tooling.openCodec("src/test/resources/MikeSlinnResume.docx.codec")
      tooling.decodeAndSave
    }

  }

}