import com.lms.comp.HuffmanTooling
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class StratTest extends AnyFunSpec with Matchers {

  describe("UTF-8 250kb") {

    val tooling = new HuffmanTooling()
      .openFile("src/test/resources/UTF8_250kb.html")

    it("generates a codec") {
      tooling.generateCodec
    }

    it("outputs codec stats") {
      tooling.getCodecStats
    }

  }

}