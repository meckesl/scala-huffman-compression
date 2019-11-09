import java.math.BigInteger

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class RegexTest extends AnyFunSpec with Matchers {

    describe("Structure Characters") {

        //https://regex101.com/r/sS2dM8/35
        val r = """(\\\\|[^(),\\]|\\,|\\\(|\\\))""".r

        it("Regex 0") {
          assert(r.findAllIn("a(b,c)").toList == List("a", "b", "c"))
        }

        it("Regex 1") {
          assert(r.findAllIn("\\,(b,c)").toList == List("\\,", "b", "c"))
        }

        it("Regex 2") {
          assert(r.findAllIn(",(b,c)").toList == List("b", "c"))
        }

        it("Regex 3") {
          assert(r.findFirstIn("((b,c)").contains("b"))
        }

        it("Regex 4") {
          assert(r.findFirstIn(")(b,c)").contains("b"))
        }

        it("Regex 5") {
          assert(r.findFirstIn("\\)(b,c)").contains("\\)"))
        }

        it("Regex 6") {
          assert(r.findFirstIn("\\((b,c)").contains("\\("))
        }

        it("Regex 7") {
          assert(r.findFirstIn("c,").contains("c"))
        }

      }


}