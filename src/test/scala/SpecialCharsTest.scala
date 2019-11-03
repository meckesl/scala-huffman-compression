import java.math.BigInteger

import HuffmanCodec.Tree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class SpecialCharsTest extends AnyFunSpec with Matchers {

  def loadTestFileRaw(f: String) = scala.io.Source.fromResource(f).mkString
  def loadTestFileAlphanum(f: String) = scala.io.Source.fromResource(f).mkString.filter(_.toString.matches(Regex.alphanum))
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolSeq(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList

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
          assert(r.findFirstIn("((b,c)") == Some("b"))
        }

        it("Regex 4") {
          assert(r.findFirstIn(")(b,c)") == Some("b"))
        }

        it("Regex 5") {
          assert(r.findFirstIn("\\)(b,c)") == Some("\\)"))
        }

        it("Regex 6") {
          assert(r.findFirstIn("\\((b,c)") == Some("\\("))
        }

        it("Regex 7") {
          assert(r.findFirstIn("\\(\\((b,c)") == Some("\\(\\("))
        }

        it("Regex 8") {
          assert(r.findFirstIn("\\(\\,(b,c)") == Some("\\(\\,"))
        }

        it("Regex 9") {
          assert(r.findFirstIn("c,") == Some("c"))
        }

        /*it("Should serialize") {
          assert(tree.toString ==
            "((\\,,\\(),\\))")
        }

        it("Should parse from serialized") {
          val loaded = new Tree[Char].fromString("((\\,,\\(),\\))")
          assert(loaded.toString == "((\\,,\\(),\\))")
        }*/

      }


}