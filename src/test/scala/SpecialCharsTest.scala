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

        val r = """^((?>z)[(),z]){1,}""".r
        val data = "(,)"
        val tree = new Tree[Char].buildHuffman(data)

        it("Regex 1") {
          assert(r.findFirstIn("z,(b,c)") == Some("z,"))
        }

        it("Regex 2") {
          assert(r.findFirstIn(",(b,c)") == None)
        }

        it("Regex 3") {
          assert(r.findFirstIn("((b,c)") == None)
        }

        it("Regex 4") {
          assert(r.findFirstIn(")(b,c)") == None)
        }

        it("Regex 5") {
          assert(r.findFirstIn("z)(b,c)") == Some("z)"))
        }

        it("Regex 6") {
          assert(r.findFirstIn("z((b,c)") == Some("z("))
        }

        it("Regex 7") {
          assert(r.findFirstIn("z(z((b,c)") == Some("z(z("))
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