import java.util.Base64

import scala.annotation.tailrec
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache._

import scala.collection.immutable.Queue

case class EmptyNode() extends HuffmanTree
case class TreeNode(node: Option[Byte], weight: Option[Int],
                    left: HuffmanTree, right: HuffmanTree) extends HuffmanTree

  class HuffmanTree() {

    type HData = Byte

    def build(xs: Array[Byte]): HuffmanTree = {
      @tailrec
      def huffmanAlgorithm(xs: Seq[TreeNode]): TreeNode = {
          xs.sortWith(_.weight.get < _.weight.get) match {
          case head::Nil => head
          case a::b::tail =>
              val weight = Some(a.weight.get + b.weight.get)
              val parent = TreeNode(None, weight, a, b)
              huffmanAlgorithm(parent +: tail)
        }
      }
      huffmanAlgorithm {
        xs.toList
          .distinct
          .map(e => (e, xs.count(_ equals e)))
          .map { case (node: HData, weight: Int) =>
            TreeNode(Some(node), Some(weight), EmptyNode(), EmptyNode())
          }
      }
    }

    val cache = Caffeine.newBuilder.build[HData, Entry[Seq[Boolean]]]

    @throws(classOf[NoSuchElementException])
    def encode(data: HData, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {

      cache.getIfPresent(data) match {
        case null => {
          val bits = this match {
            case TreeNode(n, w, l, r) =>
              if (n.contains(data)) acc else {
                try r.encode(data, acc :+ true) catch { case e: NoSuchElementException =>
                  try l.encode(data, acc :+ false) catch { case e: NoSuchElementException =>
                    throw e
                  }
                }
              }
            case EmptyNode() =>
              throw new NoSuchElementException
          }
          cache.put(data, Entry(bits, expiresAt = None))
          bits
        }
        case v => v.value
      }

    }

    def encodeSeq(data: Array[HData]): Seq[Boolean] = data.flatMap(encode(_))

    @tailrec
    final def decode(data: Seq[Boolean]): Option[HData] = {
      this match {
        case TreeNode(n, w, l, r) => {
          data match {
            case Nil => n
            case h :: tail =>
              if (h) r.decode(tail) else l.decode(tail)
          }
        }
        case EmptyNode() => None
      }
    }

    @tailrec
    final def decodeSeq(binary: List[Boolean], acc: Queue[HData] = Queue(), root: HuffmanTree = this) : Seq[HData] = {
      this match {
        case TreeNode(n, w, l, r) if n.nonEmpty =>
          binary match {
            case x::xs =>  root.decodeSeq(binary, acc:+n.get, root)
            case Nil => acc:+n.get
          }
        case TreeNode(n, w, l, r) =>
          binary match {
            case x::xs if x => r.decodeSeq(xs, acc, root)
            case x::xs => l.decodeSeq(xs, acc, root)
            case Nil => acc
          }
        case EmptyNode() =>
          Seq[HData]()
      }
    }

    override def toString: String = {

      def b64(data: Option[HData]) : String = {
        data match {
          case Some(data) => Base64.getEncoder.encodeToString(Array(data)) // Base64 encode
          case None => ""
        }
      }

      this match {
        case TreeNode(n, w, l, r) =>
          (l, r) match {
            case (EmptyNode(), EmptyNode()) => b64(n)
            case _ => s"${b64(n)}($l,$r)"
        }
        case _ => ""
      }

    }

    def fromString(s: String): HuffmanTree = {
      import scala.util.parsing.combinator.RegexParsers
      object TreeParser extends RegexParsers {
        override def skipWhitespace = false
        //def node: Parser[A] = """(\\\\|[^(),\\]|\\,|\\\(|\\\))""".r ^^ {
        //  res => (if (res.startsWith("\\")) res.drop(1) else res).charAt(0).asInstanceOf[A]
        //}
        def node: Parser[HData] = """([^(),]{1,})""".r ^^ {
          res => Base64.getDecoder.decode(res).head
        }
        def subtrees: Parser[(HuffmanTree, HuffmanTree)] = "(" ~ tree.? ~ "," ~ tree.? ~ ")" ^^ {
          case (start ~ left ~ comma ~ right ~ stop) => (left.getOrElse(EmptyNode()), right.getOrElse(EmptyNode()))
        }
        def tree: Parser[HuffmanTree] = (node ~ subtrees | (subtrees | node)) ^^ {
          case (n: HData) ~ ((l: HuffmanTree, r: HuffmanTree)) =>
             TreeNode(Some(n), None, l, r)
          case (l: HuffmanTree, r: HuffmanTree) =>
             TreeNode(None, None, l, r)
          case n: HData =>
             TreeNode(Some(n), None, EmptyNode(), EmptyNode())
        }
        def apply(input: String) = parseAll(tree, input) match {
          case Success(result, _) => result
          case failure: NoSuccess => scala.sys.error(failure.msg)
        }
      }
      TreeParser.apply(s)
    }

  }