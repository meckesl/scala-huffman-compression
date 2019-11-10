import scala.annotation.tailrec
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache._

import scala.collection.immutable.Queue

case class EmptyNode[T]() extends HuffmanTree[T]
case class TreeNode[T](node: Option[T], weight: Option[Int],
                    left: HuffmanTree[T], right: HuffmanTree[T]) extends HuffmanTree[T]

  class HuffmanTree[T] {

    def build(xs: List[T]): HuffmanTree[T] = {

      @tailrec
      def huffmanAlgorithm(xs: Seq[TreeNode[T]]): TreeNode[T] = {
          xs.sortWith(_.weight.get < _.weight.get) match {
          case head::Nil => head
          case head::tail =>
            (head, tail.head) match { case (a, b) =>
              val weight = Some(a.weight.get + b.weight.get)
              val parent = TreeNode(None, weight, a, b)
              huffmanAlgorithm(parent +: tail.tail)
            }
        }
      }

      huffmanAlgorithm {
        xs.distinct
          .map(e => (e, xs.count(_ equals e)))
          .map { case (node: T, weight: Int) =>
            TreeNode(Some(node), Some(weight), EmptyNode(), EmptyNode())
          }
      }

    }

    val cache = Caffeine.newBuilder.build[T, Entry[Seq[Boolean]]]

    @throws(classOf[NoSuchElementException])
    def encode(data: T, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {

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
              throw new NoSuchElementException(data.toString)
          }
          cache.put(data, Entry(bits, expiresAt = None))
          bits
        }
        case v => v.value
      }

    }

    def encodeSeq(data: Seq[T]): Seq[Boolean] = data.flatMap(encode(_))

    @tailrec
    final def decode(data: Seq[Boolean]): Option[T] = {
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
    final def decodeSeq(binary: List[Boolean], acc: Queue[T] = Queue(), root: HuffmanTree[T] = this) : Seq[T] = {
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
          Seq[T]()
      }
    }

    override def toString: String = {

      def esc(data: Option[T]) : String = {
        data match {
          case Some(data) => data.toString.replaceAll("([(|)|,|\\\\])","\\\\$1")
          case None => ""
        }
      }

      this match {
        case TreeNode(n, w, l, r) =>
          (l, r) match {
            case (EmptyNode(), EmptyNode()) => esc(n)
            case _ => s"${esc(n)}($l,$r)"
        }
        case _ => ""
      }

    }

    def fromString(s: String): HuffmanTree[T] = {
      import scala.util.parsing.combinator.RegexParsers
      object TreeParser extends RegexParsers {
        override def skipWhitespace = false
        def node: Parser[T] = """(\\\\|[^(),\\]|\\,|\\\(|\\\))""".r ^^ {
          res => (if (res.startsWith("\\")) res.drop(1) else res).charAt(0).asInstanceOf[T]
        }
        def subtrees: Parser[(HuffmanTree[T], HuffmanTree[T])] = "(" ~ tree.? ~ "," ~ tree.? ~ ")" ^^ {
          case (start ~ left ~ comma ~ right ~ stop) => (left.getOrElse(EmptyNode()), right.getOrElse(EmptyNode()))
        }
        def tree: Parser[HuffmanTree[T]] = (node ~ subtrees | (subtrees | node)) ^^ {
          case (n: T) ~ ((l: HuffmanTree[T], r: HuffmanTree[T])) =>
             TreeNode(Some(n), None, l, r)
          case (l: HuffmanTree[T], r: HuffmanTree[T]) =>
             TreeNode(None, None, l, r)
          case n: T =>
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