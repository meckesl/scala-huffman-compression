import java.math.BigInteger

import scala.annotation.tailrec

object HuffmanCodec {

  class EmptyTree[T] extends Tree[T]

  case class NonEmptyTree[T](node: Option[T], weight: Option[Int], left: Tree[T], right: Tree[T]) extends Tree[T]

  class Tree[T] {

    def buildBalanced(xs: Seq[T]): Tree[T] = {

      def build(xs: Seq[T]): Tree[T] = {
        xs match {
          case Nil => new EmptyTree
          case _ => {
            val branch = xs.tail.partition(xs.indexOf(_) % 2 == 0)
            NonEmptyTree(Some(xs.head), None, build(branch._2), build(branch._1))
          }
        }
      }

      build(
        xs
          .map(e => (e, xs.count(_ equals e)))
          .sortWith(_._2 > _._2)
          .distinct
          .map(_._1)
      )

    }

    def buildHuffman(xs: Seq[T]): Tree[T] = {

      def build(xsu: Seq[NonEmptyTree[T]]): Tree[T] = {
        val xs = xsu.sortWith(_.weight.getOrElse(0) < _.weight.getOrElse(0))
        xs.size match {
          case 1 => xs.head
          case _ => {
            val tuple = xs.take(2)
            val weightSum = tuple(0).weight.getOrElse(0) + tuple(1).weight.getOrElse(0)
            val parent = NonEmptyTree(None, Some(weightSum), tuple(1), tuple(0))
            build(xs.drop(2) :+ parent)
          }
        }
      }

      build(
        xs
          .distinct
          .map(e => (e, xs.count(_ equals e)))
          .map({ case (node: T, weight: Int) =>
            NonEmptyTree(Some(node), Some(weight), new EmptyTree, new EmptyTree) })
      )

    }

    @throws(classOf[NoSuchElementException])
    def encode(data: T, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {
      this match {
        case tr: NonEmptyTree[T] if tr.node.contains(data) =>
            acc
        case tr: NonEmptyTree[T] =>
            try tr.right.encode(data, acc :+ true) catch {
              case e: NoSuchElementException =>
                try tr.left.encode(data, acc :+ false) catch {
                  case e: NoSuchElementException =>
                    throw new NoSuchElementException(data.toString)
                }
            }
        case _ => throw new NoSuchElementException
      }
    }

    def encodeSeq(data: Seq[T]): Seq[Boolean] = {
        data.flatMap(encode(_))
    }

    def decode(data: Seq[Boolean]): Option[T] = {
      this match {
        case tree: NonEmptyTree[T] =>
          data match {
            case Nil => tree.node
            case h :: tail =>
              if (h) tree.right.decode(tail) else tree.left.decode(tail)
          }
        case _ => None
      }
    }

    @tailrec
    final def decodeSeq(binary: List[Boolean], root: Tree[T] = this, acc: Seq[T] = Seq()) : Seq[T] = {
      this match {
        case tree : NonEmptyTree[T] if (tree.node.nonEmpty) =>
            binary match {
              case Nil => acc :+ tree.node.get
              case _ =>  root.decodeSeq(binary, root, acc :+ tree.node.get)
            }
        case tree : NonEmptyTree[T] =>
            binary match {
              case Nil => acc
              case h::tail if !h => tree.left.decodeSeq(tail, root, acc)
              case h::tail if h => tree.right.decodeSeq(tail, root, acc)
            }
        case _ => Seq[T]()
      }
    }

    override def toString: String = {

      def esc(data: Option[T]) : String = {
        data match {
          case Some(data) =>
            data.toString
              .replaceAll("([(|)|,|\\\\])","\\\\$1")
          case None => ""
        }
      }

      this match {
        case t: NonEmptyTree[T] =>
          (t.right, t.left) match {
            case (l, r) if (l.isInstanceOf[EmptyTree[T]] && r.isInstanceOf[EmptyTree[T]]) =>
              esc(t.node)
            case _ =>
              s"${esc(t.node)}(${t.left},${t.right})"
          }
        case _ => ""
      }
    }

    def fromString(s: String): Tree[T] = {
      import scala.util.parsing.combinator.RegexParsers
      object TreeParser extends RegexParsers {
        override def skipWhitespace = false
        // (?<!Y)X matches an X that is not preceded by a Y
        def node: Parser[T] = """(\\\\|[^(),\\]|\\,|\\\(|\\\))""".r ^^ {
          res => (if (res.startsWith("\\")) res.drop(1) else res).charAt(0).asInstanceOf[T]
        }
        def subtrees: Parser[(Tree[T], Tree[T])] = "(" ~ tree.? ~ "," ~ tree.? ~ ")" ^^ {
          case (start ~ left ~ comma ~ right ~ stop) =>
            (left.getOrElse(new EmptyTree[T]), right.getOrElse(new EmptyTree[T]))
        }
        def tree: Parser[Tree[T]] = (node ~ subtrees | (subtrees | node)) ^^ {
          case ((n: T) ~ ((l: Tree[T], r: Tree[T]))) => new NonEmptyTree[T](Some(n), None, l, r)
          case (l: Tree[T], r: Tree[T]) => new NonEmptyTree[T](None, None, l, r)
          case n: T => new NonEmptyTree[T](Some(n), None, new EmptyTree[T], new EmptyTree[T])
          case _ => new EmptyTree[T]
        }
        def apply(input: String) = parseAll(tree, input) match {
          case Success(result, _) => result
          case failure: NoSuccess => scala.sys.error(failure.msg)
        }
      }
      TreeParser.apply(s)
    }

    def toHTML: String = {
      this match {
        case t: NonEmptyTree[T] => {
          s"""
          <div style='width: 100%;'>${t.node}<br/>
          <div style='float:left; width: 50%;'>${t.left.toHTML}</div>
          <div style='float:right; width: 50%;'>${t.right.toHTML}</div>
          </div>
          """
        }
        case _ => ""
      }
    }

  }

}