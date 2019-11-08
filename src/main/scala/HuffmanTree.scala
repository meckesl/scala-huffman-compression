import scala.annotation.tailrec

case class EmptyNode[T]() extends HuffmanTree[T]
case class TreeNode[T](node: Option[T], weight: Option[Int],
                    left: HuffmanTree[T], right: HuffmanTree[T]) extends HuffmanTree[T]

  class HuffmanTree[T] {

    def build(xs: Seq[T]): HuffmanTree[T] = {

      def bu(xsu: Seq[TreeNode[T]]): TreeNode[T] = {
        val sortedXs = xsu.sortWith(_.weight.get < _.weight.get).toList
        sortedXs match {
          case head::Nil => head
          case head::tail =>
            sortedXs.take(2) match { case List(a, b) =>
              val weight = Some(a.weight.get + b.weight.get)
              val parent = TreeNode(None, weight, a, b)
              bu(parent +: sortedXs.drop(2))
            }
        }
      }

      bu {
        xs.distinct
          .map(e => (e, xs.count(_ equals e)))
          .map { case (node: T, weight: Int) =>
            TreeNode(Some(node), Some(weight), EmptyNode(), EmptyNode())
          }
      }

    }

    @throws(classOf[NoSuchElementException])
    def encode(data: T, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {
      this match {
        case TreeNode(n, w, l, r) =>
          if (n.contains(data)) acc else {
            try r.encode(data, acc :+ true) catch { case e: Exception =>
              try l.encode(data, acc :+ false) catch { case e: Exception =>
                throw e
              }
            }
          }
        case EmptyNode() =>
          throw new NoSuchElementException(data.toString)
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
    @throws(classOf[NoSuchElementException])
    final def decodeSeq(binary: List[Boolean], acc: Seq[T] = Seq(), root: HuffmanTree[T] = this) : Seq[T] = {
      this match {
        case TreeNode(n, w, l, r) if n.nonEmpty =>
            binary match {
              case x::xs =>  root.decodeSeq(binary, acc :+ n.get, root )
              case Nil => acc :+ n.get
            }
        case TreeNode(n, w, l, r) =>
            binary match {
              case h::tail if !h => l.decodeSeq(tail, acc, root)
              case h::tail if h => r.decodeSeq(tail, acc, root)
              case Nil => acc
            }
        case EmptyNode() =>
          throw new NoSuchElementException("Empty node should never be reached when decoding huffman code")
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