import java.math.BigInteger

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
          .map(e => (e, xs.count(_ equals e)))
          .distinct
          .map({ case (node: T, weight: Int) =>
            NonEmptyTree(Some(node), Some(weight), new EmptyTree, new EmptyTree) })
      )

    }

    @throws(classOf[NoSuchElementException])
    def encode(data: T, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {
      this match {
        case tr: NonEmptyTree[T] =>
          if (tr.node.contains(data)) {
            acc
          } else {
            try tr.left.encode(data, acc :+ false) catch {
              case e: NoSuchElementException =>
                try tr.right.encode(data, acc :+ true) catch {
                  case e: NoSuchElementException =>
                    throw new NoSuchElementException(data.toString)
                }
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

    def decodeSeq(binary: Seq[Boolean], root: Tree[T] = this) : Seq[T] = {
      this match {
        case tree : NonEmptyTree[T] =>
          if (tree.node.nonEmpty) {
            binary.size match {
              case 0 => Seq[T](tree.node.get)
              case _ => tree.node.get +: root.decodeSeq(binary)
            }
          } else {
            if (!binary.head)
              tree.left.decodeSeq(binary.tail, root)
            else
              tree.right.decodeSeq(binary.tail, root)
          }
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
        //def node: Parser[T] = """[^(),]{1,}""".r ^^ { _.charAt(0).asInstanceOf[T] }
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


  def main(args: Array[String]): Unit = {

    val inputFileName = "sample.txt"
    val inputFile = scala.io.Source.fromResource(inputFileName).mkString

    //val nodes = inputFile.filter(_.toString.matches("[a-zA-Z ]+")).toLowerCase//.split("\\W")

    val nodes = inputFile.filter(_.toString.matches("[^(),]+")) //[^(),]+

    println(s"Building Tree using source ${inputFileName} ... ")
    val tree = new Tree[Char].buildHuffman(nodes)

    println
    println(s"Serializing Tree ... ")
    val serialized: String = tree.toString
    println(tree.toString)

    println
    println("Unserializing Tree ...")
    val tree2 = new Tree[Char].fromString(serialized)
    println(tree2.toString)

    println
    println("Map ...")
    nodes.toSeq.distinct.map(n => {
      println(s"${n} = ${tree.encode(n).map(if (_) 1 else 0).mkString("")}")
    })

    val word = "the little boy is hungry and keeps doing stupid things. The man likes to build encoding software."
    println
    println(s"Encoding '$word' ...")
    println(new BigInteger(word.getBytes()).toString(2))
    println("    bits=" + new BigInteger(word.getBytes()).toString(2).size)
    val encodedWord = tree.encodeSeq(word)
    println(encodedWord.map(x=> if (x == true) 1 else 0 ).mkString)
    println("    bits=" + encodedWord.size + " bytes=" + word.length + " avg=" + (encodedWord.size / word.length))

    println
    println(s"Decoding sequence ...")
    println(tree.decodeSeq(encodedWord).mkString(""))

    //println(tree.decodeSeq(Seq(true, false, false, true, true, true)))

    import java.io._
    val pw = new PrintWriter(new File(s"src/main/resources/${inputFileName}.codec" ))
    pw.write(tree.toString)
    pw.close

  }

}