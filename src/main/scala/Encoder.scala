object Encoder {

  type TreeData = String;

  class EmptyTree[T] extends Tree[T]
  case class NonEmptyTree[T](node: Option[T], weight: Option[Int], left: Tree[T], right: Tree[T]) extends Tree[T]

  class Tree[T] {

    def build(xs: Seq[T]): Tree[T] = {
      xs match {
        case Nil => new EmptyTree
        case _ => {
          val branch = xs.tail.partition(xs.indexOf(_) % 2 == 0)
          NonEmptyTree(Some(xs.head), None, build(branch._2), build(branch._1))
        }
      }
    }

    def buildHuffman(xs:Seq[T]) : Tree[T] = {

      def build(xsUnsorted:Seq[NonEmptyTree[T]]): Tree[T] = {
        val xs = xsUnsorted.sortWith(_.weight.getOrElse(0) < _.weight.getOrElse(0))
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
          .map({ case (node: T, weight: Int) => NonEmptyTree(Some(node), Some(weight), new EmptyTree, new EmptyTree)})
      )

    }

    @throws(classOf[IllegalArgumentException])
    def encode(data: T, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {
      this match {
        case tree: NonEmptyTree[T] => {
          if (tree.node.equals(Some(data))) acc else {
            try ((acc :+ false) ++ tree.left.encode(data, acc)) catch {
              case e: IllegalArgumentException =>
                try ((acc :+ true) ++ tree.right.encode(data, acc)) catch {
                  case e: IllegalArgumentException => throw (e)
                }
            }
          }
        }
        case _ => throw new IllegalArgumentException()
      }
    }

    @throws(classOf[NoSuchElementException])
    def decode(data: Seq[Boolean]): Option[T] = {
      this match {
        case tree: NonEmptyTree[T] => {
          data match {
            case Nil => tree.node
            case h :: tail =>
              if (h) tree.right.decode(tail) else tree.left.decode(tail)
          }
        }
        case _ => throw new NoSuchElementException
      }
    }

    override def toString = {
      this match {
        case t : NonEmptyTree[T] => {
          (t.right, t.left) match {
            case (l, r) if (l.isInstanceOf[EmptyTree[T]] && r.isInstanceOf[EmptyTree[T]]) => {
              s"${t.node.getOrElse("").toString}"
            }
            case _ => {
              s"${t.node.getOrElse("").toString}(${t.left.toString},${t.right.toString})"
            }
          }
        }
        case _ => ""
      }
    }


    def fromString(s: String): Tree[T] = {
      import scala.util.parsing.combinator.RegexParsers
      object TreeParser extends RegexParsers {
        override def skipWhitespace = false
        def node: Parser[T] = """[^(),]+""".r ^^ {_.asInstanceOf[T]}
        def subtrees: Parser[(Tree[T],Tree[T])] = "(" ~ tree.? ~ "," ~ tree.? ~ ")" ^^ {
          case (start ~ left ~ comma ~ right ~ stop) =>
            (left.getOrElse(new EmptyTree[T]), right.getOrElse(new EmptyTree[T]))
        }
        def tree: Parser[Tree[T]] = ((node | subtrees) | (node ~ subtrees)) ^^ {
          case node: T => new NonEmptyTree[T](Some(node), None, new EmptyTree[T], new EmptyTree[T])
          case (left: Tree[T], right: Tree[T]) => new NonEmptyTree[T](None, None, left, right)
          case _ => new EmptyTree[T]
        }
        def apply(input: String) = parseAll(tree, input) match {
          case Success(result, _) => result
          case failure : NoSuccess => scala.sys.error(failure.msg)
        }
      }
      TreeParser.apply(s)
    }

    def toHTML : String = {
      this match {
        case t : NonEmptyTree[T] => {
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

    val nodes = inputFile.filter(_.toString.matches("[^(,]+"))

    val tree = new Tree[Char].buildHuffman(nodes)

    println(s"Building Tree using source ${inputFileName} ... ")

    println(s"Serializing Tree ... ")
    val serialized = tree.toString
    println(serialized)

    println("Unserializing Tree ...")
    val tree2 = new Tree[Char].fromString(serialized)
    println(tree2.toString)

    /*nodes.toSeq.distinct.map(n => {
      println(s"${n} = ${tree.encode(n).map(if (_) 1 else 0).mkString("")}")
    })*/

    /*import java.io._
    val pw = new PrintWriter(new File(s"src/main/resources/${inputFileName}.codec" ))
    pw.write(tree.toString)
    pw.close

    val pw2 = new PrintWriter(new File(s"src/main/resources/${inputFileName}.codec.html" ))
    pw2.write(tree.toHTML)
    pw2.close*/

  }

}