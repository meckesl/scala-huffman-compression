import com.lms.comp.HuffmanTree

type T = Char

def buildBalanced(xs: Seq[T]): HuffmanTree[T] = {

  def build(xs: Seq[T]): HuffmanTree[T] = {
    xs match {
      case Nil => new EmptyHuffmanTree
      case _ => {
        val branch = xs.tail.partition(xs.indexOf(_) % 2 == 0)
        NonEmptyHuffmanTree(Some(xs.head), None, build(branch._2), build(branch._1))
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

//def node: Parser[A] = """(\\\\|[^(),\\]|\\,|\\\(|\\\))""".r ^^ {
//  res => (if (res.startsWith("\\")) res.drop(1) else res).charAt(0).asInstanceOf[A]
//}