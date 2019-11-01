import java.io.{BufferedWriter, DataOutputStream, File, FileOutputStream, FileWriter}

import HuffmanCodec.Tree

class HuffmanTooling[T] {

  var file: Option[File] = None
  var codec: Option[Tree[T]] = None

  def openFile(res: String): HuffmanTooling[T] = {
    file = Some(new File(res))
    this
  }

  def generateCodec: HuffmanTooling[T] = {
    file match {
      case Some(file) => codec =
        Some(new Tree[T]()
          .buildHuffman(
            scala.io.Source.fromFile(file)
              .mkString.toSeq.asInstanceOf[Seq[T]]))
    }
    this
  }

  def saveCodec: HuffmanTooling[T] = {
    (codec, file) match {
      case (Some(codec), Some(f)) => {
        saveCodec(s"${f.getAbsolutePath}.codec")
      }
    }
    this
  }

  def encodeAndSave: HuffmanTooling[T] = {

    (codec, file) match {

      case (Some(codec), Some(f)) => {

        val fos = new FileOutputStream(s"${f.getAbsolutePath}.huff")

        def writeBits(bytes: Seq[Int]): Boolean = {

          bytes.size match {
            case 1 => {
              fos.write(bytes.head)
              true
            }
            case _ => {
              fos.write(bytes.head)
              writeBits(bytes.tail)
              false
            }
          }
        }

        def byte8bits(bit8: Seq[Boolean]): Int = {
          Integer.parseInt(HuffmanTooling.asBinaryDigits(bit8), 2)
        }

        val fileData = scala.io.Source.fromFile(f).mkString.toSeq.asInstanceOf[Seq[T]]
        val bits = codec.encodeSeq(fileData)
        val bytes: Seq[Int] = bits.grouped(8).map(byte8bits(_)).toSeq
        println(" ---- - -- - - - - - - - --- --- -- - -")
        println(bits.size + " bits")
        println(bytes.size + " bytes")
        writeBits(bytes)
        println(" ---- - -- - - - - - - - --- --- -- - -")
        fos.close()

      }
    }
    this
  }

  def saveCodec(path: String): HuffmanTooling[T] = {
    (codec) match {
      case (Some(codec)) => {
        val bw = new BufferedWriter(new FileWriter(path))
        bw.write(codec.toString)
        bw.close()
      }
    }
    this
  }

  def openCodec(): HuffmanTooling[T] = {
    file match {
      case Some(file) =>
        openCodec(s"${file.getAbsolutePath}.codec")
    }
  }

  def openCodec(codecPath: String): HuffmanTooling[T] = {
    val codecData = scala.io.Source.fromFile(s"$codecPath").mkString
    codec = Some(new Tree[T]().fromString(codecData))
    this
  }

}

object HuffmanTooling {
  //def loadBuffered(f: String) = scala.io.Source.fromFile(f)
  //def loadTestFileAlphanum(f: String) = scala.io.Source.fromResource(f).mkString.filter(_.toString.matches(Regex.alphanum))
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolSeq(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList
}