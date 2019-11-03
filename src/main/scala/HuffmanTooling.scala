import java.io.{BufferedWriter, ByteArrayOutputStream, DataOutputStream, File, FileNotFoundException, FileOutputStream, FileWriter}

import HuffmanCodec.Tree

class HuffmanTooling[T] {

  var file: Option[File] = None
  var codec: Option[Tree[T]] = None

  def openFile(res: String): HuffmanTooling[T] = {
    file = Some(new File(res))
    this
  }

  @throws(classOf[FileNotFoundException])
  def generateCodec: HuffmanTooling[T] = {
    file match {
      case Some(file) => codec =
        Some(new Tree[T]()
          .buildHuffman(
            scala.io.Source.fromFile(file)
              .mkString
              .filter(_.toString.matches("[^(),]+"))
              .toSeq.asInstanceOf[Seq[T]]))
      case  None => throw new FileNotFoundException("There is no open file")
    }
    this
  }

  @throws(classOf[FileNotFoundException])
  def saveCodec: HuffmanTooling[T] = {
    (codec, file) match {
      case (Some(codec), Some(f)) => saveCodec(s"${f.getAbsolutePath}.codec")
      case  _ => throw new FileNotFoundException("There is no open file")
    }
    this
  }

  @throws(classOf[Exception])
  def encode(s: Seq[T]): String = {
    codec match {
      case Some(codec) =>
        HuffmanTooling.asBinaryDigits(codec.encodeSeq(s))
      case None => throw new Exception("Codec not loaded")
    }
  }

  @throws(classOf[Exception])
  def decode(value: String): String = {
    codec match {
      case Some(codec) =>
        codec.decodeSeq(HuffmanTooling.asBoolList(value)).mkString("")
      case None => throw new Exception("Codec not loaded")
    }
  }


  @throws(classOf[Exception])
  def encodeAndSave: HuffmanTooling[T] = {

    (codec, file) match {

      case (Some(codec), Some(f)) => {

        val baos = new ByteArrayOutputStream()
        val fos = new FileOutputStream(s"${f.getAbsolutePath}.huff")

        def writeInts(ints: Seq[Int]): Boolean = {
          baos.write(ints.head)
          if (ints.size != 1) writeInts(ints.tail) else false
        }

        val fileData = scala.io.Source.fromFile(f).mkString.toSeq.asInstanceOf[Seq[T]]
        val bits = codec.encodeSeq(fileData)
        val ints: Seq[Int] = bits.grouped(8).map(HuffmanTooling.bitsAsInts(_)).toSeq

        writeInts(ints)
        baos.writeTo(fos)
        baos.close()
        fos.close()

      }
      case _ => throw new Exception("A file and a codec must be loaded")

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

  @throws(classOf[FileNotFoundException])
  def openCodec(): HuffmanTooling[T] = {
    file match {
      case Some(file) =>
        openCodec(s"${file.getAbsolutePath}.codec")
      case None => throw new FileNotFoundException("There is no open file")
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

  def bitsAsInts(bit8: Seq[Boolean]): Int = Integer.parseInt(asBinaryDigits(bit8), 2)
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolList(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList
}