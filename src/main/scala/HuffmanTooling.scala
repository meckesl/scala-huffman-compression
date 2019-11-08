import java.io.{BufferedWriter, ByteArrayOutputStream, DataOutputStream, File, FileInputStream, FileNotFoundException, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util

class HuffmanTooling[T] {

  var file: Option[File] = None
  var codec: Option[HuffmanTree[T]] = None

  def openFile(res: String): HuffmanTooling[T] = {
    file = Some(new File(res))
    this
  }

  private def time[R](msg: String)(block: => R): R = {
    println(msg)
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(" --> Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  @throws(classOf[FileNotFoundException])
  def generateCodec: HuffmanTooling[T] = {
    file match {
      case Some(file) =>
        time(s"Generate Codec from '$file'") {
        codec = Some(new HuffmanTree[T]()
          .build(
            scala.io.Source.fromFile(file)
              .mkString
              .toSeq.asInstanceOf[Seq[T]]))
        }
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
        val logmsg = s.take(20).mkString("")
        time(s"Encoding [$logmsg]...") {
          HuffmanTooling.asBinaryDigits(codec.encodeSeq(s))
        }
      case None => throw new Exception("Codec not loaded")
    }
  }

  @throws(classOf[Exception])
  def decode(value: String): String = {
    codec match {
      case Some(codec) =>
        codec.decodeSeq(HuffmanTooling.asBoolList(value).toList).mkString("")
      case None => throw new Exception("Codec not loaded")
    }
  }

  @throws(classOf[Exception])
  def decodeAndSave = {

    (codec, file) match {
      case (Some(codec), Some(f)) => {

        time(s"Decoding ${f.getAbsolutePath}.huff") {

          val bytes = Files.readAllBytes(Paths.get(s"${f.getAbsolutePath}.huff"))
          val bitset = util.BitSet.valueOf(bytes)
          val bools = (0 to bitset.length).map(bitset.get(_)).toList

          var content: String = null
          time(s"   Algo. decodeSeq time ${f.getAbsolutePath}.huff") {
            content = codec.decodeSeq(bools).mkString("")
          }

          val pw = new PrintWriter(s"${f.getAbsolutePath}.dec")
          pw.write(content);
          pw.close

        }

      }
        case _ => throw new Exception("A file and a codec must be loaded")
    }
  }

  @throws(classOf[Exception])
  def encodeAndSave: HuffmanTooling[T] = {
    (codec, file) match {
      case (Some(codec), Some(f)) => {
        time(s"Encoding $file") {
          val baos = new ByteArrayOutputStream()
          val fos = new FileOutputStream(s"${f.getAbsolutePath}.huff")
          val fileData = scala.io.Source.fromFile(f).mkString.toSeq.asInstanceOf[Seq[T]]
          val toWrite = HuffmanTooling.asBitSet(codec.encodeSeq(fileData))
          baos.write(toWrite.toByteArray)
          baos.writeTo(fos)
          baos.close()
          fos.close()
        }
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
    codec = Some(new HuffmanTree[T]().fromString(codecData))
    this
  }

}

object HuffmanTooling {
  def bitsAsInts(bit8: Seq[Boolean]): Int = Integer.parseInt(asBinaryDigits(bit8), 2)
  def asBitSet(bits: Seq[Boolean]) : util.BitSet = {
    val bitSet = new util.BitSet(bits.length)
    var count = 0
    for (c <- asBinaryDigits(bits).toCharArray) {
      if (c == '1') bitSet.set(count)
      count += 1
    }
    bitSet
  }
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolList(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList
}