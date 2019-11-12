import java.io.{BufferedWriter, ByteArrayOutputStream, File, FileNotFoundException, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util
import java.util.Base64

import scala.collection.parallel.CollectionConverters._

class HuffmanTooling {

  var File: Option[File] = None
  var Codec: Option[HuffmanTree] = None

  private def time[R](msg: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(msg + " -> " + (t1 - t0) / 1000000 + "ms")
    result
  }

  def openFile(res: String): HuffmanTooling = {
    File = Some(new File(res))
    this
  }

  @throws(classOf[FileNotFoundException])
  def generateCodec: HuffmanTooling = {
    File match {
      case Some(file) =>
        time(s"Generate Codec from '$file'") {
        Codec = Some(new HuffmanTree()
          .build(
            Files.readAllBytes(Paths.get(file.getAbsolutePath))
              .grouped(1)
              .map(x=> new TData(x, HexByte.toHex(x)))
              .toList))
        }
      case None => throw new FileNotFoundException("There is no open file")
    }
    this
  }

  @throws(classOf[FileNotFoundException])
  def saveCodec: HuffmanTooling = {
    (Codec, File) match {
      case (Some(codec), Some(f)) => saveCodec(s"${f.getAbsolutePath}.codec")
      case _ => throw new FileNotFoundException("There is no open file")
    }
    this
  }

  @throws(classOf[Exception])
  def encodeParallel(s: List[Byte]): Seq[Boolean] = {
      Codec match {
        case Some(codec) =>
          val cores = Runtime.getRuntime.availableProcessors
          val size = 1 + s.size / cores
          time(s"Encoding with $cores cores ($size chars out of ${s.size}) ") {
            s.grouped(size).zipWithIndex.toSeq.par.flatMap(threadSeq => {
              time(s"Thread #${threadSeq._2 + 1} -> encoding [${threadSeq._1.take(20).mkString("")}]...") {
                codec.encodeSeq(threadSeq._1.toArray)
              }
            })
          }.seq
        case None => throw new Exception("Codec not loaded")
    }
  }

  @throws(classOf[Exception])
  def decode(value: String): String = {
    Codec match {
      case Some(codec) =>
        codec.decodeSeq(HuffmanTooling.asBoolList(value).toList).mkString("")
      case None => throw new Exception("Codec not loaded")
    }
  }

  @throws(classOf[Exception])
  def decodeAndSave = {
    (Codec, File) match {
      case (Some(codec), Some(f)) => {
        time(s"Decoding ${f.getAbsolutePath}.huff") {
          val bytes = Files.readAllBytes(Paths.get(s"${f.getAbsolutePath}.huff"))
          val bitset = util.BitSet.valueOf(bytes)
          val bools = (0 to bitset.length).map(bitset.get(_)).toList
          val path = Paths.get(s"${f.getAbsolutePath}.dec")
          val output = codec.decodeSeq(bools).flatMap(_.getBytes())
          Files.write(path, output.toArray)
        }
      }
      case _ => throw new Exception("A file and a codec must be loaded")
    }
  }

  @throws(classOf[Exception])
  def encodeAndSave: HuffmanTooling = {
    (Codec, File) match {
      case (Some(codec), Some(f)) => {
        time(s"Encoding $File") {
          val baos = new ByteArrayOutputStream()
          val fos = new FileOutputStream(s"${f.getAbsolutePath}.huff")
          val fileData = Files.readAllBytes(Paths.get(f.getAbsolutePath))
          val toWrite = HuffmanTooling.asBitSet(encodeParallel(fileData.toList))
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

  @throws(classOf[Exception])
  def saveCodec(path: String): HuffmanTooling = {
    (Codec) match {
      case (Some(codec)) => {
        val bw = new BufferedWriter(new FileWriter(path))
        bw.write(codec.toString)
        bw.close()
      }
      case _ => throw new Exception("A codec must have been generated")
    }
    this
  }

  @throws(classOf[FileNotFoundException])
  def openCodec(): HuffmanTooling = {
    File match {
      case Some(file) =>
        openCodec(s"${file.getAbsolutePath}.codec")
      case None => throw new FileNotFoundException("There is no open file")
    }
  }

  def openCodec(codecPath: String): HuffmanTooling = {
    val codecData = scala.io.Source.fromFile(s"$codecPath").mkString
    Codec = Some(new HuffmanTree().fromString(codecData))
    this
  }

}

object HuffmanTooling {
  def bitsAsInts(bit8: Seq[Boolean]): Int = Integer.parseInt(asBinaryDigits(bit8), 2)
  def asBitSet(bits: Seq[Boolean]) : util.BitSet = {
    val bitSet = new util.BitSet(bits.length)
    bits.zipWithIndex.foreach{case(b:Boolean,i:Int)=>if(b)bitSet.set(i)}
    bitSet
  }
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolList(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList
}