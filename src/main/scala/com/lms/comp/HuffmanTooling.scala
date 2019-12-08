package com.lms.comp

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.BitSet

import com.lms.comp.util.HexByte

class HuffmanTooling {

  var File: Option[File] = None
  var Codec: Option[HuffmanTree] = None

  private def time[R](msg: String)(block: => R): R = {
    println(msg)
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(" -> " + (t1 - t0) / 1000000 + "ms" + "(" + msg + ")")
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

          val bytes = Files.readAllBytes(Paths.get(file.getAbsolutePath))
            .grouped(1)
            .map(new TData(_))
            .toList

          Codec = Some(new HuffmanTree().build(bytes))
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
  def encodeParallel(s: Seq[Byte]): Seq[Boolean] = {
    import scala.collection.parallel.CollectionConverters._
    Codec match {
      case Some(codec) =>
        val cores = Runtime.getRuntime.availableProcessors
        val singleCoreTaskSize = 1 + s.size / cores
        time(s"Encoding with $cores cores ($singleCoreTaskSize chars out of ${s.size}) ") {
          s.grouped(singleCoreTaskSize).zipWithIndex.toSeq.par.flatMap {
          case (threadSeq: Seq[Byte], threadId: Int) => {
            time(s"Thread #${threadId + 1} -> encoding [${threadSeq.take(20).mkString("")}]...") {
              codec.encodeSeq(threadSeq.toArray)
            }
          }
          }
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
          val bitset = BitSet.valueOf(bytes)
          val bools = (0 to bitset.length).map(bitset.get(_)).toList
          val path = Paths.get(s"${f.getAbsolutePath}.dec")
          val output = codec.decodeSeq(bools).flatMap(_.bytes)
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

  def getCodecStats: Unit = {

    (Codec, File) match {
      case (Some(codec), Some(f)) =>

        val fileSize = f.length()
        val fileData: Array[Byte] = Files.readAllBytes(Paths.get(f.getAbsolutePath))

        val entries = codec.flatten.filter(_.node.nonEmpty).sortBy(_.weight).reverse
        println("==============")
        println(s"File: $f ($fileSize bytes)")
        println(s"Codec: entries=${entries.size}")
        println("==============")

        entries.foreach(n => {

          val byte: Byte = n.node.get.bytes.head
          val byteInSource = fileData.count(_.equals(byte))
          //val byteInSource = s"[${new String(n.node.get.getBytes(), StandardCharsets.UTF_8)}]".r.findAllIn(fileData.mkString).length

          println(
            s"utf8='${new String(n.node.get.bytes, StandardCharsets.UTF_8)}' " +
            s"hex=${n.node.get} " +
            s"value=${HexByte.toBytes(n.node.get.toString).length} " +
            s"bitcost=${codec.encode(n.node.get).size} " +
            s"weight=${(100.toDouble/fileSize)*byteInSource}% ")
        })
      case _ => throw new Exception("A file and a codec must be loaded")
    }
  }

}

object HuffmanTooling {
  def bitsAsInts(bit8: Seq[Boolean]): Int = Integer.parseInt(asBinaryDigits(bit8), 2)
  def asBitSet(bits: Seq[Boolean]) : BitSet = {
    val bitSet = new BitSet(bits.length)
    bits.zipWithIndex.foreach{case(b:Boolean,i:Int)=>if(b)bitSet.set(i)}
    bitSet
  }
  def asBinaryDigits(bs: Seq[Boolean]) = bs.map(if (_) '1' else '0').mkString("")
  def asBoolList(binary: String) : Seq[Boolean] = binary.toSeq.map(x => if (x.equals('1')) true else false).toList
}