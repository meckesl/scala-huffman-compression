object ScratchPad {

  def main(args: Array[String]): Unit = {

    val t = new HuffmanTooling[Char]()

    t.openFile("/Users/meckes/Desktop/btsync-projects/Encoder/src/test/resources/english_50kb.txt")
      .generateCodec
      .saveCodec
      .encodeAndSave
      .decodeAndSave
  }

}