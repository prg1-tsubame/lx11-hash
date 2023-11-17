import scala.collection.mutable.ArraySeq

object Hash {
  val 雪国 = List("国境", "の", "長い", "トンネル", "を", "抜ける", "と", "雪国", "で", "あつ", "た")

  val occurrences = ArraySeq.fill(31)(0)

  def hash(word: String): Int = word.hashCode() % occurrences.size

  @main def run_hash() = {
    println("雪国に出現する語のハッシュ値：")
    雪国.foreach(word => println((word, hash(word))))
    println()

    // ハッシュ表の構築
    雪国.foreach(word => {
      val h = hash(word)
      occurrences(h) = occurrences(h) + 1
    })

    println(" \n雪国に出現する語の出現頻度：")
    雪国.foreach(word => println((word, occurrences(hash(word)))))
    println()
  }
}

object OpenAddressing {
  val HASH_SIZE = 31
  val entries: ArraySeq[String]  = ArraySeq.fill(HASH_SIZE)("")
  val occurrences: ArraySeq[Int] = ArraySeq.fill(HASH_SIZE)(0)

  def hash(word: String): Int = word.hashCode() % HASH_SIZE

  /* 以下の関数の実装はハッシュ表が十分に大きいことを仮定している。
   * キーの数がハッシュ表のエントリー数よりも大きい場合は無限ループになるので注意を要する。
   * その可能性がある場合は終了判定を厳格にすること
   */
  def new_occurrence(word: String): Unit = {
    var h = hash(word) % HASH_SIZE
    while (entries(h) != "" && entries(h) != word) h = (h + 1) % HASH_SIZE
    entries(h) = word
    occurrences(h) = occurrences(h) + 1
  }

  @main def run_open_adressing() = {
    Hash.雪国.foreach(word => new_occurrence(word))
    for (i <- 0 until HASH_SIZE) {
      if (entries(i) != "") println(f"$i ${entries(i)}(${occurrences(i)})")
    }
  }
}