

object Bayes extends App {
  def create(lines: Iterator[String]): Bayes = {
    val bayes = new Bayes()
    bayes.loadData(lines)
    bayes
  }

  override def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile("resources/english.txt").getLines()
    Bayes.create(lines)
  }
}

class Bayes() {
  //训练出来的模型
  //创建词典
  //新建一个字符串代码
  //计算概率
  //var vocabsVector: Seq[Int] = Seq[Int]()
  var vocabs: List[String]
  val isSpamVec: Iterator[Boolean]
  case class Proper(p0: Double, p1: Double)

  def loadData(lines: Iterator[String]): Unit = {
    val isSpamVec = lines.map(line => line.split(",").last == "spam")

    val linesHandled = for (line <- lines) yield line.slice(0, line.lastIndexOf(",")).replaceAll("[* ,.]", "")
    //val linessWithout = lines.map(lines => lines.replaceAll("[, .]", ""))
    val vocaSet = for (e <- linessWithout; word <- e.split(" ")) yield word
    vocabs = vocaSet.toSet.toList
  }

  def vocCntInEmail(wordVec: Array[String], tofind: String): Int = {
    wordVec.foldLeft(0)((z: Int, b: String) => {
      if (b == tofind) {
        z + 1
      } else {
        z
      }
    })
  }

  def generateVector(email: String): Seq[Int] = {
    val wordVec = "zhangsan".replaceAll("[, .]", "").split(" ")
    for (v <- vocabs) yield vocCntInEmail(wordVec, v)
  }

  def spamProper: Double = {

  }

  def calculate(): Proper = {
    // P(c|w) 正比垃圾邮件的概率  
    //正比垃圾邮件中出现 w 的概率
    //反比正常邮件中出现该 w 的概率(该值 垃圾邮件和飞垃圾邮件一样 不参与对比
    Proper(1, 1)
    val p0 = math.log()
  }

  def classify(s: String, spam: Boolean): Boolean = {
    ???
  }
}

