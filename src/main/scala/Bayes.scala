

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
  var isSpamVector: Seq[Boolean]
  var gvocaSet: Set[String]
  var gmatrix: Iterator[Seq[Int]]
  var dataLines: Iterator[String]

  def loadData(lines: Iterator[String]): Set[String] = {
    case class Line(line: String, isSpam: Boolean)
    val linesHandled = for (line <- lines) yield {
      val index = line.lastIndexOf(",")
      (line.slice(0, index).replaceAll("[*,.]", ""), line.slice(index, line.size) == "spam")
    }
    isSpamVector = linesHandled.map(_._2).toSeq
    dataLines = linesHandled.map(_._1)

    val vocaSet = linesHandled.flatMap(x => x._1.split(" ")).toSet[String]
    gvocaSet = vocaSet
    gvocaSet
  }

  def generateVector(line: String): Iterator[Int] = {
    val wordVec = line.replaceAll("[,.*]", "").split(" ")
    val existVec = for (v <- gvocaSet) yield wordVec.count(_ == v)
    existVec.toIterator
  }

  def genMatrix(): Iterator[Seq[Int]] = {

    gmatrix = dataLines.map(line => {
      generateVector(line).toSeq
    })
    gmatrix
  }

  //word2vec  计算在原始词汇库中出现的次数
  //用于计算

  case class Ratios(wordInSpamVec: Seq[Double], wordInAll: Seq[Double], spamInAll: Double)
  def train(): Ratios = {
    //由标注数据 作为原始词库 训练得出如下几个数据
    // 垃圾邮件占比
    // 单词在垃圾邮件所有单词中出现的概率 单词在全部邮件所有单词中出现的概率 
    // 从 vocabulary
    // 需要首先生成 matrix
    var wordRadioInAll = gvocaSet.map(x => 0.0).toSeq
    var wordRadioInSpam = gvocaSet.map(x => 0.0).toSeq
    var wordcntInAll = 0
    var wordcntInSpam = 0
    var radioVector = gvocaSet.map(x => 0).toIterator

    val wordcnt = gvocaSet.size
    val spamCnt = 0
    var spanRatio = isSpamVector.filter(_ == true).size.toDouble / isSpamVector.size.toDouble
    gvocaSet.foldLeft(0)((bindex: Int, a: String) => {
      //b 是 index
      var matrixIndex = 0

      var lineIndex = 0
      var spamCnt = 0
      gmatrix.foreach(line => {
        if (isSpamVector(lineIndex)) {
          spamCnt = spamCnt + line.sum
          lineIndex = lineIndex++
        }
      })

      gmatrix.foreach(line => {
        if (isSpamVector(matrixIndex)) {
          //is spam
          wordcntInSpam = wordcntInSpam + line(bindex)
          wordcntInAll = wordcntInAll + line(bindex)
        } else {
          //is ham
          wordcntInAll = wordcntInAll + line(bindex)
        }

        matrixIndex = matrixIndex + 1
      })
      wordRadioInAll[bindex] = wordcntInAll / gvocaSet.size
      wordRadioInSpam[bindex] = wordcntInSpam / spamCnt
      bindex + 1
    })
    Ratios(wordRadioInAll, wordRadioInAll, spanRatio)
  }
}

