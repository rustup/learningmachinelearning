import scala.io.Source
// knn with data set from http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
// shortcomings : imbalanced samples leads deviation
import Iris._
object kNN extends App {

  var split = 0.75
  override def main(args: Array[String]): Unit = {
    val dataLines = Source.fromURL("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data").getLines()
    val dataSets = for (irisLine <- dataLines if (irisLine.size > 0)) yield Iris.create(irisLine)

    val bstart = Tuple2[List[Iris], List[Iris]](List[Iris](), List[Iris]())
    val setTuple = dataSets.foldLeft(bstart) {
      (b: (List[Iris], List[Iris]), i: Iris) =>
        {
          if ((scala.util.Random.nextDouble() % 1.0) > 0.75) (b._1, i :: b._2) else (i :: b._1, b._2)
        }
    }

    val trainSet = setTuple._1
    val testSet = setTuple._2
    testSet.foreach { x: Iris =>
      {
        println("predicated:", mostType(neighbourTopN(trainSet, x, 5)), "real:", x.ttype)
      }
    }

  }

  def neighbourTopN(trainSet: List[Iris], iris: Iris, n: Int): List[Iris] = {
    val neighs = for (ts <- trainSet) yield (ts, distanceOf(ts, iris))
    neighs.sortWith((x1, x2) => {
      x1._2 < x2._2
    }).take(n).map(x => x._1)
  }

  def mostType(xs: List[Iris]): Ttype = {
    xs.groupBy(i => i.ttype).mapValues(ilist => ilist.size).toSeq.sortWith((x1, x2) => x1._2 > x2._2).head._1
  }

  def distanceOf(i1: Iris, i2: Iris): Double = {
    // can iterator element of a class ?
    math.sqrt(math.pow(i1.petalLen - i2.petalLen, 2) + math.pow(i1.petalWidth - i2.petalWidth, 2) + math.pow(i1.sepalLen - i2.sepalLen, 2) + math.pow(i1.sepalWidth - i2.sepalWidth, 2))
  }

}

