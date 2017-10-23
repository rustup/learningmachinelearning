sealed trait Ttype
case object Setosa extends Ttype
case object VersiColour extends Ttype
case object Verginica extends Ttype

case class Iris(sepalLen: Double, sepalWidth: Double, petalLen: Double, petalWidth: Double, ttype: Ttype)

object Iris {
  def create(line: String): Iris = {
    val irisSlice = line.split(",")
    println(line)
    if (irisSlice.size < 4) {
      throw new IndexOutOfBoundsException()
    }

    val ttyp = irisSlice(4) match {
      case "Iris-setosa" => Setosa
      case "Iris-versicolor" => VersiColour
      case "Iris-virginica" => Verginica
    }

    Iris(irisSlice(0).toDouble, irisSlice(1).toDouble, irisSlice(2).toDouble, irisSlice(3).toDouble, ttyp)
  }

  def dataSource(url: String): Iterator[Iris] = {
    import scala.io.Source
    val dataLines = Source.fromURL("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data").getLines()
    for (irisLine <- dataLines if (irisLine.size > 0)) yield Iris.create(irisLine)
  }

}
