import scala.io.Source
object DecisionTree extends App {
  val MIN_GIN = 0.08
  override def main(args: Array[String]): Unit = {
    
  }

  case class GainFeature(gain: Double, feature: Int, left: List[Iris], right: List[Iris], threshold: Double)
  def divideByFeature(dataSet: List[Iris], features: List[Int]): DTree = {
    if (dataSet.size == 0) Empty
    else {
      //best feature
      val gainFeature = bestFeature(features, dataSet, shannoForFeature(dataSet))
      val leftTree = divideByFeature(gainFeature.left, features.filter(f => f != gainFeature.feature))
      val rightTree = divideByFeature(gainFeature.right, features.filter(f => f != gainFeature.feature))
      Branch(leftTree, rightTree, Criteria(gainFeature.feature, gainFeature.threshold, mostType(dataSet)))
    }
  }

  def bestFeature(features: List[Int], dataSet: List[Iris], curGain: Double): GainFeature = {
    var gainFeature = GainFeature(0.0, 0, List[Iris](), List[Iris](), 0.0)
    def productDouble(ir: Iris, index: Int): Double = {
      ir.productElement(index) match {
        case s: Double => s
        case _ => throw new IllegalArgumentException()
      }
    }
    features.foreach(findex => {
      //对于每一个特征 遍历所有的 findex 对应的值 进行拆分并计算增益
      val featureValue: List[Double] = dataSet.map(i => productDouble(i, findex))
      featureValue.toSet.foreach(v => {
        var left = dataSet.filter(ir => {
          productDouble(ir, findex) < v
        })
        var right = dataSet.filter(ir => {
          productDouble(ir, findex) >= v
        })
        val gains = curGain - shannoForFeature(left) - shannoForFeature(right)
        if (gains> gainFeature.gain) {
          gainFeature = GainFeature(gains, findex, left, right, v)
        }
      })
    })
    gainFeature
  }

}

//feature index, threshold, left list, right list, left-tree, right-tree

def mostType(xs: List[Iris]): Ttype = {
  xs.groupBy(i => i.ttype).mapValues(ilist => ilist.size).toSeq.sortWith((x1, x2) => x1._2 > x2._2).head._1
}

def shannoForFeature(ds: List[Iris]): Double = {
  // limit(1->n) p(x) * (-log2( p(x) )
  // log(x) 特征： 在 x:[0,1) 上面 y 单调递增  (-max, 0)
  // 统计有多少数据样本 以及每个 ds 里面样本的含量
  // 可以算出每个样本的概率
  val probiblyByKey = ds.groupBy(r => r.ttype).mapValues(i => i.size.toDouble / ds.size.toDouble)
  // foldLeft 一边折叠一边sum
  // 一边计算一边 sum
  ds.foldLeft(0.0)((b: Double, a: Iris) => {
    b - probiblyByKey(a.ttype) * math.log(probiblyByKey(a.ttype))
  })
}

sealed trait DTree
case class Branch(left: DTree, right: DTree, criteria: Criteria) extends DTree
case object Empty extends DTree

case class Criteria(featureIndex: Int, threshold: Double, ttype: Ttype)
}

//Decision Tree
