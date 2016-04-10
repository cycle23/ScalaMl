import org.scalaml.core.Types.ScalaMl.{DblPair, XVSeries}
import org.scalaml.util.MapUtils.Counter
import org.scalaml.validation.Label._

import scala.util.Random

val xs = List.tabulate(5)(_ + 1)

val values = Vector(1.0,2.0,3.0,8.0,4.5)
val zero = (0.0, 0.0)
val sums = values./:(zero)((s,x) => (s._1 + x, s._2 + x*x))
lazy val mean = sums._1/values.size
lazy val variance =
  (sums._2 - mean*mean*values.size)/(values.size-1)
lazy val stdDev = Math.sqrt(variance)
trait Validation { def score: Double}
class BinFValidation[T <: AnyVal](
                                 expected: Vector[Int],
                                 xt: XVSeries[T]
                                 )
                                 (predict: Array[T] => Int)
                                 (implicit f: T=>Double)
      extends Validation {
  final val POSITIVE = 1

  val counters = {
    val predicted = xt.map (predict(_))
    expected.zip(predicted)
            .aggregate(new Counter[Label])((cnt, ap) =>
              cnt + classify(ap._1, ap._2), _ ++ _)
  }

  override def score: Double = f1
  lazy val f1 = 2.0*precision*recall/(precision + recall)
  lazy val precision = compute(FP)
  lazy val recall = compute(FN)
  def compute(n: Label): Double = {
    val denom=counters(TP) + counters(n)
    counters(TP).toDouble/denom
  }
  def classify(predicted: Int, expected: Int): Label =
    if (expected == predicted) if (expected == POSITIVE) TP else TN
    else if (expected == POSITIVE) FN else FP


}
def predict(arr: Array[Double]) : Int = {
  if (Random.nextBoolean()) 1
  else 0
}
val expected = Vector(1,1,0,1,0)
val xvSeries = values.map(f => Array(f))
val x0 = xvSeries(0)
val binF = new BinFValidation(expected, xvSeries)(predict)
val c = binF.counters

class MultiFValidation[T <: AnyVal](
                                   expected: Vector[Int],
                                   xv: XVSeries[T],
                                   classes: Int)
                                   (predict: Array[T] => Int)
                                   (implicit f: T => Double)
      extends Validation {

  val confusionMatrix: Matrix[Int] =
    labeled./:(Matrix[Int](classes)){
      case (m, (x, n)) =>
        m + (n, predict(x), 1)
    }

  val macroStats: DblPair = {
    val pr = Range(0, classes)./:(0.0,0.0)
      ((s, n) => {
        val tp = confusionMatrix(n, n)
        val fn = confusionMatrix.col(n).sum - tp
        val fp = confusionMatrix.row(n).sum - tp
        (s._1 + tp.toDouble/(tp + fp), s._2 + tp.toDouble/(tp + fn))
    })
  }
}