import org.scalaml.core.Types.ScalaMl.{XSeries, DblVector}
import org.scalaml.filtering.movaverage._
import org.scalaml.plots._
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.{DataSource, DataSink}
val p = 7
val p_2 = p >> 1
val w = Array.tabulate(p)(n => if(n==p_2) 1.0 else 1.0/Math.abs(n-p_2))
val weights = w map { _ / w.sum }
val src = DataSource("/home/cody/learn/ScalaMl/resources/data/Chap3/BAC.csv", normalize=false)
val sMvAve = SimpleMovingAverage[Double](p) |>
val wMvAve = WeightedMovingAverage[Double](weights) |>
val eMvAve = ExpMovingAverage[Double](p) |>
val results = for {
  price <- src.get(YahooFinancials.adjClose)
  priceSlice = price.slice(0,200)
  sMvOut <- sMvAve(priceSlice)
  wMvOut <- wMvAve(priceSlice)
  eMvOut <- eMvAve(priceSlice)
} yield {
  List[DblVector](priceSlice, sMvOut, wMvOut, eMvOut)
}
val outFile = "/home/cody/learn/ScalaMl/output/chap3/mvaverage"+p.toString+".csv"
DataSink[Double]( outFile ) |> results.get
LinePlot.display(
  results.get.zip(List("price","simple","weighted","exponential")),
  Legend("", "Moving Averages " + p, "time", "price"),
  new LightPlotTheme)
