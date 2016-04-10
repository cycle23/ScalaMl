import org.scalaml.core.ETransform
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.stats.Stats
import scala.util.{Random, Try}

trait Monad[M[_]] {
  def unit[T](a: T): M[T]
  def map[U, V](m: M[U])(f: U => V): M[V]
  def flatMap[U, V](m: M[U])(f: U => M[V]): M[V]
}

// f: R^n=>R^n
// g: R^n=>Double
type V = Vector[Double]
trait F { val f: V => V }
trait G { val g: V => Double }

// h: f composed g
class H { self: G with F =>
  def apply(v: V): Double = g(f(v))
}

// f: x -> e^x
// g: x -> sum from i=0 to n-1 of x(i)
// h: x -> sum from i=0 to n-i of e^x(i)

lazy val h = new H with G with F {
  val f: V => V = (v: V) => v.map(Math.exp(_))
  val g: V => Double = (v: V) => v.sum
}

h(Vector(1.0, 2.0, 3.0))

trait Sampling[T, A, B] {
  val sampler: ETransform[T] { type U = A; type V = B }
}
trait Normalization[T, A, B] {
  val normalizer: ETransform[T] { type U = A; type V = B }
}
trait Aggregation[T, A, B] {
  val aggregator: ETransform[T] { type U = A; type V = B }
}

class Workflow[T, U, V, W, Z] {
  self:
    Sampling[T, U, V] with
    Normalization[T, V, W] with
    Aggregation[T, W, Z] =>

  def |> (u: U): Try[Z] = for {
    v <- sampler |> u
    w <- normalizer |> v
    z <- aggregator |> w
  } yield z
}

type Dbl_F = Double=>Double
val samples = 100
val normRatio = 10
val splits = 4

val workflow =
    new Workflow[Int, Dbl_F, DblVector, DblVector, Int]
      with Sampling[Int, Dbl_F, DblVector]
      with Normalization[Int, DblVector, DblVector]
      with Aggregation[Int, DblVector, Int] {

   val sampler = new ETransform[Int](samples)
   {
     type U = Dbl_F
     type V = DblVector
     override def |> : PartialFunction[U, Try[V]] = {
       case f: U =>
          Try(Vector.tabulate(samples)(n => f(1.0*n/samples)))
     }
   }
   val normalizer = new ETransform[Int](normRatio) {
     type U = DblVector
     type V = DblVector
     override def |> : PartialFunction[U, Try[V]] = {
       case x: U if x.nonEmpty =>
         Try(Stats[Double](x).normalize(0.0,1.0))
     }
   }
   val aggregator = new ETransform[Int](splits) {
      type U = DblVector
      type V = Int
      override def |> : PartialFunction[U, Try[V]] = {
        case x: U if x.nonEmpty =>
          Try(x.indices.find(x(_) == 1.0).get)
      }
   }

}

val g = (x: Double) => Math.log(x+1.0) + Random.nextDouble
Try( workflow |> g)


/*
abstract class ITransform[T](val xt: Vector[T]) { // input
  type V  // output
  def |> : PartialFunction[T, Try[V]]

  private val iTransformMonad = new Monad[ITransform] {

    override def unit[T](t: T) = iTransform(Vector[T](t))

    override def map[T, U](m: ITransform[T])
                          (f: T => U):
      ITransform[U] = iTransform( m.xt.map(f) )

    override def flatMap[T, U](m: ITransform[T])
                              (f: T=>ITransform[U]): ITransform[U] =
      iTransform(m.xt.flatMap(t => f(t).xt))
  }

  implicit class iTransform2Monad[T](fct: ITransform[T]) {
    def unit(t: T) = iTransformMonad.unit(t)

    final def map[U](f: T=>U): ITransform[U] =
      iTransformMonad.map(fct)(f)

    final def flatMap[U](f: T => ITransform[U]): ITransform[U] =
      iTransformMonad.flatMap(fct)(f)

    def filter(p: T => Boolean): ITransform[T] =
      iTransform(fct.xt.filter(p))
  }
}

class DataSourceConfig
class DataSource (
      config: DataSourceConfig,
      srcFilter: Option[List[String] => Boolean] = None)
  extends ETransform[DataSourceConfig](config) {
    type U = List[List[String] => Double]
    type V = List[List[Double]]

    override def |> : PartialFunction[U, Try[V]] = {
      case u: U if u.nonEmpty =>
        Try(List(List(2.0)))
    }
  }

class SVMConfig
class SVM[T <: AnyVal](
                        config: SVMConfig,
                        xt: Vector[Array[T]],
                        expected: Vector[Double]
                      )
                      (implicit f: T => Double)
  extends ITransform[Array[T]](xt) {

    type V = Double
    override def |> : PartialFunction[Array[T], Try[V]] = {
      case x: Array[T]
        if(x.length == 1) =>
          Try(2.0)
    }
}

*/

/*
abstract class ETransform[T](val config: T) {
  type U  // input, along with config
  type V  // output
  def |> : PartialFunction[U, Try[V]]

  private val eTransformMonad = new Monad[ETransform] {
    override def unit[T](t: T) = eTransform(t)
    override def map[T, U](m: ETransform[T])(f: T=> U): ETransform[U] =
      eTransform( f(m.config) )
    override def flatMap[T, U](m: ETransform[T])(f: T=>ETransform[U]): ETransform[U] = f(m.config)
  }

  implicit class eTransform2Monad[T](fct: ETransform[T]) {
    def unit(t: T): Any = eTransformMonad.unit(t)
    final def map[U](f: T => U): ETransform[U] =
        eTransformMonad.map(fct)(f)
    final def flatMap[U](f: T => ETransform[U]): ETransform[U] =
        eTransformMonad.flatMap(fct)(f)
  }
}*/

