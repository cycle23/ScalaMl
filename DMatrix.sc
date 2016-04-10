import org.scalaml.util.MathUtils.DMatrix

val m2 = Range(0, 2)./:(DMatrix(3, 2))((m, n) => m +=(1, n, 1.0))
val m = Vector(0,1)./:(DMatrix(3, 2))((m, n) => m +=(1, n, 1.0))
val v2 = Range(0, 2)./:(0)((a, t) => a + t)
val v = Vector(0,1,2)./:(0)((a,t) => a + t)
val a = Vector(1,3,5)
val b = Vector(2,4,6)
val err = b.zip(a)./:(0.0)((err, z) => {
  println("err: "+ err +", b: "+ z._1 +", a: "+ z._2)
  err + (z._1 - z._2) * (z._1 - z._2)
})
val err2 = b.zip(a)./:(0.0)((err2, z) => err2 + (z._1 - z._2) * (z._1 - z._2))

