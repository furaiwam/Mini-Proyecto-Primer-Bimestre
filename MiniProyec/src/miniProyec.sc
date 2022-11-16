def integracion(a:Int, b:Int ,f:Double => Double) : Double= {
  val x = (a + b) / 2
  (b - a) * ((f(a) + 4 * f((x) + f(b)) / 6))
}
val f = (x: Double) => -Math.pow(x, 2) + (8 * x) - 12

integracion(3, 5, f)

val h = (x: Double) => 3 * Math.pow(x, 2)
val i = (x: Double) => x + 2 * Math.pow(x, 2) - Math.pow(x, 3) + 5 * Math.pow(x, 4)
val j = (x: Double) => ((2 * x + 1) / (Math.pow(x, 2) + x))
val k = (x: Double) => Math.pow(Math.E, x)
val l = (x: Double) => (1 / Math.sqrt(x - 1))
val m = (x: Double) => (1 / (1 + Math.pow(x, 2)))

val i1 = integracion(3, 5, f)
val i2 = integracion(0, 2, h)
val i3 = integracion(-1, 1, i)
val i4 = integracion(1, 2, j)
val i5 = integracion(0, 1, k)
val i6 = integracion(2, 3, l)
val i7 = integracion(0, 1, m)

print("Simpson 1/3 compuesta ")

def integralCom(a: Int, b: Int, n: Int, f: Double => Double): Double = {
  val h = ((b - a) / n)
  val xj = (j: Double) => a + (j * h)
  val funcion = (j: Double) => f(xj(2 * j - 2)) + 4 * f(xj(2 * j - 1)) + f(xj(2 * j))
  (1 to 2).map(funcion(_))(h / 3)
}

print("Simpson 1/3 extendida.")

def integralExt( a:Double, b:Double, f:Double=>Double) : Double = {
  val n = (2 * (b - a))
  val h = (b-a)/n
  val i = (1 to n-1 by 2)
  val j = (2 to n-2 by 2)
  val sumI = i.map(x => f(a + x * h))
  val sumJ = j.map(x => f(a + x * h))
  (h/3) * (  f(a) +  (4 * sumI) + (2 * sumJ) +  f(b)  )
}