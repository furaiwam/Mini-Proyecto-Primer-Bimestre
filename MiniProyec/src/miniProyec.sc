//declaramos la funcion integracion
def integracion(a:Double, b:Double ,f:Double => Double) : Double= {
  //declaramos la funcion error
  def error(a:Double, b:Double) : Double = (a - b).abs
  (b - a) * ((f(a) + (4 * (f(error(a, b))))  +f(b))) / 6
}
val f = (x: Double) => -Math.pow(x, 2) + (8 * x) - 12
val f2 = (x: Double) => 3 * Math.pow(x, 2)
val f3 = (x: Double) => x + 2 * Math.pow(x, 2) - Math.pow(x, 3) + 5 * Math.pow(x, 4)
val f4 = (x: Double) => ((2 * x + 1) / (Math.pow(x, 2) + x))
val f5 = (x: Double) => Math.pow(Math.E, x)
val f6 = (x: Double) => (1 / Math.sqrt(x - 1))
val f7 = (x: Double) => (1 / (1 + Math.pow(x, 2)))

integracion(3, 5, f)
integracion(0, 2, f2)
integracion(-1,1, f3)
integracion(1, 2, f4)
integracion(0, 1, f5)
integracion(2, 3, f6)
integracion(0, 1, f7)

print("Simpson 1/3 compuesta ")

def integralCom(a: Int, b: Int, n: Int, f: Double => Double): Double = {
  val h = ((b - a) / n)
  val xj = (j: Double) => a + (j * h)
  val funcion = (j: Double) => f(xj(2 * j - 2)) + 4 * f(xj(2 * j - 1)) + f(xj(2 * j))
  (1 to 2).map(funcion(_))(h / 3)
}

integralCom(3, 5, 20, f)
integralCom(0, 2, 22, f2)
integralCom(-1,1, 3,f3)
integralCom(1, 2,3, f4)
integralCom(0, 1, 4,f5)
integralCom(2, 3, 5,f6)
integralCom(0, 1, 3, f7)
print("Simpson 1/3 extendida.")

def integralExt( a:Double, b:Double, f:Double=>Double) : Double = {
  val n = (2 * (b - a)).toInt
  val h = (b-a)/n
  val i = (1 to n-1 by 2).toList
  val j = (2 to n-2 by 2).toList
  val sumI = i.map(x => f(a + x * h)).sum
  val sumJ = j.map(x => f(a + x * h)).sum
  (h/3) * (  f(a) +  (4 * sumI) + (2 * sumJ) +  f(b))
}
integralExt(3, 5, f)
integralExt(0, 2, f2)
integralExt(-1,1, f3)
integralExt(1, 2, f4)
integralExt(0, 1, f5)
integralExt(2, 3, f6)
integralExt(0, 1, f7)
