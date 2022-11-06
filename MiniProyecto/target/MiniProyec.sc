print("GSDDS")
def integracion(a:Int, b:Int ,f:Double => Double) : Double= {
  (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}
val f = (x : Double) => -Math.pow(x,2)+(8*x)-12

integracion(3,5,f)
//Pregunta 2
//Aproxima el valor de las siguientes integrales definidas usando la función integracion creada en el paso 1

val h = (x : Double) => 3*Math.pow(x,2)
val i = (x : Double) => x+2*Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4)
val j = (x : Double) => ((2*x+1)/(Math.pow(x,2)+x))
val k = (x : Double) => Math.pow(Math.E,x)
val l = (x : Double) => (1/Math.sqrt(x-1))
val m = (x : Double) => (1/(1+Math.pow(x,2)))


val i1 = integracion(3,5,f)
val i2 = integracion(0,2,h)
val i3 = integracion(-1,1,i)
val i4 = integracion(1,2,j)
val i5 = integracion(0,1,k)
val i6 = integracion(2,3,l)
val i7 = integracion(0,1,m)

//Pregunta 3
def err(a : Double, b : Double) : Double = (a - b).abs

val v01 = 7.33
val v02 = 8
val v03 = 3.333
val v04 = 1.09861
val v05 = 1.71828
val v06 = 0.828427
val v07 = 0.785398

err(i1, v01)
err(i2, v02)
err(i3, v03)
err(i4, v04)
err(i5, v05)
err(i6, v06)
err(i7, v07)