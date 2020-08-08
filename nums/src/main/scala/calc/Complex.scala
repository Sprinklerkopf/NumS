package calc

import BetterOperators._
class Complex(val r:Double, val i:Double) {
  def this(v:Double) = this(v, 0)
  def +(c:Complex) = new Complex(r+c.r, i+c.i)
  def -(c:Complex) = new Complex(r-c.r, i-c.i)
  def *(c:Complex) = new Complex(r*c.r - i*c.i, r*c.i + i*c.r)
  def /(c:Complex) = new Complex((r*c.r + i*c.i)/(c.r*c.r + c.i*c.i), (i*c.r - r*c.i)/(c.r*c.r + c.i*c.i))
  def unary_~ = new Complex(r, -i)
  def unary__ : Double = math.sqrt(r*r + i*i)
  override def equals(obj: Any): Boolean = obj match{
    case c:Complex => (c.r ~= r) && (c.i ~= i)
    case x => false
  }
  override def toString: String = (r, i) match{
    case (r, 0) => s"${r}"
    case (r, i) if i >= 0 => s"${r} + ${i}*i"
    case (r, i) if i < 0 => s"${r}${i}*i"
  }
}
object Complex{
  def i = new Complex(0, 1)
  implicit def intToComplex(v:Int):Complex = new Complex(v)
  implicit def doubleToComplex(v:Double):Complex = new Complex(v)
  implicit def floatToComplex(v:Float):Complex = new Complex(v)
  implicit def intTupleToComplex(t:(Float, Float)):Complex = new Complex(t._1, t._2)
  implicit def doubleTupleToComplex(t:(Double, Double)):Complex = new Complex(t._1, t._2)
  implicit def floatTupleToComplex(t:(Float, Float)):Complex = new Complex(t._1, t._2)
}
