package visual
import calc._
trait Stuff{}
//var for performance (functional would require to change the rendering list every time)
class Point(var pos:Vec2, var color:(Int,Int,Int) = (0,0,255), var radius:Double=1) extends Stuff{}
class MathFunction(val f:(Double => Double), val color:(Int, Int, Int) = (0,0,255), val defspace:Option[(Double, Double)] = None) extends Stuff{}
object MathFunction{
    implicit def toMathFunction(f:(Double => Double)):MathFunction = new MathFunction(f)
}