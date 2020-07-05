package visual
import numSmath._
trait Stuff{}
//var for performance (functional would require to change the rendering list every time)
class Point(var pos:Vec2, var color:(Int,Int,Int) = (0,0,255), var radius:Float=1) extends Stuff{}
class MathFunction(val f:(Float => Float), val color:(Int, Int, Int) = (0,0,255), val defspace:Option[(Float, Float)] = None) extends Stuff{}
object MathFunction{
    implicit def toMathFunction(f:(Float => Float)):MathFunction = new MathFunction(f)
}