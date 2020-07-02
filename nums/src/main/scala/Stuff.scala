trait Stuff{}
class Point(val pos:Vec2, val color:(Int,Int,Int) = (0,0,255), val radius:Float=1) extends Stuff{}
class MathFunction(val f:(Float => Float), val color:(Int, Int, Int) = (0,0,255), val defspace:Option[(Float, Float)] = None) extends Stuff{}
object MathFunction{
    implicit def toMathFunction(f:(Float => Float)):MathFunction = new MathFunction(f)
}