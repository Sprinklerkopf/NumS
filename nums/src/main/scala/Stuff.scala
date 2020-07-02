trait Stuff{}
class Point(var pos:Vec2, var color:(Int,Int,Int) = (0,0,255), var radius:Float=1) extends Stuff{}
class MathFunction(val f:(Float => Float), var color:(Int, Int, Int) = (0,0,255), var defspace:Option[(Float, Float)] = None) extends Stuff{}
object MathFunction{
    implicit def toMathFunction(f:(Float => Float)):MathFunction = new MathFunction(f)

}