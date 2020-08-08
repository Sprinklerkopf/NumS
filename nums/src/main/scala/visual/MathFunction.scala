package visual

class MathFunction(val f:Double => Double, val color:(Int, Int, Int) = (0,0,255), val defspace:Option[(Double, Double)] = None) extends Stuff{
    def derive(h:Double = 0.00001) = new MathFunction((x:Double) => (f(x+h)-f(x))/h, color, defspace)    
}
object MathFunction{
    implicit def toMathFunction(f:Double => Double):MathFunction = new MathFunction(f)
}