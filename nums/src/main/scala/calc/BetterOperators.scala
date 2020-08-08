package calc

object BetterOperators {
  implicit class BetterDouble(private val a: Double) extends AnyVal{
    def **(b:Double):Double = b match{
      case 1 => a
      case 2 => a*a
      case 3 => a*a*a
      case n => math.pow(a, n)
    }
    implicit def ~=(b:Double):Boolean = (a-b) <= 0.000001
  }
}