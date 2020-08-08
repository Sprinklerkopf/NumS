package calc

object Frac {
    @scala.annotation.tailrec
    def gcd(a:Int, b:Int):Int = (a,b) match{
        case (a, 0) => a
        case (a, b) => gcd(b, a % b)
    }
    implicit def toFrac(x:Double, error:Double=0.0001):Frac = {
        val v = math.floor(x).toInt
        var lower = (0,1)
        var higher = (1,1)
        while(true){
            val middle = (lower._1 + higher._1, lower._2 + higher._2)
            if(math.abs(v+middle._1/middle._2.toDouble - x) < error) 
                return new Frac(v*middle._2 + middle._1, middle._2)
            if((v*middle._2 + middle._1)/middle._2.toDouble < x) lower = middle
            else  higher = middle
        }
        null
    }
    implicit def toDouble(x:Frac):Double = x.value()
}
class Frac(num:Int, den:Int){
    val (n, d) = {
        val g = Frac.gcd(num, den)
        (num/g, den/g)
    }
    def this(v:Frac){
        this(v.n, v.d)
    }
    def this(x:Double){
        this(Frac.toFrac(x))
    }
    def +(f:Frac) = new Frac(n*f.d + d*f.n, d*f.d) 
    def *(f:Frac) = new Frac(n*f.n, d*f.d)
    def /(f:Frac) = new Frac(n/f.n, d/f.d)
    def value(): Double = n/d.toDouble
    override def toString: String = num+"/"+den
}