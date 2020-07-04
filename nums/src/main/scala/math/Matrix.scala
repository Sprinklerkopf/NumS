import java.util

class Matrix (v:List[List[Float]]){
  private val vals:List[List[Float]] = v
  private val m = v.size
  private val n = v(0).size
  def this(m:Int, n:Int, a:Seq[Float]) = {
    this((for(i<-0 to m-1) yield (for(j <-0 to n-1) yield a(i*n+j)).toList).toList)
  }
  def *(mat:Matrix): Matrix ={
      if(mat == null || mat.n != m) throw new IllegalArgumentException("incompatible Matrix size")
      new Matrix( (for(i <- 0 to m-1) yield
        (for(j <- 0 to mat.n-1) yield (0 to n-1).foldLeft(0f)((sum, k) => sum+vals(i)(k)*mat.vals(k)(j))).toList).toList)
  }
  def setIdentity(): Matrix = {
    val l = Array.ofDim[Float](m, n)
    new Matrix((for(i <- 0 to m-1) yield {
      l(i)(i) = 1
      l(i).toList
    }).toList)
  }
  override def toString: String = {
    val s = new StringBuilder("[")
    for(i <- 0 to m-1) for(j <- 0 to n-1) s.append(vals(i)(j) + (if (j != n-1)", " else if(i != m-1) ",\n" else "]"))
    s.toString()
  }
  override def equals(obj: Any):Boolean = obj match{
    case mat:Matrix => if(mat.m != m || mat.n != n) false else mat.vals.zip(vals).forall(c => c._1.zip(c._2).forall(c2 => c2._1 == c2._2))
    case _ => false
  }
}
