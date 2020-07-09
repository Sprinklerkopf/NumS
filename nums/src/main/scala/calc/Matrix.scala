package calc
import java.util

class Matrix (v:List[List[Float]]){
  if(v == null || v.size == 0 || v(0).size == 0) throw new InvalidMatrixSizeException("No empty matrices!")
  private val vals:List[List[Float]] = v
  private val m = v.size
  private val n = v(0).size
  def this(m:Int, n:Int, a:Seq[Float]) = {
    this(List.tabulate(m)(i=>List.tabulate(n)(j=>a(i*m+j))))
  }

  def *(mat:Matrix): Matrix ={
      if(mat == null || mat.n != m) throw new IllegalArgumentException("incompatible Matrix size")
      new Matrix(List.tabulate(m)(i => (List.tabulate(mat.n)(j => 
        (0 to n-1).foldLeft(0f)((sum, k) => sum+vals(i)(k)*mat.vals(k)(j))))))
  }
  def *(vec:Vector): Vector = {
      if(vec == null || vec.getValues().size != m) throw new IllegalArgumentException("incompatible Matrix size")
      new Vector(vals.map(_.zip(vec.getValues()).foldLeft(0f)((sum, p) => p._1*p._2 + sum)):_*)
  }
  def *(v:Float): Matrix = new Matrix(vals.map(row => row.map(m => m*v)))
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
object Matrix{
  def identity(size:Int) = {
    if(size <= 0) throw new InvalidMatrixSizeException("No empty matrices!")
    new QuadraticMatrix(List.tabulate(size)(i => List.tabulate(size)(j => if(i == j) 1 else 0)))
  }
  implicit def toQuadratic(mat:Matrix):QuadraticMatrix = new QuadraticMatrix(mat.vals)
}
class QuadraticMatrix(vals:List[List[Float]]) extends Matrix(vals){
  if(vals == null || vals.size < 2) throw new InvalidMatrixSizeException()
  if(!vals.forall(p => p.size == vals.size)) throw new InvalidMatrixSizeException("Matrix not Quadratic")
  def this(m:Int, a:Seq[Float]) = {
    this(List.tabulate(m)(i=>List.tabulate(m)(j=>a(i*m+j))))
  }
  def determinant():Float = {
    if(vals.size == 2){
      vals(0)(0)*vals(1)(1) - vals(0)(1)*vals(1)(0)
    }else if(vals.size == 3){
      vals(0)(0)*vals(1)(1)*vals(2)(2) + vals(0)(1)*vals(1)(2)*vals(2)(0) + vals(0)(2)*vals(1)(0)*vals(2)(1) -
      vals(2)(0)*vals(1)(1)*vals(0)(2) - vals(2)(1)*vals(1)(2)*vals(0)(0) - vals(2)(2)*vals(1)(0)*vals(0)(1)
    }else{ //very unefficient //Laplace says:
      //matrix without first column and i-th row
      def submatrix(i:Int) = {
        val row = vals(i)
        new QuadraticMatrix(vals.filter(p=>p!=row).map(p=>p.drop(1)))
      }
      (0 to vals.size-1).foldLeft(0f)((sum, i) => sum + vals(i)(0)*(if(i%2==0) 1 else -1)*submatrix(i).determinant())
    }
  }
  def inverse():QuadraticMatrix = ???
}