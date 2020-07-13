package calc
import java.util

class Matrix (v:List[List[Double]]){
  if(v == null || v.size == 0 || v(0).size == 0) throw new InvalidMatrixSizeException("No empty matrices!")
  private val vals:List[List[Double]] = v
  private val m = v.size
  private val n = v(0).size
  def this(m:Int, n:Int, a:Seq[Double]) = {
    this(List.tabulate(m)(i=>List.tabulate(n)(j=>a(i*n+j))))
  }
  def +(mat:Matrix): Matrix = {
    if(mat == null || mat.m != m || mat.n != n) throw new InvalidMatrixSizeException("incompatible Matrix size")
    new Matrix(v.zip(mat.vals).map(f => f._1.zip(f._2).map(v => v._1 + v._2)))
  }
  def *(mat:Matrix): Matrix ={
      if(mat == null || mat.m != n) throw new InvalidMatrixSizeException("incompatible Matrix size")
      new Matrix(List.tabulate(m)(i => (List.tabulate(mat.n)(j => 
        (0 to n-1).foldLeft(0.0)((sum, k) => sum+vals(i)(k)*mat.vals(k)(j))))))
  }
  def *(vec:Vector): Vector = {
      if(vec == null || vec.getValues().size != n) throw new InvalidMatrixSizeException("incompatible Matrix size")
      new Vector(vals.map(_.zip(vec.getValues()).foldLeft(0.0)((sum, p) => p._1*p._2 + sum)):_*)
  }
  def *(v:Double): Matrix = new Matrix(vals.map(row => row.map(m => m*v)))
  def transpose() : Matrix = new Matrix(List.tabulate(n)(j => List.tabulate(m)(i => vals(i)(j))))
  def swapLine(i:Int, j:Int) = if(i < 0 || i > m || j < 0 || j > n) throw new NonExistingElementException("Line does not exist")
    else new Matrix(List.tabulate(m)(k => if(k == i) vals(j) else if(k == j) vals(i) else vals(k)))
  
  def toFracString(): String = {
    val s = new StringBuilder("[")
    for(i <- 0 to m-1) for(j <- 0 to n-1) s.append(new Frac(vals(i)(j)) + (if (j != n-1)", " else if(i != m-1) ",\n" else "]"))
    s.toString()
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
object Matrix{
  def identity(size:Int) = {
    if(size <= 0) throw new InvalidMatrixSizeException("No empty matrices!")
    new QuadraticMatrix(List.tabulate(size)(i => List.tabulate(size)(j => if(i == j) 1 else 0)))
  }
  implicit def toQuadratic(mat:Matrix):QuadraticMatrix = new QuadraticMatrix(mat.vals)
}
class QuadraticMatrix(vals:List[List[Double]]) extends Matrix(vals){
  if(vals == null || vals.size < 2) throw new InvalidMatrixSizeException()
  if(!vals.forall(p => p.size == vals.size)) throw new InvalidMatrixSizeException("Matrix not Quadratic")
  val m = vals.size
  //indirectly functional, chaching of solution important for performance
  private var det:Option[Double] = None
  def this(m:Int, a:Seq[Double]) = {
    this(List.tabulate(m)(i=>List.tabulate(m)(j=>a(i*m+j))))
  }
  def determinant():Double = {
    det match{
      case None => {
        det = Some(
        if(vals.size == 2){
          vals(0)(0)*vals(1)(1) - vals(0)(1)*vals(1)(0)
        }else if(vals.size == 3){
          vals(0)(0)*vals(1)(1)*vals(2)(2) + vals(0)(1)*vals(1)(2)*vals(2)(0) + vals(0)(2)*vals(1)(0)*vals(2)(1) -
          vals(2)(0)*vals(1)(1)*vals(0)(2) - vals(2)(1)*vals(1)(2)*vals(0)(0) - vals(2)(2)*vals(1)(0)*vals(0)(1)
        }else{ //very unefficient
          //matrix without first column and i-th row
          def submatrix(i:Int) = {
            val row = vals(i)
            new QuadraticMatrix(vals.filter(p=>p!=row).map(p=>p.drop(1)))
          } //Laplace says:
          (0 to vals.size-1).foldLeft(0.0)((sum, i) => sum + vals(i)(0)*(if(i%2==0) 1 else -1)*submatrix(i).determinant())
        })
        det.get
      }
      case Some(value) => det.get
    }
  }
  def inverse():QuadraticMatrix = { //jordan - gauß
    def swap(a:Array[Array[Double]], i:Int, j:Int){
      val temp = a(i)
      a(i) = a(j)
      a(j) = temp
    }
    val d = determinant()
    if(d == 0) throw new MatrixNotInvertible()
    val jor = Array.tabulate(m)(i => Array.tabulate(2*m)(j => if(j < m) vals(i)(j) else if(i == j-m) 1f else 0f))
    //jor => triangular form //Gauß
    for(i <- 0 to m-2){
      if(jor(i)(i) == 0){ 
        var swapped = false
        for(j <- i+1 to m-1) if(jor(j)(i) != 0 && !swapped){
          swap(jor, i, j)
          swapped = true
        }
      }
      for(j <- i+1 to m-1){
        val pivot = jor(j)(i)/jor(i)(i)
        for(k <- i to 2*m -1) jor(j)(k) = jor(j)(k)-jor(i)(k)*pivot
      }
    }
    for(i <- 1 to m-1) if(jor(i)(i) == 0) throw new MatrixNotInvertible()
    //Jordan
    for(i <- m-1 to 1 by -1){
      for(j <- i-1 to 0 by -1){
        val pivot = jor(j)(i)/jor(i)(i)
        for(k <- i to 2*m-1) jor(j)(k) =jor(j)(k) - jor(i)(k)*pivot
      }
    }
    //norm
    new QuadraticMatrix(List.tabulate(m)(i => {
      val pv = jor(i)(i)
      List.tabulate(m)(j => jor(i)(j+m)/pv)
    }))
  }
}