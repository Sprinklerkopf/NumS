import calc.{Matrix, NumSMathException}
import org.scalatest.funsuite.AnyFunSuite

class NumsTester extends AnyFunSuite{
  test("Empty Matrix"){
    assertThrows[NumSMathException](new Matrix(null))
    assertThrows[NumSMathException](new Matrix(List()))
    assertThrows[NumSMathException](new Matrix(List(List())))
  }
  test("Matrix constructors + getValue/apply"){
    val a = new Matrix(List(List(0,1), List(2,3)))
    for(i <- 0 until 4)
      assertResult(i)(a(i/2)(i%2))
    val b = new Matrix(2, 2, Seq(0,1,2,3))
    for(i <- 0 until 4)
      assertResult(i)(b(i/2)(i%2))
  }
  test("Matrix Determinant"){
    val a = new Matrix(2,2, Seq(1,2,3,4))
    assertResult(-2)(a.determinant())
  }
}
