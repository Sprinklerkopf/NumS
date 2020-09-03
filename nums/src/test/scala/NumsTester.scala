import calc.{Matrix, NumSMathException}
import org.scalatest.funsuite.AnyFunSuite
import calc.InvalidMatrixSizeException

class NumsTester extends AnyFunSuite{
  test("Empty Matrix"){
    assertThrows[NumSMathException](new Matrix(null))
    assertThrows[NumSMathException](new Matrix(List()))
    assertThrows[NumSMathException](new Matrix(List(List())))
  }
  test("Non QuadraticMatrix determinant"){
    val a = new Matrix(List(List(3,4,5), List(1,2,3)))
    assertThrows[NumSMathException](a.determinant())
  }
  test("Matrix constructors + apply"){
    val a = new Matrix(List(List(0,1), List(2,3)))
    for(i <- 0 until 4)
      assertResult(i)(a(i/2)(i%2))
    val b = new Matrix(2, 2, Seq(0,1,2,3))
    for(i <- 0 until 4)
      assertResult(i)(b(i/2)(i%2))
  }
  test("QuadraticMatrix determinant"){
    val a = new Matrix(2,2, Seq(1,2,3,4))
    assertResult(-2)(a.determinant())
    val b = new Matrix(3,3, Seq(4, 5, 1,
                                1, 2, 1,
                                1, 1, 2))
    assertResult(6)(b.determinant())
    val c = new Matrix(5,5, Seq(1, 0, 0, 0, 2,
                                0, 1, 0, 2, 0,
                                0, 0, 1, 0, 0,
                                0, 2, 0, 1, 0,
                                2, 0, 0, 0, 1)) //1-
    assertResult(9)(c.determinant())
  }
  test("Matrix shape/transpose/toString"){
    val a = new Matrix(2,2, Seq(3,4,5,6)) //5 6 3 4
    assertResult("[5, 3,\n6, 4]")(a.swapLine(0,1).transpose.toString)
  }
  test("Matrix + * Matrix"){
    val a = new Matrix(2,2, Seq(1,2,
                                3,4))
    val b = new Matrix(2,2, Seq(2,3,
                                2,3))
    assertResult(new Matrix(2,2, Seq(11, 16, 11, 16)))(a*b)
    assertResult(new Matrix(2,2, Seq(3, 5, 5, 7)))(a+b)
    assertResult(new Matrix(2,2, Seq(5,4,5,11,10,11))(a*new Matrix(2,3, Seq(1, 2, 1, 
                                                                            2, 1, 2)))
    assertThrows[InvalidMatrixSizeException](a+new Matrix(3,3, Seq(3, 5, 5, 7, 3, 1, 1, 2, 3)))
    assertThrows[InvalidMatrixSizeException](a*new Matrix(3,2, Seq(3, 5, 5, 7, 3, 1)))
  }
}
