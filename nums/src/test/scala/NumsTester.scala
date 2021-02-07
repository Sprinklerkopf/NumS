import calc.{InvalidMatrixSizeException, Matrix, NumSMathException, QuadraticMatrix, BetterOperators}
import calc.Frac._
import calc.BetterOperators._
import org.scalatest.funsuite.AnyFunSuite

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
  test("QuadraticMatrix inverse"){
    assertResult(true)(new Matrix(3,3, Seq(
      1/41f, -2/41f, 1/41f,
      -2/41f, -119/41f, 80/41f,
      1/41f, 322/123f, -202/123f
    )).equals(0.0001f)(new Matrix(3,3, Seq.tabulate(9)(i => if(i==0) 42 else i+1)).inverse()))
  }
  test("Matrix shape/transpose/toString"){
    val a = new Matrix(2,2, Seq(3,4,5,6)) //5 6 3 4
    assertResult("[5.0, 3.0,\n6.0, 4.0]")(a.swapLine(0, 1).transpose().toString)
  }
  test("Matrix +/*/equals Matrix"){
    val a = new Matrix(2,2, Seq(1,2,
                                3,4))
    val b = new Matrix(2,2, Seq(2,3,
                                2,3))
    assertResult(new Matrix(2,2, Seq(6, 9, 14, 21)))(a*b)
    assertResult(new Matrix(2,2, Seq(3, 5, 5, 7)))(a+b)
    assertResult(new Matrix(2,3, Seq(5,4,5,11,10,11)))(a*new Matrix(2,3, Seq(1, 2, 1, 2, 1, 2)))
    assertThrows[InvalidMatrixSizeException](a+new Matrix(3,3, Seq(3, 5, 5, 7, 3, 1, 1, 2, 3)))
    assertThrows[InvalidMatrixSizeException](a*new Matrix(3,2, Seq(3, 5, 5, 7, 3, 1)))
    assertResult(false)(a.equals(b))
    assertResult(true)(new Matrix(List(List(1,2), List(3,4))).equals(a))
  }
  test("toFrac"){
    assertResult(5)(toFrac(5/3.0).n)
    assertResult(3)(toFrac(5/3.0).d)

    assertResult(1)(toFrac(2/8.0).n)
    assertResult(4)(toFrac(2/8.0).d)
  }
  test("Better Operators"){
    assertResult(true)(5.000000001 ~= 5.0)
    assertResult(true)(5.0+3.0 ~= 8.0)
    assertResult(true)(5.0**2.0 ~= 25.0)
    assertResult(true)(9.0**(.5) ~= 3.0)
    assertResult(true)(3.0**(-2) ~= 1.0/9.0)
    assertResult(false)(5.1 ~= 5.2)
  }
}
