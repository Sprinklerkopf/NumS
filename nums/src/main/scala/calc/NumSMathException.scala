package calc

class NumSMathException(s:String) extends RuntimeException(s){}
class InvalidMatrixSizeException(s:String) extends NumSMathException(s){
    def this() = this("Invalid Matrix Size!")
}
class NonExistingElementException(s:String) extends NumSMathException(s){
    def this() = this("Element does not exist!")
}
class MatrixNotInvertible(s:String) extends NumSMathException(s){
    def this() = this("Matrix is not invertible (determinant probably 0)!")
}