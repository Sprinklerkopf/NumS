package calc

class NumSMathException(s:String) extends RuntimeException(s){}
class InvalidMatrixSizeException(s:String) extends NumSMathException(s){
    def this() = this("Invalid Matrix Size!")
}