package visual

import java.awt.Font
import java.awt.FontMetrics
import java.awt.image.BufferedImage
import numSmath.Vec3
import numSmath.Vec2

class OGLChar(c:Char, font:Font){
    var image:Int = -1 //OpenGL Image
    def calculate(fm:FontMetrics):BufferedImage = ??? //calculate BufferedImage with drawn char => parallel
    def toGLImage(img:BufferedImage): Unit = ??? //set Image
}
class OGLFont(font:Font){
    //TODO: generates chars
    
    def drawString(s:String, pos:Vec2, color:Vec3) = ???
}