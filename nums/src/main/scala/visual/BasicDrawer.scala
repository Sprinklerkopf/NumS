import org.lwjgl.opengl.GL11._
import java.awt.Font
import javax.swing.JLabel
object BasicDrawer {
  def endDrawing() = glEnd
  //FONT //TODO

  
  //POINTS
  def drawPoints(p:List[((Float, Float), (Float, Float, Float))], radius:Float = 1f) {
    glPointSize(radius)
    glBegin(GL_POINTS)
    p.foreach(p=>{
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }
  def beginDrawingPoints(radius:Float = 1f) = {
      glPointSize(radius)
      glBegin(GL_POINTS)
  }
  def drawPoint(p:((Float, Float), (Float, Float,Float))){
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
  }
  
  //LINES
  def beginDrawingLines() = glBegin(GL_LINES)
  def drawLine(p1:(Float, Float), p2:(Float, Float), col:(Float, Float, Float)) = {
      glColor3f(col._1, col._2, col._3)
      glVertex2f(p1._1, p1._2)
      glVertex2f(p2._1, p2._2)
  }
  def drawLines(lines:List[((Float, Float), (Float, Float), (Float, Float, Float))]){
    glBegin(GL_LINES)
    lines.foreach(p => {
      glColor3f(p._3._1, p._3._2, p._3._3)
      glVertex2f(p._1._1, p._1._2)
      glVertex2f(p._2._1, p._2._2)
    })
    glEnd()
  }
  def drawLineStrip(lines:List[((Float, Float), (Float, Float, Float))]){
    glBegin(GL_LINE_STRIP)
    lines.foreach(p => {
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }
  //POLYGONS
  def drawPolygon(coords:List[((Float, Float), (Float, Float, Float))]){
    glBegin(GL_LINE_LOOP)
    coords.foreach(p => {
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }
  def fillPolygon(coords:List[((Float, Float), (Float, Float, Float))]): Unit ={
    glBegin(GL_POLYGON)
    coords.foreach(p => {
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }

}
