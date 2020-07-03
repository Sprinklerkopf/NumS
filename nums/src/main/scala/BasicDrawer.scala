import org.lwjgl.opengl.GL11._
import java.awt.Font
import javax.swing.JLabel
import org.newdawn.slick.TrueTypeFont
import org.newdawn.slick.Color
object BasicDrawer {
  //FONT
  val deff:Font = new Font(new JLabel().getFont().getName(), Font.PLAIN, 20)
  val deffont:TrueTypeFont = new TrueTypeFont(deff, true)
  //Did i mention how much I love Scala?
  def drawString(s:String, pos:(Float, Float), col:Color = Color.black, size:Int = 20) = if(size == 20) deffont.drawString(pos._1, pos._2, s, col) else
      new TrueTypeFont(new Font(new JLabel().getFont().getName(), Font.PLAIN, size), true).drawString(pos._1, pos._2, s, col)
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
  def endDrawing() = glEnd
  //LINES
  def drawLines(lines:((Float, Float), (Float, Float), (Float, Float, Float))*){
    glBegin(GL_LINES)
    lines.foreach(p => {
      glColor3f(p._3._1, p._3._2, p._3._3)
      glVertex2f(p._1._1, p._1._1)
      glVertex2f(p._2._2, p._2._2)
    })
    glEnd()
  }
  def drawLineStrip(lines:((Float, Float), (Float, Float, Float))*){
    glBegin(GL_LINE_STRIP)
    lines.foreach(p => {
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }
  //POLYGONS
  def drawPolygon(coords:((Float, Float), (Float, Float, Float))* ){
    glBegin(GL_LINE_LOOP)
    coords.foreach(p => {
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }
  def fillPolygon(coords:((Float, Float), (Float, Float, Float))*): Unit ={
    glBegin(GL_POLYGON)
    coords.foreach(p => {
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }

}
