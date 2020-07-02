import org.lwjgl.opengl.GL11._
object BasicDrawer {
  def drawPoints(p:((Float, Float), (Float, Float, Float))*) {
    glBegin(GL_POINTS)
    p.foreach(p=>{
      glColor3f(p._2._1, p._2._2, p._2._3)
      glVertex2f(p._1._1, p._1._2)
    })
    glEnd()
  }
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
