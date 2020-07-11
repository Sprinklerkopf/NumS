import visual._
import calc._
import java.awt.Font
object Main  {
  def main(args:Array[String]){
    val mat1 = new Matrix(List(List(3f,	1f, 42f),
        List(5f,	2f, 69f),
        List(7f, 3f, -402f)))
    println(mat1.inverse())
    val g = new OpenGLGraph()
    val cols = List[(Int,Int,Int)]((0,0,255), (50,255,0), (255,50,0), (250,250,0), (250,0,250))
    for(i<-0 to 500){
      val x = (Math.random()-0.5)*1000
      val y = (Math.random()-0.5)*1000
      g.add(new Point(new Vec2(x.toFloat,y.toFloat), color=cols((Math.random()*cols.size).toInt), radius=3))
    }
    g.add(new MathFunction(x => x*math.sin(x/10f).toFloat))
    g.drawStuff()
  }
}