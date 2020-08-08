package visual
import calc._
trait Stuff{}
//var for performance (functional would require to change the rendering list every time)
class Point(var pos:Vec2, var color:(Int,Int,Int) = (0,0,255), var radius:Double=1) extends Stuff{}
