object Main  {
  def main(args:Array[String]){
    val d = new Graph2D()
    val cols = List[(Int,Int,Int)]((0,0,255), (50,255,0), (255,50,0), (250,250,0), (250,0,250))
    for(i<-0 to 500){
      val x = (Math.random()-0.5)*1000
      val y = (Math.random()-0.5)*1000
      d.add(new Point(new Vec2(x.toFloat,y.toFloat), color=cols((Math.random()*cols.size).toInt), radius=2))
    }
    d.add(new MathFunction(x=>(math.sin(x/20)*x*2).toFloat))
    d.drawStuff
  } 
}