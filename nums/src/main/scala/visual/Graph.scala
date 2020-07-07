package visual
import numSmath._
import javax.swing._
import java.awt.Toolkit
import java.awt.GraphicsEnvironment
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Graphics
import java.awt.Color
import java.awt.RenderingHints
import java.awt.Font
import scala.collection.parallel.CollectionConverters._


trait Graph{
    //INTERFACING
    protected var stuff:List[Stuff] = List.empty
    def drawStuff():Unit = ???
    protected var x_space = (-5f, 5f)
    protected var y_space = (-5f, 5f)
    protected def toFrameSpace(p:(Float,Float)):(Float, Float) = ???
    protected def toDiagramSpace(p:(Float,Float)):(Float, Float) = ???
    protected def getWindowSize():(Int, Int) = ???
    //PROGRAMMING
    protected def upstream(n:Float = 0f):Stream[(Float, Float)] = n match{
        case x if(x == 0) => (0f,1f)#::upstream(1)
        case x if(x == 1) => (1f,2f)#::upstream(2)
        case x if(x == 2) => (2f,5f)#::upstream(5)
        case x => (x,x+5)#::upstream(x+5)
    }
    protected lazy val stru = upstream()
    protected def downstream(n:Float = 1f):Stream[(Float, Float)] = n match{
        case x => Stream((x, x/2f), (x/2f, x/10f))#:::downstream(n/10f)
    } 
    protected lazy val strd = downstream()
    protected def getStep(steps:Int, range:(Float, Float)):Float = {
        val w = range._2 - range._1
        if(math.abs(w/steps) >= 1f)
            stru.takeWhile(p=>Math.abs(w-p._1*steps) > Math.abs(w-p._2*steps)).last._2
        else
            strd.takeWhile(p=>Math.abs(w-p._1*steps) > Math.abs(w-p._2*steps)).last._2
    }
    protected def normalize(f:Float) = if(f == 0) 0 else
        if(f < 1 && f > 0)
            strd.takeWhile(p=>Math.abs(f-p._1) > Math.abs(f-p._2)).last._2
        else if(f >= 1)
            stru.takeWhile(p=>Math.abs(f-p._1) > Math.abs(f-p._2)).last._2+5
        else -stru.takeWhile(p=>Math.abs((-f)-p._1) > Math.abs((-f)-p._2)).last._2
    def add(s:Stuff){
        this.synchronized{
            stuff = s+:stuff
            s match{
                case p:Point => {
                    val edge = ((p.pos.x - p.radius, p.pos.x + p.radius), (p.pos.y - p.radius, p.pos.y + p.radius))
                    val changed = (edge._1._1 < x_space._1 || stuff.size == 1, edge._1._2 > x_space._2|| stuff.size == 1, 
                      edge._2._1 < y_space._1|| stuff.size == 1, edge._2._2 > y_space._2 || stuff.size == 1)
                    if(changed._1) x_space = (normalize(edge._1._1), x_space._2)
                    if(changed._2) x_space = (x_space._1, normalize(edge._1._2))
                    if(changed._3) y_space = (normalize(edge._2._1), y_space._2)
                    if(changed._4) y_space = (y_space._1, normalize(edge._2._2))
                }
                case f:MathFunction => {
                    f.defspace match{
                        case d:Some[(Float, Float)] => {
                            val s = d.value
                            if(s._1 > s._2) throw new IllegalArgumentException("MathFunction with illegal interval!")
                            if(s._1 < x_space._1) x_space = (normalize(s._1), x_space._2)
                            if(s._2 > x_space._2) x_space = (x_space._1, normalize(s._2))
                            y_space = (0 to getWindowSize()._1).foldLeft(y_space)((old, curr) => {
                                val x = toDiagramSpace(curr,0)._1
                                if(x >= s._1 && x <= s._2){
                                    val y = f.f(x)
                                    if(y < old._1) (normalize(y), old._2)
                                    else if(y > old._2)(old._1, normalize(y)) 
                                    else old
                                }else old
                            })
                        }
                        case None => y_space = (0 to getWindowSize()._1).foldLeft(y_space)((old, curr) => {
                                val x = toDiagramSpace(curr,0)._1
                                val y = f.f(x)
                                if(y < old._1) (normalize(y), old._2)
                                else if(y > old._2)(old._1, normalize(y)) 
                                else old
                            })
                    }
                }
                case x => throw new IllegalArgumentException("That's not my stuff!")
            }
        }
    }
    def remove(s:Stuff){
        this.synchronized{
            stuff = stuff.filter(_ != s)
        }
    }
   
}
class OpenGLGraph extends Thread with Graph{
    @volatile
    private var update = true
    private var w = 0
    private var h = 0
    start()
    override def run(){
        val font:OGLFont = new OGLFont(new Font("Arial", Font.PLAIN, 20), 1300, 800)
        val display = new Display("NumS - 2 dimensional Graph", 1300, 800)
        display.setClearColor((1f,1f,1f))
        display.clear()
        display.show()
        while(!display.shouldClose){
            w = display.getSize()._1
            h = display.getSize()._2
            if(update){
                update = false
                display.clear()
                val midp = toFrameSpace(0,0)
                val numx = w/20
                val numy = h/20
                val stepx = getStep(numx, range = x_space)
                val stepy = getStep(numy, range = y_space)
                val nums = (math.ceil((x_space._2-x_space._1)/stepx).toInt, math.ceil((y_space._2-y_space._1)/stepy).toInt)
                val linewidth = 0.007f
                val fh = (font.getHeight()/display.getSize()._2.toFloat)
                for (i <- 1 to Math.max(nums._1, nums._2)){
                    val (x,y) = toFrameSpace(x_space._1 + i*stepx  - stepx/2f, y_space._1 + i*stepy)
                    if(Math.abs(x) > 0.05f)
                        font.drawString(""+(x_space._1 + i*stepx), (x, midp._2- (if(i%2==0)linewidth + fh else -linewidth)), (0f,0f,0f, 1f))
                    if(Math.abs(y) > 0.05f)
                        font.drawString(""+(y_space._1 + i*stepy), (midp._1- (if(i%2==0)linewidth+0.045f else -linewidth), y-fh/2f), (0f,0f,0f, 1f))
                }
                val lines = ((-1f, midp._2), (1f, midp._2), (0f, 0f, 0f)) :: ((midp._1, -1f), (midp._1, 1f), (0f, 0f, 0f)) :: 
                (1 to Math.max(nums._1, nums._2)).par.map(i =>{
                    val (x,y) = toFrameSpace(x_space._1 + i*stepx, y_space._1 + i*stepy)
                    var l = List[((Float, Float), (Float, Float), (Float, Float, Float))]()
                    if(i<nums._1){
                        l = ((x, midp._2 - linewidth), (x, midp._2 + linewidth), (0f, 0f, 0f))::l
                    }
                    if(i<nums._2)
                        l = ((midp._1 - linewidth, y), (midp._1 + linewidth, y), (0f, 0f, 0f))::l
                    l
                }).flatten.toList
                BasicDrawer.drawLines(lines)
                var currentSize = -1f
                for(po <- stuff if(po.isInstanceOf[Point])){
                    val p:Point = po.asInstanceOf[Point]
                    if(currentSize < 0){
                        BasicDrawer.beginDrawingPoints(p.radius)
                        currentSize = p.radius
                    }else if(currentSize != p.radius){
                        BasicDrawer.endDrawing()
                        BasicDrawer.beginDrawingPoints(p.radius)
                        currentSize = p.radius
                    }
                    BasicDrawer.drawPoint((toFrameSpace(p.pos), (p.color._1/255f, p.color._2/255f, p.color._3/255f)))
                } 
                BasicDrawer.endDrawing()
                for(so <- stuff if(so.isInstanceOf[MathFunction])){
                    val f = so.asInstanceOf[MathFunction]
                    BasicDrawer.beginDrawingLines()
                    var old = (-1f, toFrameSpace(0, f.f(toDiagramSpace(-1f,0)._1))._2)
                    for(i<-0 to getWindowSize()._1){
                        val x = (i.toFloat/getWindowSize()._1 * 2f) - 1f
                        val n = (x, toFrameSpace(0, f.f(toDiagramSpace(x,0)._1))._2)
                        BasicDrawer.drawLine(old, n, (f.color._1/255f, f.color._2/255f, f.color._3/255f))
                        old = n
                    }
                    BasicDrawer.endDrawing()
                }
            }
            display.update()
        }
        display.destroy()
    }
    override def drawStuff:Unit = {
        update = true
    }
    //xspace, yspace => (-1, 1), (-1, 1)
    override protected def toFrameSpace(p:(Float,Float)):(Float, Float) = (((p._1 - x_space._1)/(x_space._2 - x_space._1))*2f - 1f,
                                                                           ((p._2 - y_space._1)/(y_space._2 - y_space._1))*2f - 1f)
    override protected def toDiagramSpace(p:(Float,Float)):(Float, Float) = (((p._1 + 1f)/2f)*(x_space._2 - x_space._1) + x_space._1, 
                                                                             ((p._2 + 1f)/2f)*(y_space._2 - y_space._1) + y_space._1)
    override protected def getWindowSize():(Int, Int) = (w,h)
}
class SwingGraph extends JFrame with Graph{
    setTitle("NumS - 2 dimensional Graph")
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    private val ge = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice()
    private val sz = ge.getDisplayMode()
    private val bd = ge.getDefaultConfiguration().getBounds()
    private val ks = if(sz.getWidth() > sz.getHeight()) sz.getHeight() else sz.getWidth()
    setBounds(bd.x + sz.getWidth/2 - ks/4, bd.y + sz.getHeight/2 - ks/4, ks/2, ks/2)
    private var puff:BufferedImage = null
    private var g:Graphics2D = null
    repaint()
    setVisible(true)
    /***************************/
    
    /***************************/
    override protected def getWindowSize(): (Int, Int) = (getWidth(), getHeight())
    override def toFrameSpace(p:(Float,Float)):(Float, Float) = ((((p._1-x_space._1)/(x_space._2-x_space._1))*getWidth()).toInt, 
                                getHeight()-(((p._2-y_space._1)/(y_space._2-y_space._1))*(getHeight()-30)).toInt)
    override def toDiagramSpace(p:(Float,Float)):(Float, Float) = (x_space._1 + (p._1.toFloat/getWidth())*(x_space._2-x_space._1), 
                                y_space._1 + (1-(p._2.toFloat)/getHeight())*(y_space._2-y_space._1))
    override def paint(x:Graphics){
        if(puff == null || g == null || puff.getWidth() != getWidth() || puff.getHeight() != getHeight()){
            puff = createImage(getWidth(), getHeight()).asInstanceOf[BufferedImage]
            g = puff.getGraphics().asInstanceOf[Graphics2D]
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            g.setFont(new Font("SansSerif", Font.PLAIN, 9))
        }
        drawStuff
        x.drawImage(puff, 0, 0, getWidth(), getHeight(), this)
    }
    override def drawStuff: Unit = {
        g.setColor(Color.white)
        g.fillRect(0,0,getWidth(),getHeight())
        //TODO: draw the grid
        g.setColor(new Color(50, 50, 50))
        val midp = toFrameSpace(0,0)
        val numx = getWidth()/20
        val numy = getHeight()/20
        val stepx = getStep(numx, range = x_space)
        val stepy = getStep(numy, range = y_space)
        var i = 1
        while(x_space._1+stepx*i < x_space._2){
            val x = toFrameSpace(x_space._1+stepx*i, 0)._1
            g.drawLine(x.toInt, midp._2.toInt - 3, x.toInt, midp._2.toInt + 3)
            g.drawString(""+(x_space._1 + stepx*i), x-5, midp._2 + (if(i%2==0) -3 else 12))
            i+=1
        }
        i = 1
        while(y_space._1+stepy*i < y_space._2){
            val y = toFrameSpace(0, y_space._1+stepy*i)._2
            g.drawLine(midp._1.toInt - 3, y.toInt, midp._1.toInt + 3, y.toInt)
            if(Math.abs(y-midp._2) >= 20) g.drawString(""+(y_space._1 + stepy*i), midp._1 + (if(i%2==0) -25 else 3), y)
            i+=1
        }
        g.drawLine(midp._1.toInt, 0, midp._1.toInt, getHeight())
        g.drawLine(0, midp._2.toInt, getWidth(), midp._2.toInt)

        this.synchronized{
            for(x<-stuff) x match{
                case p: Point => {
                    g.setColor(new Color(p.color._1, p.color._2, p.color._3))
                    val pos = toFrameSpace(p.pos.x-p.radius, p.pos.y-p.radius)
                    g.fillArc(pos._1.toInt, pos._2.toInt, (p.radius*2).toInt, (p.radius*2).toInt, 0, 360)
                }
                case f: MathFunction =>{ 
                    g.setColor(new Color(f.color._1, f.color._2, f.color._3))
                    var old = (0, toFrameSpace(0, f.f(toDiagramSpace(0,0)._1))._2)
                    val div = if(getWidth() > 1000) 5 else 1
                    for(i<-0 to getWidth()/div){
                        val n = (i*div, toFrameSpace(0, f.f(toDiagramSpace(i*div,0)._1))._2)
                        g.drawLine(old._1, old._2.toInt, n._1, n._2.toInt)
                        old = n
                    }
                }
            }
        }
        repaint()
    }
}