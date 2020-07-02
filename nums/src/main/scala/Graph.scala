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


trait Graph extends JFrame{
    protected var stuff:List[Stuff] = List.empty
    def add(s:Stuff){
        this.synchronized{
            stuff = s+:stuff
        }
    }
    def remove(s:Stuff){
        this.synchronized{
            stuff = stuff.filter(_ != s)
        }
    }
    def drawStuff
}
class Graph2D extends Graph{
    private var x_space = (-5f, 5f)
    private var y_space = (-5f, 5f)
    setTitle("NumS - 2 dimensional Graph")
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    private  val ge = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice()
    private val sz = ge.getDisplayMode()
    private val bd = ge.getDefaultConfiguration().getBounds()
    private val ks = if(sz.getWidth() > sz.getHeight()) sz.getHeight() else sz.getWidth()
    setBounds(bd.x + sz.getWidth/2 - ks/4, bd.y + sz.getHeight/2 - ks/4, ks/2, ks/2)
    private var puff:BufferedImage = null
    private var g:Graphics2D = null
    repaint()
    setVisible(true)
    /***************************/
    private def upstream(n:Float = 0f):Stream[(Float, Float)] = n match{
        case x if(x == 0) => (0f,1f)#::upstream(1)
        case x if(x == 1) => (1f,2f)#::upstream(2)
        case x if(x == 2) => (2f,5f)#::upstream(5)
        case x => (x,x+5)#::upstream(x+5)
    }
    private lazy val stru = upstream()
    private def downstream(n:Float = 1f):Stream[(Float, Float)] = n match{
        case x => Stream((x, x/2f), (x/2f, x/10f))#:::downstream(n/10f)
    } 
    private lazy val strd = downstream()
    private def getStep(steps:Int, range:(Float, Float)):Float = {
        val w = range._2 - range._1
        if(math.abs(w/steps) >= 1f)
            stru.takeWhile(p=>Math.abs(w-p._1*steps) > Math.abs(w-p._2*steps)).last._2
        else
            strd.takeWhile(p=>Math.abs(w-p._1*steps) > Math.abs(w-p._2*steps)).last._2
    }
    /***************************/
    private def toFrameSpace(p:(Float,Float)):(Int, Int) = ((((p._1-x_space._1)/(x_space._2-x_space._1))*getWidth()).toInt, 
                                getHeight()-(((p._2-y_space._1)/(y_space._2-y_space._1))*(getHeight()-30)).toInt)
    private def toDiagramSpace(p:(Int,Int)):(Float, Float) = (x_space._1 + (p._1.toFloat/getWidth())*(x_space._2-x_space._1), 
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
            g.drawLine(x, midp._2 - 3, x, midp._2 + 3)
            g.drawString(""+(x_space._1 + stepx*i), x-5, midp._2 + (if(i%2==0) -3 else 12))
            i+=1
        }
        i = 1
        while(y_space._1+stepy*i < y_space._2){
            val y = toFrameSpace(0, y_space._1+stepy*i)._2
            g.drawLine(midp._1 - 3, y, midp._1 + 3, y)
            if(Math.abs(y-midp._2) >= 20) g.drawString(""+(y_space._1 + stepy*i), midp._1 + (if(i%2==0) -25 else 3), y)
            i+=1
        }
        g.drawLine(midp._1, 0, midp._1, getHeight())
        g.drawLine(0, midp._2, getWidth(), midp._2)

        this.synchronized{
            for(x<-stuff) x match{
                case p: Point => {
                    g.setColor(new Color(p.color._1, p.color._2, p.color._3))
                    val pos = toFrameSpace(p.pos.x-p.radius, p.pos.y-p.radius)
                    g.fillArc(pos._1, pos._2, (p.radius*2).toInt, (p.radius*2).toInt, 0, 360)
                }
                case f: MathFunction =>{ 
                    g.setColor(new Color(f.color._1, f.color._2, f.color._3))
                    var old = (0, toFrameSpace(0, f.f(toDiagramSpace(0,0)._1))._2)
                    val div = if(getWidth() > 1000) 5 else 1
                    for(i<-0 to getWidth()/div){
                        val n = (i*div, toFrameSpace(0, f.f(toDiagramSpace(i*div,0)._1))._2)
                        g.drawLine(old._1, old._2, n._1, n._2)
                        old = n
                    }
                }
            }
        }
        repaint()
    }
    private def normalize(f:Float) = if(f == 0) 0 else
        if(f < 1 && f > 0)
            strd.takeWhile(p=>Math.abs(f-p._1) > Math.abs(f-p._2)).last._2
        else if(f >= 1)
            stru.takeWhile(p=>Math.abs(f-p._1) > Math.abs(f-p._2)).last._2+5
        else -stru.takeWhile(p=>Math.abs((-f)-p._1) > Math.abs((-f)-p._2)).last._2
    override def add(s:Stuff){
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
                            y_space = (0 to getWidth()).foldLeft(y_space)((old, curr) => {
                                val x = toDiagramSpace(curr,0)._1
                                if(x >= s._1 && x <= s._2){
                                    val y = f.f(x)
                                    if(y < old._1) (normalize(y), old._2)
                                    else if(y > old._2)(old._1, normalize(y)) 
                                    else old
                                }else old
                            })
                        }
                        case None => y_space = (0 to getWidth()).foldLeft(y_space)((old, curr) => {
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
}