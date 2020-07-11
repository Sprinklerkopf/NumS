package visual

import java.awt.Font
import java.awt.FontMetrics
import java.awt.image.BufferedImage
import calc._
import java.awt.RenderingHints
import java.awt.Color
import org.lwjgl.opengl.GL11._
import java.nio.ByteBuffer
import java.awt.Canvas
import scala.language.postfixOps
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
class OGLChar(c:Char, font:Font){
    private var image:Int = -1 //OpenGL Image
    private var width:Int = -1
    def getWidth() = width
    def getImage() = image
    def getChar() = c
    def calculate(fm:FontMetrics):(Array[Byte], Int, Int) = { //convert BufferedImage with drawn char => parallel to byte array
        if(fm.charWidth(c) <= 0){
            (null, -1, -1)
        }else{
            val bi = new BufferedImage(fm.charWidth(c), fm.getHeight(), BufferedImage.TYPE_INT_ARGB)
            width = fm.charWidth(c)
            val g = bi.createGraphics()
            g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
            g.setFont(font)
            val desc = fm.getLineMetrics(c.toString(), g).getDescent()
            val baseline = bi.getHeight()/(bi.getHeight()-desc)
            g.setColor(new Color(0,0,0,0))
            g.fillRect(0, 0, bi.getWidth(), bi.getHeight())
            g.setColor(Color.white)
            g.drawString(c.toString(), 0, (bi.getHeight()-desc).toInt-1)
            g.dispose()
            val t2 = System.currentTimeMillis()
            val data = Array.ofDim[Byte](bi.getWidth()*bi.getHeight()*4)
            for(i<- (0 to bi.getHeight()-1)){
                for(j<- 0 to bi.getWidth()-1){
                    val rgba = bi.getRGB(j, bi.getHeight()-1-i)
                    val col = new Color(rgba, true)
                    data((i*bi.getWidth() + j)*4) = col.getRed().toByte //r
                    data((i*bi.getWidth() + j)*4+1) = col.getGreen().toByte //g
                    data((i*bi.getWidth() + j)*4+2) = col.getBlue().toByte   //b
                    data((i*bi.getWidth() + j)*4+3) = col.getAlpha().toByte //a
                }
            }
            (data, bi.getWidth(), bi.getHeight())
        }
    }
    def toGLImage(data:Array[Byte], width:Int, height:Int): Int = {
        if(data != null){
            image = glGenTextures()
            val bb = ByteBuffer.allocateDirect(4*width*height)
            bb.put(data)
            bb.flip()
            glBindTexture(GL_TEXTURE_2D, image)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, bb)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
        }
        image
        
    }
}
class OGLFont(font:Font, width:Int, height:Int){
    //generate chars parallel
    val fmf = Future { new BufferedImage(1,1,BufferedImage.TYPE_BYTE_GRAY).createGraphics().getFontMetrics(font) }
    var fm:Option[FontMetrics] = None
    val chars = new HashMap[Char, OGLChar]()
    def getHeight() = {
        if(fm == None) fm = Some(Await.result(fmf, 5 second))
        fm.get.getHeight()
    }
    def genAll() = {
        if(fm == None) fm = Some(Await.result(fmf, 5 second))
        val arras = ParSeq.tabulate(128)(i =>{
            val c = new OGLChar(i.toChar, font)
            (c,c.calculate(fm.get))
        })
        for(v <- arras.seq){
            v._1.toGLImage(v._2._1,v._2._2,v._2._3)
            chars.addOne((v._1.getChar(), v._1))
        }
    }
    def drawString(s:String, pos:Vec2, color:Vec4) = {
        def toFrame(x:Float, y:Float) = ((x/width.toFloat)*2f-1f, (y/height)*2f-1f)
        var posx = 0f
        if(fm == None) fm = Some(Await.result(fmf, 5 second))
        for(c <- s.toCharArray()){
            if(!chars.contains(c)){
                val cd = new OGLChar(c, font)
                val ce = cd.calculate(fm.get)
                cd.toGLImage(ce._1, ce._2, ce._3)
                chars.addOne((c, cd))
            }
            val oc = chars(c)
            val cs = (oc.getWidth/width.toFloat, fm.get.getHeight()/height.toFloat)
            val p:Vec2 =  (pos+new Vec2(posx, 0))
            BasicDrawer.drawImage(oc.getImage(), (p.x.toFloat, p.y.toFloat), cs, (color.x.toFloat, color.y.toFloat, color.z.toFloat, color.w.toFloat)) //toFrame(0,oc.getBaseline())._2
            posx += cs._1
        }
    }
}