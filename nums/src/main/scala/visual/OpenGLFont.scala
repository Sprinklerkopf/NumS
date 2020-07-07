package visual

import java.awt.Font
import java.awt.FontMetrics
import java.awt.image.BufferedImage
import numSmath.Vec3
import numSmath.Vec2
import java.awt.RenderingHints
import java.awt.Color
import org.lwjgl.opengl.GL11._
import java.nio.ByteBuffer
import java.awt.Canvas
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq
class OGLChar(c:Char, font:Font){
    private var image:Int = -1 //OpenGL Image
    private var width:Int = -1
    def getWidth() = width
    def getImage() = image
    def calculate(fm:FontMetrics):(Array[Byte], Int, Int) = { //convert BufferedImage with drawn char => parallel to byte array
        if(fm.charWidth(c) <= 0){
            (null, -1, -1)
        }else{
            val bi = new BufferedImage(fm.charWidth(c), fm.getHeight(), BufferedImage.TYPE_INT_ARGB)
            width = fm.charWidth(c)
            val g = bi.createGraphics()
            g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
            g.setFont(font)
            val desc = g.getFontMetrics().getLineMetrics(c.toString(), g).getDescent()
            val baseline = bi.getHeight()/(bi.getHeight()-desc)
            g.setColor(new Color(0,0,0,0))
            g.clearRect(0, 0, bi.getWidth(), bi.getHeight())
            g.setColor(Color.white)
            g.drawString(c.toString(), 0, (bi.getHeight()-desc).toInt-1)
            g.dispose()
            val data = Array.ofDim[Byte](bi.getWidth()*bi.getHeight()*4)
            for(i<-0 to bi.getHeight()-1){
                for(j<-0 to bi.getWidth()-1){
                    val rgba = bi.getRGB(j, bi.getHeight()-1-i)
                    val col = new Color(rgba, true)
                    data((i*bi.getWidth() + j)*4) = col.getRed().toByte //r
                    data((i*bi.getWidth() + j)*4+1) = col.getGreen().toByte //g
                    data((i*bi.getWidth() + j)*4+2) = col.getBlue().toByte   //b
                    data((i*bi.getWidth() + j)*4+3) = ((rgba >> 24) & 0xFF).toByte //a
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
    val fm = new Canvas().getFontMetrics(font)
    val arras = ParSeq.tabulate(128)(i =>{
        val c = new OGLChar(i.toChar, font)
        (c,c.calculate(fm))
    })
    val chars = arras.seq.map(v =>{
        v._1.toGLImage(v._2._1,v._2._2,v._2._3)
        v._1
    })
    def drawString(s:String, pos:Vec2, color:Vec3) = {
        def toFrame(x:Float, y:Float) = ((x/width.toFloat)*2f-1f, (y/height)*2f-1f)
        var posx = 0f
        for(c <- s.toCharArray()){
            val oc = chars(c.toInt)
            val cs = (oc.getWidth/width.toFloat, fm.getHeight()/height.toFloat)
            BasicDrawer.drawImage(oc.getImage(), Vec2.toTupel(pos+new Vec2(posx, 0)), cs) //toFrame(0,oc.getBaseline())._2
            posx += cs._1
        }
    }
}