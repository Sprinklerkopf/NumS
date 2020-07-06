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
class OGLChar(c:Char, font:Font){
    var image:Int = -1 //OpenGL Image
    def calculate(fm:FontMetrics):(Array[Byte], Int, Int) = { //convert BufferedImage with drawn char => parallel to byte array
        val bi = new BufferedImage(fm.charWidth(c), fm.getHeight(), BufferedImage.TYPE_INT_ARGB)
        val g = bi.createGraphics()
        g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
        g.setFont(font)
        val desc = g.getFontMetrics().getLineMetrics(c.toString(), g).getDescent()
        val baseline = bi.getHeight()/(bi.getHeight()-desc)
        g.setColor(new Color(0,0,0,0))
        g.fillRect(0, 0, bi.getWidth(), bi.getHeight())
		g.setColor(Color.white)
		g.drawString(c.toString(), 0, (bi.getHeight()-desc).toInt-1)
        g.dispose()
        (Array.tabulate(bi.getWidth, bi.getHeight)((i, j) => {
            val c = new Color(bi.getRGB(i, j), true)
            Array[Byte](c.getRed().toByte, c.getGreen().toByte, c.getBlue().toByte, c.getAlpha().toByte)
        }).flatten.flatten, bi.getWidth(), bi.getHeight())
    }
    def toGLImage(data:Array[Byte], width:Int, height:Int): Int = {
        image = glGenTextures()
        val bb = ByteBuffer.allocateDirect(4*width*height)
        bb.put(data)
        bb.flip()
        glBindTexture(GL_TEXTURE_2D, image)
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, bb)
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
        image
    }
}
class OGLFont(font:Font){
    //TODO: generates chars
    
    def drawString(s:String, pos:Vec2, color:Vec3) = ???
}