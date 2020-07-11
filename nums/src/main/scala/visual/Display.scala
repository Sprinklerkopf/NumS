package visual
import calc._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.system.MemoryUtil._
import org.lwjgl.opengl.GL11._
class GLFWException(cause:String) extends RuntimeException(cause){
  def this(){
    this("")
  }
}
class Display(title:String, w:Int, h:Int){
  private var nfsw: Int = w
  private var nfsh:Int = h

  private var old = System.currentTimeMillis()
  private var frametime = 0L
  private var fullscreen = false

  if (!glfwInit()) {
    throw new GLFWException("Could not initialize GLFW")
  }
  private var monitor = glfwGetPrimaryMonitor()
  private var width = if(w < 0)(glfwGetVideoMode(monitor).width()/1.5).toInt else w
  private var height = if(h < 0)(glfwGetVideoMode(monitor).height()/1.5).toInt else h
  def getSize():(Int,Int) = (width, height)
  GLFWErrorCallback.createPrint(System.out).set()
  glfwDefaultWindowHints
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)
  glfwWindowHint(GLFW_MAXIMIZED, GLFW_FALSE)
  glfwWindowHint(GLFW_STENCIL_BITS, 4)
  glfwWindowHint(GLFW_SAMPLES, 4)
  val window:Long = glfwCreateWindow(width, height, title, NULL, NULL)
  if(window == NULL) throw new GLFWException("Could not create Window")
  val wx = Array.ofDim[Int](1)
  val wy = Array.ofDim[Int](1)
  glfwGetMonitorPos(monitor, wx, wy)
  glfwSetWindowPos(window, wx(0) + glfwGetVideoMode(monitor).width()/2-width/2, wy(0) + glfwGetVideoMode(monitor).height()/2-height/2)
  glfwMakeContextCurrent(window)
  glfwSwapInterval(1)
  GL.createCapabilities()
  glViewport(0,0, width, height)

  def show() = glfwShowWindow(window)

  def this(title:String) = this(title, -1, -1)
  def shouldClose = glfwWindowShouldClose(window)
  def setClearColor(c: Vec3) = glClearColor(c.x.toFloat, c.y.toFloat, c.z.toFloat, 1.0f)
  def destroy(){
    glfwDestroyWindow(window)
    glfwTerminate()
  }
  def getWindowId() = window
  def changeSize(width:Int, height:Int){
    glfwSetWindowSize (window, width, height)
    this.width = width
    this.height = height
    this.nfsw = width
    this.nfsh = height
    glViewport(0, 0, width, height)
  }
  def getFrameTime() = frametime
  def clear(): Unit ={
    frametime = System.currentTimeMillis()-old
    old = System.currentTimeMillis()
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
  }
  def update(): Unit = {
    glfwSwapBuffers(window)
    glfwPollEvents()
  }
  def toogleFullscreen(): Unit = {
    val mode = glfwGetVideoMode(monitor)
    if(!fullscreen) {
      nfsw = width
      nfsh = height
      width = mode.width()
      height = mode.height()
      fullscreen = true
      glfwSetWindowMonitor(window, monitor, 0, 0, mode.width(), mode.height(), 0)
    }else{
      width = nfsw
      height = nfsh
      fullscreen = false
      glfwSetWindowMonitor(window, monitor, mode.width()/2-width/2, mode.height()/2-height/2, width, height, 0)
    }
    glViewport(0, 0, width, height)
  }
  def getMonitors() : List[(String, Long)] = {
    val mon = glfwGetMonitors()
    List.tabulate(mon.limit())(i =>{
      val m = mon.get(i)
      val mode = glfwGetVideoMode(m)
      (glfwGetMonitorName(m)+" [" + mode.width() + "x" +mode.height() + "]", m)
    })
  }
  def setMonitor(mon:Long) {
    monitor = mon
    nfsw = (glfwGetVideoMode(monitor).width()/1.5).toInt
    nfsh = (glfwGetVideoMode(monitor).width()/1.5).toInt
    val mode = glfwGetVideoMode(monitor)
    if(fullscreen){
      width = mode.width()
      height = mode.height()
      glfwSetWindowMonitor(window, monitor, 0, 0, mode.width(), mode.height(), 0)
    }else{
      width = nfsw
      height = nfsh
      glfwSetWindowMonitor(window, monitor, mode.width()/2-width/2, mode.height()/2-height/2, width, height, 0)
    }
    glViewport(0, 0, width, height)
  }
}
