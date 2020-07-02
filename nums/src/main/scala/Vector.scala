class Vector(vals:Float*){
  def this(v:Vector){
    this(v.getValues().map(v=>v):_*)
  }
  /**
   * Adds the Vectors component wise.
   * The size of the bigger Vector (with more components) will be kept.
   * The missing values of the smaller Vector will be filled with ones.
   * @param vec the Vector to be added
   * @return the result of the Addition
   */
  def +(vec:Vector): Vector = new Vector(vals.zipAll(vec.getValues(), 0f,0f).map(x => x._1 + x._2):_*)
  /**
   * Multiplies the Vectors component wise.
   * The size of the bigger Vector (with more components) will be kept.
   * The missing values of the smaller Vector will be filled with ones.
   * @param vec the Vector to be multiplied
   * @return the result of the Multiplication
   */
  def *(vec:Vector): Vector = new Vector(vals.zipAll(vec.getValues(), 1f,1f).map(x => x._1 * x._2):_*)
  /**
    * Multiplies the Vectors components by the given Scalar
    * @param s the Scalar with which this Vector will be sized
    * @return a new Vector, multiplied by s
    */
  def *(s:Float): Vector = new Vector(vals.map(f => f*s):_*)
  /**
    * Returns the length of this Vector by the formula of pythagoras
    * @return Length of this Vector
    */
  def length:Float = math.sqrt(vals.foldLeft(0.0)((p, v) => p + math.pow(v,2))).toFloat
  /**
    * Calculates a new Vector with the same direction as this one but with a length of one
    * @return this vector as a normalized one
    */
  def normalize:Vector = {
    val l = length
    new Vector(vals.map(_/l):_*)
  }
  /**
    * Alternative to String Method
    * @return A String representing the components of this Vector as fractions
    */
  def asFrac():String = {
    val s = vals.foldLeft(new StringBuilder("["))((a,b)=>a.append(VecTools.convDecToFrac(b)+ ", "))
    s.delete(s.size-2, s.size)
    s.append("]")
    s.toString()
  }
  override def toString: String = {
    val s = vals.foldLeft(new StringBuilder("["))((a,b)=>a.append(b+ ", "))
    s.delete(s.size-2, s.size)
    s.append("]")
    s.toString()
  }
  override def equals(obj: Any): Boolean = obj match{
    case a:Vector => if(a.getValues.size ==vals.size) a.getValues.zip(vals).forall(x => x._1 == x._2) else false
    case _ => false
  }
  implicit def getValues() = vals
}
class Vec2(val x:Float,val y:Float) extends Vector(x,y){}
class Vec3(val x:Float,val y:Float,val z:Float) extends Vector(x,y,z){
  def cross:Vec3=>Vec3=a=>new Vec3(a.z*y-z*a.y, z*a.x-x*a.z, x*a.y-y*a.x)
}
class Vec4(val x:Float,val y:Float,val z:Float,val w:Float) extends Vector(x,y,z,w){}
//CASTING
object Vec2{
  implicit def toTupel(vec:Vec2):(Float, Float) = (vec.x,vec.y)
  implicit def toVec2(vec:Vector):Vec2 = new Vec2(vec.getValues()(0), vec.getValues()(1))
}
object Vec3{
  implicit def toTupel(vec:Vec3):(Float, Float, Float) = (vec.x,vec.y,vec.z)
  implicit def toVec3(vec:Vector):Vec3 = new Vec3(vec.getValues()(0), vec.getValues()(1), vec.getValues()(2))
}
object Vec4{
  implicit def toTupel(vec:Vec4):(Float, Float, Float, Float) = (vec.x,vec.y,vec.z,vec.w)
  implicit def toVec4(vec:Vector):Vec4 = new Vec4(vec.getValues()(0), vec.getValues()(1), vec.getValues()(2), vec.getValues()(3))
}
object VecTools{
  //FROM JAVA CODE
  def convDecToFrac(x:Double):String = {
    if(x<0) "-"+convDecToFrac(-x)
    else{
      val tolerance = 1.0E-6
      var h1=1.0
      var h2=0.0
      var k1=0.0
      var k2=1.0
      var b = x
      do {
          val a = Math.floor(b)
          var aux = h1 
          h1 = a*h1+h2
          h2 = aux
          aux = k1
          k1 = a*k1+k2
          k2 = aux
          b = 1.0/(b-a)
      } while (Math.abs(x-h1/k1) > x*tolerance);
      h1+"/"+k1;
    }
  }
}


