package calc
class Vector(vals:Double*){
  def this(v:Vector){
    this(v.getValues.map(v=>v):_*)
  }
  /**
   * Adds the Vectors component wise.
   * The size of the bigger Vector (with more components) will be kept.
   * The missing values of the smaller Vector will be filled with zeros.
   * @param vec the Vector to be added
   * @return the result of the Addition
   */
  def +(vec:Vector): Vector = new Vector(vals.zipAll(vec.getValues, 0.0,0.0).map(x => x._1 + x._2):_*)
  /**
   * Substracts the Vectors component wise.
   * The size of the bigger Vector (with more components) will be kept.
   * The missing values of the smaller Vector will be filled with zeros.
   * @param vec the Vector to be substracted
   * @return the result of the Substraction
   */
  def -(vec:Vector): Vector = new Vector(vals.zipAll(vec.getValues, 0.0,0.0).map(x => x._1 - x._2):_*)
  /**
   * Multiplies the Vectors component wise.
   * The size of the bigger Vector (with more components) will be kept.
   * The missing values of the smaller Vector will be filled with ones.
   * @param vec the Vector to be multiplied
   * @return the result of the Multiplication
   */
  def *(vec:Vector): Vector = new Vector(vals.zipAll(vec.getValues, 1.0,1.0).map(x => x._1 * x._2):_*)
  /**
    * Multiplies the Vectors components by the given Scalar
    * @param s the Scalar with which this Vector will be sized
    * @return a new Vector, multiplied by s
    */
  def *(s:Double): Vector = new Vector(vals.map(f => f*s):_*)
  /**
    * Returns the length of this Vector by the formula of pythagoras
    * @return Length of this Vector
    */
  def length:Double = math.sqrt(vals.foldLeft(0.0)((p, v) => p + math.pow(v,2))).toDouble
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
    val s = vals.foldLeft(new StringBuilder("["))((a,b)=>a.append(new Frac(b)+ ", "))
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
  implicit def getValues: Seq[Double] = vals
}
class Vec2(val x:Double,val y:Double) extends Vector(x,y){}
class Vec3(val x:Double,val y:Double,val z:Double) extends Vector(x,y,z){
  def cross:Vec3=>Vec3=a=>new Vec3(a.z*y-z*a.y, z*a.x-x*a.z, x*a.y-y*a.x)
}
class Vec4(val x:Double,val y:Double,val z:Double,val w:Double) extends Vector(x,y,z,w){}
//CASTING
object Vec2{
  implicit def toTuple(vec:Vec2):(Double, Double) = (vec.x,vec.y)
  implicit def fromTuple(vec:(Double, Double)): Vec2 = new Vec2(vec._1,vec._2)
  implicit def toVec2(vec:Vector):Vec2 = new Vec2(vec.getValues.head, vec.getValues(1))
}
object Vec3{
  implicit def toTuple(vec:Vec3): (Double, Double, Double) = (vec.x,vec.y,vec.z)
  implicit def fromTuple(vec:(Double, Double, Double)): Vec3 = new Vec3(vec._1,vec._2,vec._3)
  implicit def toVec3(vec:Vector):Vec3 = new Vec3(vec.getValues.head, vec.getValues(1), vec.getValues(2))
}
object Vec4{
  implicit def toTuple(vec:Vec4):(Double, Double, Double, Double) = (vec.x,vec.y,vec.z,vec.w)
  implicit def fromTuple(vec:(Double, Double, Double, Double)): Vec4 = new Vec4(vec._1,vec._2,vec._3,vec._4)
  implicit def toVec4(vec:Vector):Vec4 = new Vec4(vec.getValues.head, vec.getValues(1), vec.getValues(2), vec.getValues(3))
}



