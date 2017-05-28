package sketch.component
import scala.xml.{Elem}
import scalafx.Includes._

import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}

import scalafx.scene.canvas.{GraphicsContext => Ctx2D}
import sketch.wireframemodel.{P3D}
import sketch.fxio3d.{FxOut}

class Direction(_xy: DoubleP, _yz: DoubleP, _zx: DoubleP){val xy: DoubleP = _xy; val yz: DoubleP = _yz; val zx: DoubleP = _zx}
class Position(_x: DoubleP, _y: DoubleP, _z: DoubleP){val x: DoubleP = _x; val y: DoubleP = _y; val z: DoubleP = _z}
class Component(_id: String, _dir: Direction, _pos: Position){val id:String = _id; val dir: Direction = _dir; val pos: Position = _pos} 
class ComponentIO3D(ctx: Ctx2D) extends FxOut(ctx: Ctx2D){
  def s_loadComponent(s: scala.xml.Node): (Component, Shape, List[Trans]) = {
    val id = s.label
    val x = DoubleP((s\ "x").text.toDouble)
    val y = DoubleP((s\ "y").text.toDouble)
    val z = DoubleP((s\ "z").text.toDouble)
    val xy = DoubleP((s\ "xy").text.toDouble)
    val yz = DoubleP((s\ "yz").text.toDouble)
    val zx = DoubleP((s\ "zx").text.toDouble)
    (new Component(id, new Direction(xy, yz, zx), new Position(x, y, z)), Vec(None, (0, 0, 0), (100, 100, 100)), List())
  }
  def loadComponent(s: Elem): List[(Component, Shape, List[Trans])] =
    (s \ "component").head.child.map((elem: scala.xml.Node) => s_loadComponent(elem)).toList

/*    def s_loadComponent(s: scala.xml.Node): Component =
    s.label match {
      case "conveyor" => {
        val n = StringP((s\ "name").text)
        val s_x = DoubleP((s\ "x").text.toDouble)
        val s_y = DoubleP((s\ "y").text.toDouble)
        val w = DoubleP((s\ "width").text.toDouble)
        val ln0 = DoubleP((s\ "length0").text.toDouble)
        val ln1 = DoubleP((s\ "length1").text.toDouble)
        val th = DoubleP(0)
        val p = DoubleP((s\ "pitch").text.toDouble)
        Conveyor(n, s_x, s_y, w, ln0, ln1, th, p)
      }
      case "sensor" => {
        val n = StringP((s\ "name").text)
        val s_x = DoubleP((s\ "x").text.toDouble)
        val s_y = DoubleP((s\ "y").text.toDouble)
        val t = DoubleP(0)
        val st = BooleanP(false)
        Sensor(n, s_x, s_y, t, st)
      }
      case "arm1" => {
        val n = StringP((s\ "name").text)
        val s_x = DoubleP((s\ "x").text.toDouble)
        val s_y = DoubleP((s\ "y").text.toDouble)
        val ln = DoubleP((s\ "length0").text.toDouble)
        val th = DoubleP((s\ "theta0").text.toDouble)
        Arm1(n, s_x, s_y, ln, th)
      }
      case "arm2" => {
        val n = StringP((s\ "name").text)
        val s_x = DoubleP((s\ "x").text.toDouble)
        val s_y = DoubleP((s\ "y").text.toDouble)
        val ln0 = DoubleP((s\ "length0").text.toDouble)
        val th0 = DoubleP((s\ "theta0").text.toDouble)
        val ln1 = DoubleP((s\ "length1").text.toDouble)
        val th1 = DoubleP((s\ "theta1").text.toDouble)
        Arm2(n, s_x, s_y, ln0, th0, ln1, th1)
      }
      case "arm3" => {
        val n = StringP((s\ "name").text)
        val s_x = DoubleP((s\ "x").text.toDouble)
        val s_y = DoubleP((s\ "y").text.toDouble)
        val ln0 = DoubleP((s\ "length0").text.toDouble)
        val th0 = DoubleP((s\ "theta0").text.toDouble)
        val ln1 = DoubleP((s\ "length1").text.toDouble)
        val th1 = DoubleP((s\ "theta1").text.toDouble)
        val ln2 = DoubleP((s\ "length2").text.toDouble)
        val th2 = DoubleP((s\ "theta2").text.toDouble)
        Arm3(n, s_x, s_y, ln0, th0, ln1, th1, ln2, th2)
      }
      case "work" => {
        val n = StringP((s\ "name").text)
        val s_x = DoubleP((s\ "x").text.toDouble)
        val s_y = DoubleP((s\ "y").text.toDouble)
        val w = DoubleP((s\ "width").text.toDouble)
        val h = DoubleP((s\ "height").text.toDouble)
        Work(n, s_x, s_y, w, h)
      }
      case _ => BackGround(List())
    }*/
}
