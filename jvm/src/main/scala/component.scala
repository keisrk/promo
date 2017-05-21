package promotion.component
import scala.xml.{Elem}
//import scalafx.Includes._

import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}

import promotion.wireframemodel.{P3D}

abstract class Component 
case class Conveyor(name: StringP, x: DoubleP, y: DoubleP, width: DoubleP,
  length0: DoubleP, length1: DoubleP,
  theta: DoubleP, pitch: DoubleP) extends Component
case class Sensor(name: StringP, x: DoubleP, y: DoubleP, time: DoubleP, state: BooleanP) extends Component {val nick = new StringP(this, "NickName", name.value)}
case class Arm1(name: StringP, x: DoubleP, y: DoubleP,
  length: DoubleP, theta: DoubleP) extends Component
case class Arm2(name: StringP, x: DoubleP, y: DoubleP,
  length0: DoubleP, theta0: DoubleP,
  length1: DoubleP, theta1: DoubleP) extends Component
case class Arm3(name: StringP, x: DoubleP, y: DoubleP,
  length0: DoubleP, theta0: DoubleP,
  length1: DoubleP, theta1: DoubleP,
  length2: DoubleP, theta2: DoubleP) extends Component
case class Work(name: StringP, x: DoubleP, y: DoubleP, width: DoubleP, height: DoubleP) extends Component
case class BackGround(draw: List[DoubleP]) extends Component

object  ComponentOp {

    def s_loadComponent(s: scala.xml.Node): Component =
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
    }

  def loadComponent(s: Elem): List[Component] =
    (s \ "component").head.child.map((elem: scala.xml.Node) => s_loadComponent(elem)).toList

}
