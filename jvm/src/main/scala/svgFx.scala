package sketch.svgFx

import scalafx.Includes._
import scalafx.beans.property.PropertyIncludes._
import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}
import scalafx.scene.shape.{Shape, Rectangle, MoveTo, LineTo, ArcTo, Path, PathElement}
import scalafx.scene.SubScene
import scalafx.scene.text.{Text, TextAlignment, TextFlow, Font}
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Pane

import sketch.flowChart.{FlowChart}

class SVGFx(img: SubScene){ 
  val test = List(
    new Rectangle {
      id = "redrect"
      width = 100
      height = 100
      fill = Red
      translateX = 97
      translateY = 147
    },
    new Rectangle {
      id = "bluerect"
      width = 100
      height = 100
      fill = Blue
      translateX = 217
      translateY = 147
    }
  )
  def makeRect(s: String, xpos: Double, ypos: Double, w: Double, h: Double): Rectangle = {
    new Rectangle {
      id = s
      width = w
      height = h
      fill = White
      stroke = Black 
      x = xpos
      y = ypos
    }
  }
  def makeBox(rc: Rectangle, i: Int): Unit = {
    val id_s = rc.id.value
    val xpos = rc.x.value 
    val ypos = rc.y.value 
    val w = rc.width.value 
    val h = rc.height.value
    i match {
      case 0 => {}
      case 1 => {
        rc.x.update(xpos -10)
        rc.width.update(w +20)
        rc.arcWidth.value = h/2; rc.arcHeight.value = h/2}
      case 2 => {
       val es = List(
          new MoveTo{x = xpos; y = ypos},
          new LineTo{absolute = false; x = 0; y = h},
          new MoveTo{x = xpos +w; y = ypos},
          new LineTo{absolute = false; x = 0; y = h})
       val p = new Path{
          stroke = Black 
          elements = es
        }
        img.content += p
        rc.x.update(xpos -10)
        rc.width.update(w +20)
      }
      case 3 => {
        val es = List(
          new MoveTo{x = xpos - w/2; y = ypos + h/2},
          new LineTo{absolute = false; x = w; y = -h},
          new LineTo{absolute = false; x = w; y = h},
          new LineTo{absolute = false; x = -w; y = h},
          new LineTo{absolute = false; x = -w; y = -h}
        )
        val p = new Path{
          id = id_s
          stroke = Black 
          fill = rc.fill.value
          elements = es
        }
        img.content.update(img.content.indexWhere(_.id.value == id_s), p)
      }
      case 4 => {
         val es = List(
          new MoveTo{x = xpos; y = ypos},
          new LineTo{absolute = false; x = -10; y = h/2},
          new LineTo{absolute = false; x = 10; y = h/2},
          new LineTo{absolute = false; x = w -10; y = 0},
          new ArcTo{ absolute = false; x = 0; y = -h 
          radiusX = h/2; radiusY = h/2; XAxisRotation = 180},
          new LineTo{absolute = false; x = 10 -w; y = 0}
        )
        val p = new Path{
          id = id_s
          stroke = Black 
          fill = rc.fill.value
          elements = es
        }
        img.content.update(img.content.indexWhere(_.id.value == id_s), p)
      }
      case 5 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos},
          new LineTo{absolute = false; x = 0; y = h},
          new LineTo{absolute = false; x = w; y = 0},
          new LineTo{absolute = false; x = 0; y = -h},
          new LineTo{absolute = false; x = -20; y = -10},
          new LineTo{absolute = false; x = -w +40; y = 0},
          new LineTo{absolute = false; x = -20; y = 10}
        )
        val p = new Path{
          id = id_s
          stroke = Black 
          fill = rc.fill.value
          elements = es
        }
        img.content.update(img.content.indexWhere(_.id.value == id_s), p)
      }
    }                                                                                                                                                          
  }
  def makeLabel(id: String, s: String, xpos: Double, ypos: Double, i: Int): Unit = {
    val tx = new Text(xpos, ypos, s) {
      font = Font("Courier New", 20)
    }
    val minX = tx.boundsInLocal.value.minX 
    val minY = tx.boundsInLocal.value.minY 
    val w = tx.boundsInLocal.value.width 
    val h = tx.boundsInLocal.value.height 
    tx.x = minX - w / 2
    tx.y = minY + h
    val rc = makeRect(id, minX - w / 2 -5, ypos - h, w + 10, 2 * h)
    img.content += rc
    img.content += tx
    makeBox(rc, i)
  }
  def setColor(q: String): Unit = {
    for (e <- img.content) e match {
      case sh:javafx.scene.shape.Rectangle if e.id.value == q => sh.fill = Turquoise
      case sh:javafx.scene.shape.Path if e.id.value == q => sh.fill = Turquoise
      case sh:javafx.scene.shape.Rectangle if sh.fill == Transparent => {}
      case sh:javafx.scene.shape.Rectangle => sh.fill = White 
      case sh:javafx.scene.shape.Path => sh.fill = White 
      case _ => {}
    }
  }
  def lookup(q: String): Shape = {
    img.content.find( (e :javafx.scene.Node) => e match {
      case sh:javafx.scene.shape.Shape => sh.id.value == q
      case _ => false
    }) match {
      case Some(shape) => shape.asInstanceOf[javafx.scene.shape.Shape]
      case None => sys.error("Lookup failed on " + q)
    }
  }//sliding
  def connect(fc: FlowChart): Path = new Path{
    id = "connecting_edge"
    stroke = Black
    for ((q, p, i) <- fc.data) {
      val qsh = lookup(q); val psh = lookup(p)
      val qminX = qsh.boundsInLocal.value.minX
      val qminY = qsh.boundsInLocal.value.minY
      val qw = qsh.boundsInLocal.value.width
      val qh = qsh.boundsInLocal.value.height

      val pminX = psh.boundsInLocal.value.minX
      val pminY = psh.boundsInLocal.value.minY
      val pw = psh.boundsInLocal.value.width
      val ph = psh.boundsInLocal.value.height

      val span = 40
      elements ++ List(
        new MoveTo{x = qminX + qw/2; y = qminY},
        new LineTo{absolute = false; x = 0; y = qh + 2 * span})
    }
  }
}

