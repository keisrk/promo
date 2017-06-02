package sketch.svgFx
import scala.collection.mutable.ListBuffer
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

  def draw(v: List[(String, String, Int, Double, Double)], e: List[(String, String)]): Unit = {
    val boxes = ListBuffer[Shape]()
    val edges = ListBuffer[PathElement]()
    val qs = for ((id, l, i, xpos, ypos) <- v) yield {
      val q = label(id, l, xpos, ypos)
      val qminX = q.boundsInLocal.value.minX
      val qmaxY = q.boundsInLocal.value.maxY
      val w = q.boundsInLocal.value.width
      val h = q.boundsInLocal.value.height
      boxes += box(id, xpos -w/2, qmaxY -h, w, h, i); q
    }
    for (q <- qs) {
      for ((l, r) <- e) {
        if (q.id.value == l) {
          qs.find(p => p.id.value == r) match {
            case None => {}
            case Some(p) => edges ++= connect(q, p, false)
          }
        } else if (l.startsWith(q.id.value) && l.endsWith("_else")) {
          qs.find(p => p.id.value == r) match {
            case None => {}
            case Some(p) => edges ++= connect(q, p, true)
          }
        } else {

        }
      }
    }

    val path = new Path{
          id = "connecting_edge"
          stroke = Black 
          elements = edges
    }
    img.content += path
    for (b <- boxes) {img.content += b}
    for (q <- qs) {img.content += q}
  }//sliding
  def arrange(lbls: List[List[(String, String, Int)]], spanX: Double, spanY: Double): List[(String, String, Int, Double, Double)] = {
    lbls.zipWithIndex.foldLeft(List[(String, String, Int, Double, Double)]()) {
      case (acc, (ls, i)) => acc ++ ls.zipWithIndex.collect {
        case ((id, l, box), j) if id != "NA" => (id, l, box, (i+1) * spanX, (j+1) * spanY)
      }
    }
  }

  def rectangle(s: String, xpos: Double, ypos: Double, w: Double, h: Double): Rectangle = {
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
  def setColor(q: String): Unit = {
    for (e <- img.content) e match {
      case sh:javafx.scene.shape.Rectangle if e.id.value == q => sh.fill = Turquoise
      case sh:javafx.scene.shape.Path if e.id.value == q => sh.fill = Turquoise
      case sh:javafx.scene.shape.Path if e.id.value == "connecting_edge" => {}
      case sh:javafx.scene.shape.Rectangle => sh.fill = White 
      case sh:javafx.scene.shape.Path => sh.fill = White 
      case _ => {}
    }
  }
  def label(id_s: String, s: String, xpos: Double, ypos: Double): Text = {
    val txt = new Text(xpos, ypos, s) {
      id = id_s
      font = Font("Courier New", 20)
    }
    txt.x.update(xpos -txt.boundsInLocal.value.width / 2)
    txt
  }
  def connect(qt: Text, pt: Text, offset: Boolean): List[PathElement] = {
    val spanX = 120
    val spanY = 40
    val qw = qt.boundsInLocal.value.width
    val qh = qt.boundsInLocal.value.height
    val qmaxY = qt.boundsInLocal.value.maxY +qh/2
    val qcx = if (offset) {
      qt.boundsInLocal.value.maxX + qw/2
    } else {
      qt.boundsInLocal.value.minX + qw/2
    }
    val qcy = if (offset) {
      qmaxY -qh
    } else {
      qmaxY
    }

    val pw = pt.boundsInLocal.value.width
    val ph = pt.boundsInLocal.value.height
    val pminX = pt.boundsInLocal.value.minX
    val pminY = pt.boundsInLocal.value.minY -ph/2
    val pcx = pminX + pw/2

    if (qcx == pcx) {
      if (qmaxY < pminY) {
        List(
          new MoveTo{x = qcx; y = qcy},
          new LineTo{absolute = false; x = 0; y = pminY -qcy/*qmaxY*/})
      } else {
        List(
          new MoveTo{x = qcx; y = qcy},
          new LineTo{absolute = false; x = 0; y = spanY},
          new LineTo{absolute = false; x = -spanX; y = 0},
          new LineTo{absolute = false; x = 0; y = pminY -qcy /*qmaxY*/ -spanY * 2},
          new LineTo{absolute = false; x = spanX; y = 0},
          new LineTo{absolute = false; x = 0; y = spanY},
          new LineTo{absolute = false; x = -5; y = -10},
          new LineTo{absolute = false; x = 5; y = 0})
      }
    } else {
      val diff = qcx - pcx
      if (qmaxY < pminY) {
        List(
          new MoveTo{x = qcx; y = qcy},
          new LineTo{absolute = false; x = 0; y = pminY -qcy -spanY/*qmaxY*/},
          new LineTo{absolute = false; x = -diff; y = 0},
          new LineTo{absolute = false; x = 0; y = spanY})
      } else {
        val cspanY = if (offset) 0 else spanY
        List(
          new MoveTo{x = qcx; y = qcy},
          new LineTo{absolute = false; x = 0; y = cspanY},
          new LineTo{absolute = false; x = -diff/2; y = 0},
          new LineTo{absolute = false; x = 0; y = pminY -qcy /*qmaxY*/ -(cspanY + spanY)},
          new LineTo{absolute = false; x = -diff/2; y = 0},
          new LineTo{absolute = false; x = 0; y = spanY},
          new LineTo{absolute = false; x = -5; y = -10},
          new LineTo{absolute = false; x = 5; y = 0})
      }
    }
  }
  def box(id_s: String, xpos: Double, ypos: Double, w: Double, h: Double, i: Int): Shape = {
    i match {
      case 0 => rectangle(id_s, xpos, ypos -h/2, w, 2*h)
      case 1 => {
        val rc = rectangle(id_s, xpos -10, ypos -h/2, w +20, 2*h)
        rc.arcWidth.update(h/2); rc.arcHeight.update(h/2); rc}
      case 2 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new MoveTo{x = xpos +w; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new MoveTo{x = xpos -10; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new LineTo{absolute = false; x = w +20; y = 0},
          new LineTo{absolute = false; x = 0; y = -2*h},
          new LineTo{absolute = false; x = -(w +20); y = 0}
        )
        new Path{
          id = id_s
          stroke = Black
          fill = White
          elements = es
        }
      }
      case 3 => {
        val es = List(
          new MoveTo{x = xpos - w/2; y = ypos +h/2},
          new LineTo{absolute = false; x = w; y = -h},
          new LineTo{absolute = false; x = w; y = h},
          new LineTo{absolute = false; x = -w; y = h},
          new LineTo{absolute = false; x = -w; y = -h}
        )
        new Path{
          id = id_s
          stroke = Black
          fill = White
          elements = es
        }
      }
      case 4 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos -h/2},
          new LineTo{absolute = false; x = -10; y = h},
          new LineTo{absolute = false; x = 10; y = h},
          new LineTo{absolute = false; x = w -10; y = 0},
          new ArcTo{ absolute = false; x = 0; y = -2*h
            radiusX = h; radiusY = h; XAxisRotation = 180},
          new LineTo{absolute = false; x = 10 -w; y = 0}
        )
        new Path{
          id = id_s
          stroke = Black 
          fill = White
          elements = es
        }
      }
      case 5 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new LineTo{absolute = false; x = w; y = 0},
          new LineTo{absolute = false; x = 0; y = -2*h},
          new LineTo{absolute = false; x = -20; y = -10},
          new LineTo{absolute = false; x = -w +40; y = 0},
          new LineTo{absolute = false; x = -20; y = 10}
        )
        new Path{
          id = id_s
          stroke = Black 
          fill = White
          elements = es
        }
      }
    }
  }
}
