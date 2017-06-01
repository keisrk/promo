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
      val ps = e.collect{case (l, r) if q.id.value == l => qs.find(p => p.id.value == r)}
      for (pt <- ps) yield pt match {
        case None => List[PathElement]()
        case Some(p) => edges ++= connect(q, p)
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
  def connect(qt: Text, pt: Text): List[PathElement] = {
    val spanX = 120
    val spanY = 40
    val qminX = qt.boundsInLocal.value.minX
    val qmaxY = qt.boundsInLocal.value.maxY
    val qw = qt.boundsInLocal.value.width
    val qh = qt.boundsInLocal.value.height
    val qcx = qminX + qw/2

    val pminX = pt.boundsInLocal.value.minX
    val pminY = pt.boundsInLocal.value.minY
    val pw = pt.boundsInLocal.value.width
    val ph = pt.boundsInLocal.value.height
    val pcx = pminX + pw/2

    if (qcx == pcx) {
      if (qmaxY < pminY) {
        List(
          new MoveTo{x = qcx; y = qmaxY},
          new LineTo{absolute = false; x = 0; y = pminY - qmaxY})
      } else {
        List(
          new MoveTo{x = qcx; y = qmaxY},
          new LineTo{absolute = false; x = 0; y = spanY},
          new LineTo{absolute = false; x = -spanX; y = 0},
          new LineTo{absolute = false; x = 0; y = pminY - qmaxY -spanY * 2},
          new LineTo{absolute = false; x = spanX; y = 0},
          new LineTo{absolute = false; x = 0; y = spanY})
      }
    } else {
      val diff = qcx - pcx
      if (qmaxY < pminY) {
        List(
          new MoveTo{x = qcx; y = qmaxY},
          new LineTo{absolute = false; x = 0; y = pminY - qmaxY},
          new LineTo{absolute = false; x = -diff; y = 0},
          new LineTo{absolute = false; x = 0; y = 40})
      } else {
        List(
          new MoveTo{x = qcx; y = qmaxY},
          new LineTo{absolute = false; x = 0; y = spanY},
          new LineTo{absolute = false; x = -diff/2; y = 0},
          new LineTo{absolute = false; x = 0; y = pminY - qmaxY -spanY * 2},
          new LineTo{absolute = false; x = -diff/2; y = 0},
          new LineTo{absolute = false; x = 0; y = spanY})
      }
    }
  }
  def box(id_s: String, xpos: Double, ypos: Double, w: Double, h: Double, i: Int): Shape = {
    i match {
      case 0 => makeRect(id_s, xpos, ypos, w, h)
      case 1 => {
        val rc = makeRect(id_s, xpos -10, ypos, w +20, h)
        rc.arcWidth.update(h/2); rc.arcHeight.update(h/2); rc}
      case 2 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos},
          new LineTo{absolute = false; x = 0; y = h},
          new MoveTo{x = xpos +w; y = ypos},
          new LineTo{absolute = false; x = 0; y = h},
          new MoveTo{x = xpos -10; y = ypos},
          new LineTo{absolute = false; x = 0; y = h},
          new LineTo{absolute = false; x = w +20; y = 0},
          new LineTo{absolute = false; x = 0; y = -h},
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
          new MoveTo{x = xpos - w/2; y = ypos + h/2},
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
          new MoveTo{x = xpos; y = ypos},
          new LineTo{absolute = false; x = -10; y = h/2},
          new LineTo{absolute = false; x = 10; y = h/2},
          new LineTo{absolute = false; x = w -10; y = 0},
          new ArcTo{ absolute = false; x = 0; y = -h
            radiusX = h/2; radiusY = h/2; XAxisRotation = 180},
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
          new MoveTo{x = xpos; y = ypos},
          new LineTo{absolute = false; x = 0; y = h},
          new LineTo{absolute = false; x = w; y = 0},
          new LineTo{absolute = false; x = 0; y = -h},
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

