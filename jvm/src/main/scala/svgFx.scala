package sketch.svgFx

import scalafx.Includes._
import scalafx.beans.property.PropertyIncludes._
import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}
import scalafx.scene.shape.{Shape, Rectangle}
import scalafx.scene.SubScene
import scalafx.scene.text.{Text, TextAlignment, TextFlow, Font}
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Pane
// dom.raw.SVGTextElement.getBBox(): SVGRect
// dom.raw.SVGRect.{x, y, width, height}
// scene.text.Text.boundsInLocal: ReadOnlyObjectProperty[Bounds]
// scalafx.geometry.Bounds.{minX, minY, width, height}
class SVGFx {
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
  def decoRect(rc: Rectangle, i: Int): Unit = {
    val id = rc.id.value
    val x = rc.x.value 
    val y = rc.y.value 
    val w = rc.width.value 
    val h = rc.height.value
    i match {
      case 0 => {}
      case 1 => {
        rc.x.update(x -10)
        rc.width.update(w +20)
        rc.arcWidth.value = h/2; rc.arcHeight.value = h/2}
      case 2 => {}
    }
  }
  def makeLabel(root: SubScene, id: String, s: String, xpos: Double, ypos: Double, i: Int): Unit = {
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
    root.content += rc
    root.content += tx
    decoRect(rc, i)
  }
  def setColor(img: SubScene, q: String, qs: List[String]): Unit = {
    for (e <- img.content) {
      if (e.id.value == q){
        e.asInstanceOf[javafx.scene.shape.Rectangle].fill = Turquoise
      } else {
        e.asInstanceOf[javafx.scene.shape.Rectangle].fill = White
      }
    }
  }
}

