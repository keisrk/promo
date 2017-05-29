package sketch.svgFx

import scalafx.Includes._
import scalafx.beans.property.PropertyIncludes._
import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}
import scalafx.scene.shape.{Shape, Rectangle}
import scalafx.scene.SubScene
import scalafx.scene.text.{Text}
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Pane

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
      translateX = 197
      translateY = 147
    }
  )
  def setColor(img: SubScene, q: String, qs: List[String]): Unit = {
    for (e <- img.content) {
      if (e.id.value == q){
        e.asInstanceOf[javafx.scene.shape.Rectangle].fill = Cyan
      } else {
        e.asInstanceOf[javafx.scene.shape.Rectangle].fill = LightGreen
      }
    }
  }
}

