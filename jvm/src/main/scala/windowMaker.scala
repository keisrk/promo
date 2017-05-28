package sketch.windowMaker

import java.text.NumberFormat
import java.util.Locale

import scalafx.Includes._
import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}
import scalafx.geometry.Insets
import scalafx.scene.{Node}
import scalafx.scene.control.{Slider, SpinnerValueFactory, Spinner, TextField, TextFormatter}
import scalafx.scene.layout.{HBox, VBox, Region, Priority}
import scalafx.scene.text.Text
import scalafx.util.converter.{FormatStringConverter}
import scala.xml
import scala.xml.{Elem}

import sketch.component.{Position, Direction}

object WindowMaker {
  /* Light-weight DOM accessed via controles. */
  val max_value = 700
  val formatter = NumberFormat.getInstance(Locale.US)
  val converter = new FormatStringConverter[Number](formatter)
  def makeTFSlider(capt: String, v: DoubleP, maxv: Double): Node = {
    val cp = new Text(capt)
    val cv = new TextField {textFormatter = new TextFormatter(converter){value <==> v}}
    val rg0 = new Region{hgrow = Priority.Always}
    val rg1 = new Region{hgrow = Priority.Always}
    val sl = new Slider(0, maxv, 0){maxWidth = maxv; value <==> v}
    new HBox{children = List(cp, rg0, sl, rg1, cv); spacing = 20}
  }
  def makeCnvControl(id: String, dir: Direction, pos: Position, cnv: Node): Node = {
    val cp = new Text(id)
    val ps = for ((cp, v, maxv) <- List(("x", pos.x, max_value), ("y", pos.y, max_value), ("z", pos.z, max_value))) yield {makeTFSlider(cp, v, maxv)}
    val dr = for ((cp, v, maxv) <- List(("xy", dir.xy, max_value), ("yz", dir.yz, max_value), ("zx", dir.zx, max_value))) yield {makeTFSlider(cp, v, maxv)}
    val pn = new VBox{children = ps; spacing = 20}
    val dn = new VBox{children = dr; spacing = 20}
    new HBox{children = List(cp, pn, dn, cnv); spacing = 10}
  }
}
