package sketch

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{Event}
//import org.scalajs.dom.extensions._

import dom.html

import sketch.animation.Clock
import sketch.jsio3d.{JSIO3D}
import sketch.svg.{SVG}
import sketch.flowChart.{FlowChart}

object SketchApp extends JSApp {
  val fc = List(("A", "B", 0), ("B", "C", 0), ("C", "D", 0), ("D", "E", 0), ("E", "F", 0), ("F", "G", 0), ("G", "H", 0), ("H", "I", 0), ("I", "D", 0))
  val qs = new FlowChart("A", fc) 

val shusekiV = List(
  List(("0", "起動", 0), ("A", "集積<5", 3), ("B", "製品到着", 0), ("C", "昇降部下降", 0)),
  List(("NA", "", 0), ("DG", "集積マガジン移動", 0), ("EH", "プッシャ出", 0), ("FI", "プッシャ戻", 0))
  )
val shusekiE = List(("0", "A"), ("A", "B"), ("A_else", "DG"), ("B", "C"), ("C", "A"), ("DG", "EH"), ("EH", "FI"), ("FI", "A"))

  @JSExport
  def draw(cnv: html.Canvas, shp: html.TextArea, trans: html.TextArea, img: dom.svg.SVG): Unit = {
    val svg = new SVG(img)
    val v = svg.arrange(shusekiV, 300, 100)
    svg.draw(v, shusekiE)

    val l = List("A", "B", "C", "D", "E", "F")
    val q = qs.st8//(x: Int) => l(x % l.length)
    val ctx = cnv.getContext("2d").asInstanceOf[Ctx2D]; ctx.scale(1, -1); ctx.translate(0, -cnv.height)//(cnv.width / 2, -cnv.height)
    val p = new JSIO3D(ctx)
    val cl = new Clock(100, q)
    var j_sh = p.load(shp.value)
    var j_tr = p.load_trans(trans.value)
    val es = p.mashup(j_sh, j_tr, cl.state(), 1d)
    shp.onchange = (e: Event) => {
      j_sh = p.load(shp.value)
    }
    trans.onchange = (e: Event) => {
      j_tr = p.load_trans(trans.value)
    }
    p.draw(es, (0d, 0d, 0d))

    dom.window.setInterval(() => {
      ctx.clearRect(0, 0, cnv.width, cnv.height)//(- cnv.width / 2, 0, cnv.width, cnv.height)
      cl.incl(2d)
      val es = p.mashup(j_sh, j_tr, cl.state(), cl.inter())
      p.draw(es, (0d, 0d, 0d))
      svg.setColor(cl.state())
    }, 20)
  }
  def main(): Unit = {}
}
