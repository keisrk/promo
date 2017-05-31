package sketch

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{Event}
// import org.scalajs.dom.extensions._

import dom.html

import sketch.animation.Clock
import sketch.jsio3d.{JSIO3D}
import sketch.svg.{SVG}

object SketchApp extends JSApp {
  @JSExport
  def draw(cnv: html.Canvas, shp: html.TextArea, trans: html.TextArea): Unit = {
    val l = List("A", "B", "C", "D")
    val q = (x: Int) => l(x % l.length)
    val ctx = cnv.getContext("2d").asInstanceOf[Ctx2D]; ctx.scale(1, -1); ctx.translate(cnv.width / 2, -cnv.height)
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

    //SVG things
/*    val img = document.getElementById("svg01").asInstanceOf[raw.SVGSVGElement]
    val svg = new SVG
    for (s <- List(0,1,2,3)) {
      svg.makeLabel(img, s, svg.data(s))
    }*/

    dom.window.setInterval(() => {
      ctx.clearRect(- cnv.width / 2, 0, cnv.width, cnv.height)
      cl.incl(1d)
      val es = p.mashup(j_sh, j_tr, cl.state(), cl.inter())
      p.draw(es, (0d, 0d, 0d))
    }, 20)
  }
  def main(): Unit = {}
}
