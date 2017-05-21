package sketch

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import org.scalajs.dom.raw.{Event}
// import org.scalajs.dom.extensions._

import dom.html

import sketch.animation.Clock
import sketch.jsio3d.{JSIO3D}

object SketchApp extends JSApp {
  @JSExport
  def drawSanten(cnv: html.Canvas, shp: html.TextArea, trans: html.TextArea): Unit = {
    val q = List("A", "B", "C", "D")
    val p = new JSIO3D
    val ctx = cnv.getContext("2d").asInstanceOf[Ctx2D]; ctx.scale(1, -1); ctx.translate(cnv.width / 2, -cnv.height)
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
    p.draw(ctx, es, (0d, 0d, 0d))
    dom.window.setInterval(() => {
      ctx.clearRect(- cnv.width / 2, 0, cnv.width, cnv.height)
      cl.incl(1d)
      val es = p.mashup(j_sh, j_tr, cl.state(), cl.inter())
      p.draw(ctx, es, (0d, 0d, 0d))
    }, 20)
  }
  def main(): Unit = {}
}
