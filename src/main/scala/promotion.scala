package promotion

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import scala.scalajs.js.Dictionary
// import org.scalajs.dom.extensions._

import dom.document
import dom.html
import promotion.wireframemodelOld.{Window, WireFrameModel => WFM, Point3D, Edge3D}
import promotion.wireframemodel.{P3D}
import promotion.data.{Data, Astellas}
import promotion.animation.Clock
object PromotionApp extends JSApp {
  @JSExport
  def draw(cnv: html.Canvas, inp: html.Input): Unit = {
    val ctx = cnv.getContext("2d")
      .asInstanceOf[Ctx2D]
    ctx.scale(1, 1)
    ctx.translate(0, -cnv.height)

    val cl = new Clock(240)
    val global_cl = new Clock(1200)
    dom.window.setInterval(() => {
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      cl.incl(1d)
      global_cl.incl(1d)
      WFM.draw(ctx, cl.tl(List(Data.tl01_m360, Data.tl02_m360)), new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
      WFM.draw(ctx, global_cl.tl(List(Data.tl03_m1200)), new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
    }, 20)
  }
  @JSExport
  def drawAst(cnv: html.Canvas, inp: html.Input): Unit = {
    val ctx = cnv.getContext("2d")
      .asInstanceOf[Ctx2D]
    ctx.scale(1, -1)
    ctx.translate(0, -cnv.height)

    val cl = new Clock(480)
    val global_cl = new Clock(1200)
    dom.window.setInterval(() => {
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      cl.incl(1d)
      global_cl.incl(1d)
      WFM.draw(ctx, cl.tl(List(Astellas.t_m)), new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
//      WFM.draw(ctx, global_cl.tl(List(Data.tl03_m1200)), new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
    }, 20)
  }
  @JSExport
  def drawSanten(cnv: html.Canvas, shp: html.TextArea, trs: html.TextArea): Unit = {
    val q = List("A", "B", "C", "D", "E")
    val p = new P3D
    val ctx = cnv.getContext("2d").asInstanceOf[Ctx2D]; ctx.scale(0.5, -0.5); ctx.translate(0, -cnv.height)
    val cl = new Clock(100)
    val j_sh = p.load(shp.value)
    val j_tr = p.load_trs(trs.value)
    val es = p.mashup(j_sh, j_tr, cl.state(q), 1d)
    p.draw(ctx, es, (0d, 0d, 0d))
    dom.window.setInterval(() => {
      ctx.clearRect(0, 0, cnv.width * 2, cnv.height)
      cl.incl(1d)
      val j_sh = p.load(shp.value)
      val j_tr = p.load_trs(trs.value)
      val es = p.mashup(j_sh, j_tr, cl.state(q), cl.inter())
      p.draw(ctx, es, (0d, 0d, 0d))
    }, 20)
  }
  def main(): Unit = {}
}
