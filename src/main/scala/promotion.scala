package promotion

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
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
  def drawSanten(cnv: html.Canvas, inp: html.TextArea): Unit = {
    val p = new P3D
    val ctx = cnv.getContext("2d").asInstanceOf[Ctx2D]
    p.draw(ctx, p.load(inp.value).foldLeft(List[p.Edge]()){(acc, s) => acc ++ p.toEdge(s, (3d, 3d, 3d, 3d))}, (3d, 3d, 3d, 3d))
    dom.window.setInterval(() => {
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      p.draw(ctx, p.load(inp.value).foldLeft(List[p.Edge]()){(acc, s) => acc ++ p.toEdge(s, (3d, 3d, 3d, 3d))}, (3d, 3d, 3d, 3d))
    }, 20)
    //p.draw(ctx, p.jv.foldLeft(List[p.Edge]()){(acc, s) => acc ++ p.toEdge(s, (3d, 3d, 3d, 3d))}, (3d, 3d, 3d, 3d))
    //p.draw(ctx, p.e ++ p.toEdge(p.v, (3d, 3d, 3d, 3d)), (3d, 3d, 3d, 3d));
    //println(p.jv)
  }
  def main(): Unit = {}
}
