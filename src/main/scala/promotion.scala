package promotion

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
// import org.scalajs.dom.extensions._

import dom.document
import dom.html
import promotion.wireframemodel.{Window, WireFrameModel => WFM, Point3D, Edge3D}
import promotion.data.Data
import promotion.animation.Clock
object PromotionApp extends JSApp {
  @JSExport
  def draw(cnv: html.Canvas, inp: html.Input): Unit = {
    val ctx = cnv.getContext("2d")
      .asInstanceOf[Ctx2D]
    ctx.scale(1, -1)
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
  def main(): Unit = {}
}
