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
object PromotionApp extends JSApp {
/*
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
 */
  def timeline(t: Double): List[Edge3D] = {
    val i = t % 360
    if (0<= i && i < 60) {
      WFM.rotateEdge3D(Data.test05a, 0d, 30d) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05b, 0d, 30d), new Point3D(0d, 0d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05c, 0d, 30d - i), new Point3D(0d, 0d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 0d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d + 2.5 * i))

    } else if(60 <= i && i < 120) {
      WFM.rotateEdge3D(Data.test05a, 0d, 30d) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05b, 0d, 90d - i), new Point3D(0d, 0d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05c, 0d, i - 90d), new Point3D(0d, 0d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 0d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 150d + 2.5 * (i - 60d))) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d + 2.5 * i))

    } else if(120 <= i && i < 180) {
      WFM.rotateEdge3D(Data.test05a, 0d, 150d - i) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05b, 0d, i - 150d), new Point3D(0d, 0d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05c, 0d, 30d), new Point3D(0d, 0d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 0d + 2.5 * (i - 120d))) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d + 2.5 * i))

    } else if(180<= i && i < 240) {
      WFM.rotateEdge3D(Data.test05a, 0d, i - 210d) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05b, 0d, 30d), new Point3D(0d, 0d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05c, 0d, 30d), new Point3D(0d, 0d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d + 2.5 * i))

    } else if(240<= i && i < 300){
      WFM.rotateEdge3D(Data.test05a, 0d, 30d) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05b, 0d, 30d), new Point3D(0d, 0d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05c, 0d, 30d), new Point3D(0d, 0d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d + 2.5 * i))

    } else {
      WFM.rotateEdge3D(Data.test05a, 0d, 30d) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05b, 0d, 30d), new Point3D(0d, 0d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.test05c, 0d, 30d), new Point3D(0d, 0d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 150d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d)) ++
      WFM.translateEdge3D(WFM.rotateEdge3D(Data.data06, 0d, 180d), new Point3D(-10d, -40d, 300d + 2.5 * i))

    }
  }

  @JSExport
  def draw(cnv: html.Canvas, inp: html.Input, inpR: html.Input): Unit = {
    val ctx = cnv.getContext("2d")
      .asInstanceOf[Ctx2D]
    ctx.scale(1, -1)
    ctx.translate(0, -cnv.height)
    var model = timeline(inpR.value.toDouble)
    WFM.draw(ctx, model, new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
    inp.oninput = (e: dom.Event) => {
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      WFM.draw(ctx, model, new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
    }
    inpR.oninput = (e: dom.Event) => {
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      model = timeline(inpR.value.toDouble)
      WFM.draw(ctx, model, new Window(inp.value.toDouble, cnv.width/2d, cnv.height/4d))
    }
  }
/*
  @JSExport
  def addClickedMessage(): Unit = {
    appendPar(document.body, "Button Clicked.")
  }
 */
  def main(): Unit = {
    println("Hello world!");
  }
}
