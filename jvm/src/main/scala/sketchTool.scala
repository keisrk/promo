package stetch.sketchtool

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.animation.AnimationTimer
import scalafx.scene.image.{Image}
import scalafx.scene.layout.{BorderPane}
import scalafx.scene.{Scene}
import scalafx.scene.canvas.{Canvas}
import scalafx.scene.layout.{VBox, HBox}

import sketch.fxio3d.{FxIO3D}
import sketch.animation.Clock
/*
class Interval extends AnimationTimer {
  override def handle(now: Long): Unit = {
    println("animation timer")
  }
}
*/
class WindowMaker {
}
object Test {
val j_sh =
"""
[
    {"id": "v0", "shape": "vec", "data": [[0, 200, 0], [110, 180, 90]]},
    {"id": "v1", "shape": "vec", "data": [[320, 200, 0], [110, 180, 90]]},
    {"id": "bl", "shape": "ply", "d": 120, "seq": [[-100, 380, 0], [200, 380, 0], [400, 440, 0], [400, 460, 0]]},
    {"id": "br", "shape": "ply", "d": 120, "seq": [[-100, 200, 0], [200, 200, 0], [400, 140, 0], [400, 120, 0]]},
    {"shape": "ply", "seq": [[0, 0, 0], [100, 0, 0], [0, 0, 0], [0, 100, 0], [0, 0, 0], [0, 0, 100]]},
    {"id": "ca", "shape": "vec", "data": [[506, 150, 6], [12, 55, 12]]},
    {"id": "cb", "shape": "vec", "data": [[666, 150, 6], [12, 55, 12]]},
    {"id": "cc", "shape": "vec", "data": [[506, 380, 6], [12, 55, 12]]},
    {"id": "cd", "shape": "vec", "data": [[666, 380, 6], [12, 55, 12]]},

    {"shape": "cmp", "data": [{"shape": "vec", "data": [[500,150,0], [24, 50, 24]]},
                              {"shape": "vec", "data": [[660,150,0], [24, 50, 24]]},
                              {"shape": "vec", "data": [[500,380,0], [24, 50, 24]]},
                              {"shape": "vec", "data": [[660,380,0], [24, 50, 24]]}]}
]
"""
val j_tr =
"""
[
    {"id": "A-v0", "tr": [[666, 0, 0], [506, 0, 0]]},
    {"id": "B-v0", "tr": [[506, 0, 0], [506, 0, 0]]},
    {"id": "C-v0", "tr": [[506, 0, 0], [346, 0, 0]], "rz": [0, 15]},
    {"id": "D-v0", "tr": [[346, 0, 0], [ 86, 0, 0]], "rz": [15, 0]},

    {"id": "A-v1", "tr": [[666, 0, 0], [506, 0, 0]]},
    {"id": "B-v1", "tr": [[506, 0, 0], [346, 0, 0]]},
    {"id": "C-v1", "tr": [[346, 0, 0], [346, 0, 0]]},
    {"id": "D-v1", "tr": [[346, 0, 0], [346, 0, 0]]},

    {"id": "A-ca", "tr": [[0, 50, 0], [0, 50, 0]]},
    {"id": "B-ca", "tr": [[0, 50, 0], [0,  0, 0]]},
    {"id": "C-ca", "tr": [[0,  0, 0], [0,  0, 0]]},
    {"id": "D-ca", "tr": [[0,  0, 0], [0, 50, 0]]},

    {"id": "A-cb", "tr": [[0,  0, 0], [0,  0, 0]]},
    {"id": "B-cb", "tr": [[0,  0, 0], [0, 50, 0]]},
    {"id": "C-cb", "tr": [[0, 50, 0], [0, 50, 0]]},
    {"id": "D-cb", "tr": [[0, 50, 0], [0,  0, 0]]},

    {"id": "A-cc", "tr": [[0, -50, 0], [0, -50, 0]]},
    {"id": "B-cc", "tr": [[0, -50, 0], [0,   0, 0]]},
    {"id": "C-cc", "tr": [[0,   0, 0], [0,   0, 0]]},
    {"id": "D-cc", "tr": [[0,   0, 0], [0, -50, 0]]},

    {"id": "A-cd", "tr": [[0,   0, 0], [0,   0, 0]]},
    {"id": "B-cd", "tr": [[0,   0, 0], [0, -50, 0]]},
    {"id": "C-cd", "tr": [[0, -50, 0], [0, -50, 0]]},
    {"id": "D-cd", "tr": [[0, -50, 0], [0,   0, 0]]}
]
"""
}

object Main extends JFXApp {
  def draw(cnv: Canvas,  shape: String, trans: String): Unit = {
    val q = List("A", "B", "C", "D")
    val ctx = cnv.graphicsContext2D; ctx.scale(1, -1); ctx.translate(cnv.width.toDouble / 2, -cnv.height.toDouble)
    val p = new FxIO3D(ctx)
    val cl = new Clock(100, q)
    val j_sh = p.load(shape)
    val j_tr = p.load_trans(trans)
    val es = p.mashup(j_sh, j_tr, cl.state(), 1d)
    p.draw(es, (0d, 0d, 0d))   
    val timer = AnimationTimer(t => {
      ctx.clearRect(- cnv.width.toDouble / 2, 0, cnv.width.toDouble, cnv.height.toDouble)
      cl.incl(1d)
      val es = p.mashup(j_sh, j_tr, cl.state(), cl.inter())
      p.draw(es, (0d, 0d, 0d))
    })
    timer.start()
  }
  val icon = new Image(getClass.getResourceAsStream("/images/logo.png"))

  val raw0 = new HBox {
    val cnv = new Canvas(300, 200)
    spacing = 5
    children = List(cnv)
  }
  val raw1 = new HBox {
    val cnv = new Canvas(300, 200)
    spacing = 5
    children = List(cnv)
  }
  val raw2 = new HBox {
    val cnv = new Canvas(300, 200)
    spacing = 5
    children = List(cnv)
  }
  val col = new VBox {
    spacing = 5
    children = List(raw0, raw1, raw2)
  }
  stage = new JFXApp.PrimaryStage {
    title.value = "Sketch Tool"
    icons += icon
  }
  val bord = new BorderPane {
    val cnv = new Canvas(200, 700)
    draw(cnv, Test.j_sh, Test.j_tr)
    left = col
    center = cnv
  }
  stage.scene = new Scene{
    root = bord
  }
}
