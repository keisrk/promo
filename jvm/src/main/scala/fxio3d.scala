package promotion.fxio3d

import promotion.matrix.{DblMat}
import promotion.wireframemodel.{P3D}

import scalafx.scene.canvas.{GraphicsContext => Ctx2D}

class FxIO3D extends P3D {
  def draw(ctx: Ctx2D, es: List[Edge], v: View): Unit = {
    ctx.beginPath()
    for (e <- es) {
      e match {
        case Nil => {}
	case p::tl => {
          ((tp: (Double, Double))=> ctx.moveTo(tp._1, tp._2))(dump(p, v))
	  for (q <- tl) {
          ((tp: (Double, Double))=> ctx.lineTo(tp._1, tp._2))(dump(q, v))
	  }
	}
      }
    }
    ctx.strokePath()
  }
}
