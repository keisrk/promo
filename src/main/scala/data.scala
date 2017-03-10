package promotion.data
import promotion.wireframemodelOld.{Point3D, Edge3D, WireFrameModel, Poly, Rect}
object Data {

  val data05 = WireFrameModel.makeRect(new Point3D(0d, 0d, 0d), 30, 6, 120) ++ 
    WireFrameModel.makeRect(new Point3D(0d, -10d, 0d), 30, 10, 6) ++
    WireFrameModel.makeRect(new Point3D(0d, -10d, 114d), 30, 10, 6)


  val test05a = WireFrameModel.translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val test05b = WireFrameModel.translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val test05c = WireFrameModel.translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val pl = new Poly(new Point3D(0d, 0d, 0d), 12d, 20d, 6)
  val data06 = pl.toEdge3D() ++ List(
    new Edge3D(List(pl.vrtx(true, 1), pl.vrtx(true, 5).xpls(10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 5), pl.vrtx(true, 5).xpls(10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 2), pl.vrtx(true, 4).xpls(-10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 4), pl.vrtx(true, 4).xpls(-10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 5).xpls(10d).zpls(90d), pl.vrtx(true, 4).xpls(-10d).zpls(90d)))
  ) 
  val tl03_m1200: Double => List[Edge3D] = (i: Double) => {
    val mod = i % 240
    val quo = i.toInt / 240
    if (mod <= 180) {
      (1 to quo).toList.foldLeft(List(new Edge3D(List()))) { (l, j) =>
        WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d - (60d * j.toDouble)*0.4, -100d, 600d)) ++ l}
    } else {
      (1 to quo).toList.foldLeft(List(new Edge3D(List()))) { (l, j) =>
        WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d - ((mod - 180d) + 60d * j)*0.4, -100d, 600d)) ++ l}
    }}

  val fl = new Rect(new Point3D(0d, 0d, 0d), 6d, 40d, 100d)
  val flapL = fl.toEdge3D() ++ List(
    new Edge3D(List(
      fl.vrtx(true, false, false), fl.vrtx(true, false, false).xpls(15d),
      fl.vrtx(true, false, true).xpls(15d), fl.vrtx(true, false, true).xpls(15d),
      fl.vrtx(true, false, true)))
  )
  val flapR = fl.toEdge3D() ++ List(
    new Edge3D(List(
      fl.vrtx(false, false, false), fl.vrtx(false, false, false).xpls(-15d),
      fl.vrtx(false, false, true).xpls(-15d), fl.vrtx(false, false, true).xpls(-15d),
      fl.vrtx(false, false, true)))
  )
  val tl02_m360: Double => List[Edge3D] = (i: Double) =>
    if (0<= i && i< 120) {
      WireFrameModel.translateEdge3D(flapL, new Point3D(-10d, -60d, 520d))++
      WireFrameModel.translateEdge3D(flapR, new Point3D(30d, -60d, 520d))
    } else if (120<= i && i< 180) {
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(WireFrameModel.translateEdge3D(flapL, new Point3D(0d, -40d, 0d)), 120d -1 * i, 0d, 0d), new Point3D(-10d, -20d, 520d))++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(WireFrameModel.translateEdge3D(flapR, new Point3D(0d, -40d, 0d)), i -120d, 0d, 0d), new Point3D(30d, -20d, 520d))
    } else {
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(WireFrameModel.translateEdge3D(flapL, new Point3D(0d, -40d, 0d)), (i -180d) -60d, 0d, 0d), new Point3D(-10d, -20d, 520d))++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(WireFrameModel.translateEdge3D(flapR, new Point3D(0d, -40d, 0d)), 60d - (i -180d), 0d, 0d), new Point3D(30d, -20d, 520d))
    }

  val tl01_m360: Double => List[Edge3D] = (i: Double) =>
    if (0<= i && i < 60) {
      WireFrameModel.rotateEdge3D(test05a, 0d, 30d, 0d) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05b, 0d, 30d, 0d), new Point3D(0d, 0d, 150d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05c, 0d, 30d - i, 0d), new Point3D(0d, 0d, 300d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 0d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 150d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 300d + 2.5 * i))

    } else if(60 <= i && i < 120) {
      WireFrameModel.rotateEdge3D(test05a, 0d, 30d, 0d) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05b, 0d, 90d - i, 0d), new Point3D(0d, 0d, 150d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05c, 0d, i - 90d, 0d), new Point3D(0d, 0d, 300d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 0d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 150d + 2.5 * (i - 60d))) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 300d + 2.5 * i))

    } else if(120 <= i && i < 180) {
      WireFrameModel.rotateEdge3D(test05a, 0d, 150d - i, 0d) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05b, 0d, i - 150d, 0d), new Point3D(0d, 0d, 150d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05c, 0d, 30d, 0d), new Point3D(0d, 0d, 300d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 0d + 2.5 * (i - 120d))) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 300d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d - (i-120), 600d))

    } else {
      WireFrameModel.rotateEdge3D(test05a, 0d, i - 210d, 0d) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05b, 0d, 30d, 0d), new Point3D(0d, 0d, 150d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(test05c, 0d, 30d, 0d), new Point3D(0d, 0d, 300d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 150d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d, -40d, 300d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(data06, 0d, 180d, 0d), new Point3D(15d - (i - 180)*0.4, -100d, 600d))
    }
}

object Astellas {
  val bottle_a = new Poly(new Point3D(0d, 0d, 0d), 12d, 5d, 8)
  val bottle_b = new Poly(new Point3D(0d, 0d, 5d), 10d, 5d, 8)
  val bottle_c = new Poly(new Point3D(0d, 0d, 10d), 8d, 5d, 8)
  val bottle_d = new Poly(new Point3D(0d, 0d, 15d),12d, 35d, 8)
  val bottle = bottle_a.toEdge3D() ++ bottle_b.toEdge3D() ++ bottle_c.toEdge3D() ++ bottle_d.toEdge3D()
  val bottles6_i_r_pi : (Double, Double, Double, Boolean) => List[Edge3D] = (i: Double, r: Double, pi: Double, flag: Boolean) => {
    val btls = 
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(bottle, r, 90d, 0d), new Point3D(   0d, 0d, 330d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(bottle, r, 90d, 0d), new Point3D(1 * i, 0d, 330d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(bottle, r, 90d, 0d), new Point3D(4 * i, 0d, 330d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(bottle, r, 90d, 0d), new Point3D(5 * i, 0d, 330d))

    val cond_btls =
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(bottle, r, 90d, 0d), new Point3D(2 * i, 0d, 330d)) ++
      WireFrameModel.translateEdge3D(WireFrameModel.rotateEdge3D(bottle, r, 90d, 0d), new Point3D(3 * i, 0d, 330d)) 
    if (flag) {WireFrameModel.rotateEdge3D(btls ++ cond_btls, 0d, 0d, pi)} else {WireFrameModel.rotateEdge3D(btls, 0d, 0d, pi)}
  }
  val yes: Boolean = true

  val global = 480
  val unit: Double => Double = (i: Double) => i / global
  val stop = 320
  val rot = -1 * 360d / (global - stop) 
  val t_m: Double => List[Edge3D] = (i: Double) =>
  if ((global * 0/4) <= i && i < (global * 1/4)) {
    if ((global * 0/12) <= i && i < (global * 1/12)) {
      bottles6_i_r_pi(24d, 0d, 0d, true)
    } else if ((global * 1/12) <= i && i < (global * 2/12)) {
      val interv = 24d + (i / (global * 4/12)) * 24d
      bottles6_i_r_pi(interv, 0d, 0d, true)
    } else {
      bottles6_i_r_pi(48d, 0d, rot * (i - (global * 2/12)), true)
    }
  } else if ((global * 1/4) <= i && i < (global * 2/4)) {
    if ((global * 3/12) <= i && i < (global * 4/12)) {
      bottles6_i_r_pi(48d, i, rot * (global * 1/12), true)
    } else if ((global * 4/12) <= i && i < (global * 5/12)) {
      bottles6_i_r_pi(48d, i, rot * (global * 1/12), true)
    } else {
      bottles6_i_r_pi(48d, 0d, rot * (i - (global * 2/12) * 2), true)
    }
  } else if ((global * 2/4) <= i && i < (global * 3/4)) {
    if ((global * 6/12) <= i && i < (global * 7/12)) {
      bottles6_i_r_pi(48d, 0d, rot * (global * 2/12), false)
    } else if ((global * 7/12) <= i && i < (global * 8/12)) {
      bottles6_i_r_pi(48d, 0d, rot * (global * 2/12), false)
    } else {
      bottles6_i_r_pi(48d, 0d, rot * (i - (global * 2/12) * 3), false)
    }
  } else {
    if ((global * 9/12) <= i && i < (global * 10/12)) {
      bottles6_i_r_pi(48d, 0d, rot * (global * 3/12), false)
    } else if ((global * 10/12) <= i && i < (global * 11/12)) {
      WireFrameModel.translateEdge3D(bottles6_i_r_pi(48d, 0d, rot * (global * 3/12), false), new Point3D((i - (global * 10/12)), 0d, 0d))
    } else {
      WireFrameModel.translateEdge3D(bottles6_i_r_pi(48d, 0d, rot * (global * 3/12), false), new Point3D((i - (global * 10/12)), 0d, 0d))
    }
  }
}

object Santen{
  val t_m: Double => List[Edge3D] = (i: Double) =>
  if (true) {
    List()
  } else {
    List()
  }
}
