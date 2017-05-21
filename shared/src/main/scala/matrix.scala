package sketch.matrix

object DblMat {
  type Mat = Seq[Seq[Double]]
  def row(m: Mat, i: Int): Seq[Double] = m(i)
  def col(m: Mat, j: Int): Seq[Double] = for (r <- m) yield r(j)

  def init(row: Int, col: Int, f: (Int, Int) => Double): Mat = {
    for (i <- 0 until col) yield {
      for (j <- 0 until row) yield f(i, j)}
  }

  def v_dotp(a: Seq[Double], b: Seq[Double]): Seq[Double] = {
    assert(a.length == b.length)
    for ((ea, eb) <- a.zip(b)) yield ea * eb
  }

  def dotp(a: Mat, b: Mat, r: Int, c: Int): Mat = {
    init(r, c, (i: Int, j: Int) => v_dotp(row(a, i), col(b, j)).sum)
  }
  def compose(tr: List[Mat]): Mat = tr match {
    case Nil => init(4, 4, ((i, j) => if (i == j) {1} else {0}))
    case t::l => l.foldLeft(t){(acc, m) => dotp(acc, m, 4, 4)}
  }
  def reduce(m: Mat, v: Seq[Double]): Seq[Double] = {
    for (r <- m) yield v_dotp(r, v).sum
  }

  def sin(rot: Double): Double = Math.sin(Math.toRadians(rot))
  def cos(rot: Double): Double = Math.cos(Math.toRadians(rot))
  def trans(dx: Double, dy: Double, dz: Double): Mat = {
    Seq(
      Seq(1d, 0d, 0d, dx),
      Seq(0d, 1d, 0d, dy),
      Seq(0d, 0d, 1d, dz),
      Seq(0d, 0d, 0d, 1d))
  }
  def rotx(rx: Double): Mat = {
    Seq(
      Seq(1d, 0d,      0d,         0d),
      Seq(0d, cos(rx), -1*sin(rx), 0d),
      Seq(0d, sin(rx), cos(rx),    0d),
      Seq(0d, 0d,      0d,         1d))
  }
  def roty(ry: Double): Mat = {
    Seq(
      Seq(cos(ry),    0d, sin(ry), 0d),
      Seq(0d,         1d, 0d,      0d),
      Seq(-1*sin(ry), 0d, cos(ry), 0d),
      Seq(0d,         0d, 0d,      1d))
  }
  def rotz(rz: Double): Mat = {
    Seq(
      Seq(cos(rz), -1*sin(rz), 0d, 0d),
      Seq(sin(rz), cos(rz),    0d, 0d),
      Seq(0d,      0d,         1d, 0d),
      Seq(0d,      0d,         0d, 1d))
  }
  def view(rz: Double, dx: Double, dz: Double): Mat = {
    Seq(
      Seq(cos(rz), -1*sin(rz), 0d, dx),
      Seq(sin(rz), cos(rz),    0d, 0d),
      Seq(0d,      0d,         1d, dz),
      Seq(0d,      0d,         0d, 1d))
  }

  val ex_a = Seq(
             Seq(2d, 6d, 1d, 3d),
             Seq(8d, 3d, 5d, 4d),
             Seq(4d, 1d, 2d, 9d),
             Seq(6d, 2d, 1d, 2d))
  val ex_b = Seq(
             Seq(3d, 2d, 8d, 3d),
             Seq(1d, 7d, 3d, 1d),
             Seq(6d, 2d, 1d, 7d),
             Seq(1d, 1d, 3d, 2d))
  val ex_a_0 = col(ex_a, 0); val ex_a_1 = col(ex_a, 1); val ex_a_2 = col(ex_a, 2)
  val ex_b_0 = col(ex_b, 0); val ex_b_1 = col(ex_b, 1); val ex_b_2 = col(ex_b, 2)
}
