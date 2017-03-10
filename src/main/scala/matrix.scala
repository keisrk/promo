package promotion.matrix

object DblMat {
  type Mat = Seq[Seq[Double]]
  def row(m: Mat, i: Int): Seq[Double] = m(i)
  def col(m: Mat, j: Int): Seq[Double] = for (r <- m) yield r(j)

  def init(row: Int, col: Int, f: (Int, Int) => Double): Mat = {
    for (i <- 0 until col) yield {
      for (j <- 0 until row) yield f(i, j)}
  }

  def s_dotp(a: Seq[Double], b: Seq[Double]): Seq[Double] = {
    assert(a.length == b.length)
    for ((ea, eb) <- a.zip(b)) yield ea * eb
  }

  def dotp(a: Mat, b: Mat, r: Int, c: Int): Mat = {
    init(r, c, (i: Int, j: Int) => s_dotp(row(a, i), col(b, j)).sum)
  }

  def reduce(v: Seq[Double], m: Mat): Seq[Double] = {
    for (i <- 0 until v.length) yield s_dotp(v, col(m, i)).sum
  }

  def sin(rot: Double): Double = Math.sin(Math.toRadians(rot))
  def cos(rot: Double): Double = Math.cos(Math.toRadians(rot))

  def rotx(rx: Double): Mat = {
    Seq(
      Seq(1d, 0d,      0d,           0d),
      Seq(0d, cos(rx), -1 * sin(rx), 0d),
      Seq(0d, sin(rx), cos(rx),      0d),
      Seq(0d, 0d,      0d,           1d))
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