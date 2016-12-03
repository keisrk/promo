package promotion.matrix

class Matrix (p_data: Array[Array[Double]]) {
  val data = p_data map (_ clone)
  
  require(data.length > 0 && data(0).length > 0, "Matrix data table could not be empty")
  require(data.foldLeft(true)(_ && _.length == data(0).length), "Matrix rows have to contains the same number of elements")
  
  def this(rows: Int, cols: Int) = this ({
      require(rows > 0 && cols > 0, "Matrix data table could not be empty")
      Array.fill(rows, cols)(0d)
    })
  
  def load(lines: Iterator[String]) = {
    for (i <- 0 until rows) {
      require(lines.hasNext, "Exception in loading data: unexpected end of data")
      val tokens = lines.next.trim.split("\t")
      require(tokens.length == cols, "Exception in loading data: expected %d columns but was %d".format(cols, tokens.length))
      for (j <- 0 until tokens.length) 
        set(i, j, tokens(j).toDouble)
    }
  }
  
  def set(row: Int, col: Int, value: Double) = data(row)(col) = value
  def add(row: Int, col: Int, value: Double) = data(row)(col) += value
  def clear() = data.foreach(row => for (col <- row.indices) row(col) = 0)
  def get(row: Int, col: Int) = data(row)(col)

  def rows = data.length
  def getRow(rowNumber: Int) = new Matrix(Array(data(rowNumber) clone))  
  def cols = data(0).length    
  def getCol(columnNumber: Int) = new Matrix(data map {row: Array[Double] => Array(row(columnNumber))})
  
  def sum = data.flatten.sum
  def toPackedArray[Array[Double]] = data.flatten
    
  def isColumn = data(0).length == 1
  def isRow = data.length == 1
  def isZero = ! data.flatten.exists(_ != 0)

  // Precision of 10^-20 guarantees exact identity of double values for java/scala
  def equalsWithPrecision(other: Matrix, precision: Int = 20) =
    (data.length == other.data.length) && ((data flatten).length == (other.data flatten).length) &&
      ((toPackedArray zip other.toPackedArray).foldLeft(true)({
        (result, pair) => result && (math.abs(pair._1 - pair._2) * math.pow(10, precision) < 1)
      }))

  def dump = { val b = new StringBuilder(); data.foreach(row => b.append(row.mkString(" ")).append("\n")); b toString }
    
  override def clone = new Matrix(data)
  override def toString = "Matrix {%d x %d}".format(data.length, data(0).length)
  override def equals(other: Any) = other.isInstanceOf[Matrix] && equalsWithPrecision(other.asInstanceOf[Matrix])
  override def hashCode = java.util.Arrays.deepHashCode(data.asInstanceOf[Array[Object]])
}

object Matrix {
  def createColumnMatrix(data: Array[Double]) = new Matrix(data map (Array(_)))
  def createRowMatrix(data: Array[Double]) = new Matrix(Array(data))
}

object MatrixMath {
  def add(a: Matrix, b: Matrix) = {
    require(a.rows == b.rows && a.cols == b.cols, "Only matrixes with the same size can be summed: " + a + "; " + b)
    val data = a.toPackedArray zip b.toPackedArray map ({ case (aValue, bValue) => aValue + bValue })
    new Matrix((for(i <- 0 until a.rows) yield data.slice(i*a.cols, (i+1)*a.cols)) toArray)
  }

  def divide(a: Matrix, b: Double) = multiply(a, 1d /b)

  def dotProduct(a: Matrix, b: Matrix) = {
    require(a.isRow && b.isColumn, "Dot product is available only for row * column: " + a + "; " + b);
    require(a.cols == b.rows, "Matrix sizes are not match: " + a + "; " + b)
    (a.toPackedArray zip b.toPackedArray).foldLeft(0d)({ (sum, pair) => sum + pair._1 * pair._2 })
  }
  
  def identity(size: Integer) = new Matrix(
    Array.tabulate(size, size)({(rowIndex, colIndex) => if(rowIndex == colIndex) 1d else 0d})
  )
  
  def multiply(a: Matrix, b: Double) = new Matrix(a.data map ({ row => row map ({value: Double => value*b}) }))
  
  def multiply(a: Matrix, b: Matrix) = {       
    require(a.cols == b.rows, "Matrix sizes are not match: " + a + "; " + b)           
    new Matrix(
      Array.tabulate(a.rows, b.cols)( {(row, col) => 
          (for(i <- 0 until a.cols) yield a.data(row)(i) * b.data(i)(col)) sum // replace with while loop and get 10x performance
      })
    )
  }
  
  def subtract(a: Matrix, b: Matrix) = add(a, multiply(b, -1))
   
  def transpose(a: Matrix) = new Matrix(
    (for(i <- 0 until a.cols) yield a.getCol(i).toPackedArray) toArray
  )

  def vectorLength(a: Matrix) = {    
    require(a.isRow || a.isColumn, "Only single-row or single-column matrixes has vector length: " + a);
    math.sqrt(a.toPackedArray.foldLeft(0d)({ (sum, value) => sum + value*value }))
  }
}
