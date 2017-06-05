package sketch.svg
import scala.collection.mutable.ListBuffer
import org.scalajs.dom

import dom.document
import dom.svg
import dom.raw

abstract class Control
case class Terminal() extends Control
case class Process() extends Control
case class Decision() extends Control
case class RoopBgn() extends Control
case class RoopEnd() extends Control

class SVG(img: svg.SVG) {
  def draw(v: List[(String, String, Int, Double, Double)], e: List[(String, String)]): Unit = {
    val boxes = ListBuffer[svg.Element]()
    val edges = ListBuffer[svg.PathSeg]()
    val qs = for ((id, l, i, xpos, ypos) <- v) yield {
      val q = label(id, l, xpos, ypos)
      val qminX = q.getBBox().x
      val qminY = q.getBBox().y
      val w = q.getBBox().width
      val h = q.getBBox().height
      val qmaxY = qminY +h
      boxes += box(id, xpos -w/2, qmaxY -h, w, h, i); q
    }
    for (q <- qs) {
      for ((l, r) <- e) {
        if (q.id == l) {
          qs.find(p => p.id == r) match {
            case None => {}
            case Some(p) => edges ++= connect(q, p, false)
          }
        } else if (l.startsWith(q.id) && l.endsWith("_else")) {
          qs.find(p => p.id == r) match {
            case None => {}
            case Some(p) => edges ++= connect(q, p, true)
          }
        } else {

        }
      }
    }
    val path = document.createElementNS("http://www.w3.org/2000/svg", "path")
      .asInstanceOf[raw.SVGPathElement]
    path.pathSegList.initialize(edges.head)
    for (e <- edges.tail) {
      path.pathSegList.appendItem(e)
    }
    path.id = "connecting_edge"
    path.setAttribute("stroke", "black")
    path.setAttribute("fill", "white")

    img.appendChild(path)
    for (b <- boxes) {img.appendChild(b)}
    for (q <- qs) {img.appendChild(q)}
  }//sliding

  def arrange(lbls: List[List[(String, String, Int)]], spanX: Double, spanY: Double): List[(String, String, Int, Double, Double)] = {
    lbls.zipWithIndex.foldLeft(List[(String, String, Int, Double, Double)]()) {
      case (acc, (ls, i)) => acc ++ ls.zipWithIndex.collect {
        case ((id, l, box), j) if id != "NA" => (id, l, box, (i+1) * spanX, (j+1) * spanY)
      }
    }
  }
  def rectangle(s: String, xpos: Double, ypos: Double, w: Double, h: Double): svg.RectElement = {
    val rc = document.createElementNS("http://www.w3.org/2000/svg", "rect")
      .asInstanceOf[raw.SVGRectElement]
    rc.id = s
    rc.width.baseVal.value = w
    rc.height.baseVal.value = h
    rc.setAttribute("stroke", "black")
    rc.setAttribute("fill", "white")
    rc.x.baseVal.value = xpos
    rc.y.baseVal.value = ypos
    rc
  }

  def setColor(q: String): Unit = {
    for (e <- 0 to img.childNodes.length -1) img.childNodes.item(e) match {
      case sh:svg.Path if sh.id contains q => sh.setAttribute("fill", "turquoise")
      case sh:svg.Path if sh.id == "connecting_edge" => {}
      case sh:svg.Path => sh.setAttribute("fill", "white")
      case sh:svg.RectElement if sh.id contains q => sh.setAttribute("fill", "turquoise")
      case sh:svg.RectElement => sh.setAttribute("fill", "white")
      case _ => {}
    }
  }
  def label(id_s: String, s: String, xpos: Double, ypos: Double): svg.Text = {
    val tx = document.createElementNS("http://www.w3.org/2000/svg", "text")
      .asInstanceOf[raw.SVGTextElement]
    tx.id = id_s
    tx.textContent = s
    tx.setAttribute("font-size", "20px")
    tx.setAttribute("font-family", "Courier New")

    val svgx = img.createSVGLength(); svgx.value = xpos
    val svgy = img.createSVGLength(); svgy.value = ypos
    tx.x.baseVal.initialize(svgx)
    tx.y.baseVal.initialize(svgy)
    img.appendChild(tx)
    val w = tx.getBBox().width
    val h = tx.getBBox().height
    svgx.value = xpos - w/2; tx.x.baseVal.initialize(svgx)
    svgy.value = ypos + h/2; tx.y.baseVal.initialize(svgy)
    tx
  }
  def connect(qt: svg.Text, pt: svg.Text, offset: Boolean): List[svg.PathSeg] = {
    val path = document.createElementNS("http://www.w3.org/2000/svg", "path")
      .asInstanceOf[raw.SVGPathElement]

    val spanX = 120
    val spanY = 40
    val qw = qt.getBBox().width
    val qh = qt.getBBox().height
    val qmaxY = qt.getBBox().y +qh +qh/2
    val qcx = if (offset) {
      qt.getBBox().x +qh +qw/2
    } else {
      qt.getBBox().x +qw/2
    }
    val qcy = if (offset) {
      qmaxY -qh
    } else {
      qmaxY
    }

    val pw = pt.getBBox().width
    val ph =    pt.getBBox().height
    val pminX = pt.getBBox().x
    val pminY = pt.getBBox().y -ph/2
    val pcx = pminX + pw/2

    if (qcx == pcx) {
      if (qmaxY < pminY) {
        List(
        path.createSVGPathSegMovetoAbs(qcx, qcy),
        path.createSVGPathSegLinetoRel(0, pminY -qcy))
      } else {
        List(
        path.createSVGPathSegMovetoAbs(qcx, qcy),
        path.createSVGPathSegLinetoRel(0, spanY),
        path.createSVGPathSegLinetoRel(-spanX, 0),
        path.createSVGPathSegLinetoRel(0, pminY -qcy -spanY *2),
        path.createSVGPathSegLinetoRel(spanX, 0),
        path.createSVGPathSegLinetoRel(0, spanY),
        path.createSVGPathSegLinetoRel(-5, -10),
        path.createSVGPathSegLinetoRel(5, 0))
      }
    } else {
      val diff = qcx - pcx
      if (qmaxY < pminY) {
        List(
        path.createSVGPathSegMovetoAbs(qcx, qcy),
        path.createSVGPathSegLinetoRel(0, pminY -qcy -spanY),
        path.createSVGPathSegLinetoRel(-diff, 0),
        path.createSVGPathSegLinetoRel(0, spanY))
      } else {
        val cspanY = if (offset) 0 else spanY
        List(
        path.createSVGPathSegMovetoAbs(qcx, qcy),
        path.createSVGPathSegLinetoRel(0, cspanY),
        path.createSVGPathSegLinetoRel(-diff/2, 0),
        path.createSVGPathSegLinetoRel(0, pminY -qcy -(cspanY + spanY)),
        path.createSVGPathSegLinetoRel(-diff/2, 0),
        path.createSVGPathSegLinetoRel(0, spanY),
        path.createSVGPathSegLinetoRel(-5, -10),
        path.createSVGPathSegLinetoRel(5, 0))
      }
    }
  }
  def box(id_s: String, xpos: Double, ypos: Double, w: Double, h: Double, i: Int): svg.Element = {
    val path = document.createElementNS("http://www.w3.org/2000/svg", "path")
      .asInstanceOf[raw.SVGPathElement]

    i match {
      case 0 => rectangle(id_s, xpos, ypos -h/2, w, 2*h)
      case 1 => {
        val rc = rectangle(id_s, xpos -10, ypos -h/2, w +20, 2*h)
        rc.rx.baseVal.value = h/2; rc.ry.baseVal.value = h/2
        rc
      }
/*
      case 2 => {

        val es = List(
          new MoveTo{x = xpos; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new MoveTo{x = xpos +w; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new MoveTo{x = xpos -10; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new LineTo{absolute = false; x = w +20; y = 0},
          new LineTo{absolute = false; x = 0; y = -2*h},
          new LineTo{absolute = false; x = -(w +20); y = 0}
        )
        new Path{
          id = id_s
          stroke = Black
          fill = White
          elements = es
        }
      }*/
      case 3 => {
        val m2 = path.createSVGPathSegMovetoAbs(xpos -w/2, ypos +h/2)
        path.pathSegList.initialize(m2) 
        path.pathSegList.appendItem(path.createSVGPathSegLinetoRel(w, -h))
        path.pathSegList.appendItem(path.createSVGPathSegLinetoRel(w, h))
        path.pathSegList.appendItem(path.createSVGPathSegLinetoRel(-w, h))
        path.pathSegList.appendItem(path.createSVGPathSegLinetoRel(-w, -h))

        path.id = id_s
        path.setAttribute("stroke", "black")
        path.setAttribute("fill", "white")

        path
      }/*
      case 4 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos -h/2},
          new LineTo{absolute = false; x = -10; y = h},
          new LineTo{absolute = false; x = 10; y = h},
          new LineTo{absolute = false; x = w -10; y = 0},
          new ArcTo{ absolute = false; x = 0; y = -2*h
            radiusX = h; radiusY = h; XAxisRotation = 180},
          new LineTo{absolute = false; x = 10 -w; y = 0}
        )
        new Path{
          id = id_s
          stroke = Black 
          fill = White
          elements = es
        }
      }
      case 5 => {
        val es = List(
          new MoveTo{x = xpos; y = ypos -h/2},
          new LineTo{absolute = false; x = 0; y = 2*h},
          new LineTo{absolute = false; x = w; y = 0},
          new LineTo{absolute = false; x = 0; y = -2*h},
          new LineTo{absolute = false; x = -20; y = -10},
          new LineTo{absolute = false; x = -w +40; y = 0},
          new LineTo{absolute = false; x = -20; y = 10}
        )
        new Path{
          id = id_s
          stroke = Black 
          fill = White
          elements = es
        }
      }*/
    }
  }

  def decoRect(rc: svg.RectElement, i: Int): Unit = {
    val id = rc.id
    val x = rc.x.baseVal.value 
    val y = rc.y.baseVal.value 
    val w = rc.width.baseVal.value 
    val h = rc.height.baseVal.value
    i match {
      case 0 => {}
      case 1 => {
        rc.x.baseVal.value -= 10
        rc.width.baseVal.value += 20
        rc.rx.baseVal.value = h/2; rc.ry.baseVal.value = h/2}
      case 2 => {
        val rcL = document.createElementNS("http://www.w3.org/2000/svg", "rect")
          .asInstanceOf[raw.SVGRectElement]
        val rcR = document.createElementNS("http://www.w3.org/2000/svg", "rect")
          .asInstanceOf[raw.SVGRectElement]
        rcL.x.baseVal.value = x - 10
        rcL.y.baseVal.value = y
        rcL.width.baseVal.value = 10 
        rcL.height.baseVal.value = h
        rcL.setAttribute("stroke", "black")
        rcL.setAttribute("fill-opacity", "0.0")
        rcR.x.baseVal.value = x + w 
        rcR.y.baseVal.value = y
        rcR.width.baseVal.value = 10 
        rcR.height.baseVal.value = h
        rcR.setAttribute("stroke", "black")
        rcR.setAttribute("fill-opacity", "0.0")
        rc.parentNode.appendChild(rcL)
        rc.parentNode.appendChild(rcR)
        rc.x.baseVal.value -= 10
        rc.width.baseVal.value += 20
      }
      case 3 => {
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path")
          .asInstanceOf[raw.SVGPathElement]
        val m2 = p.createSVGPathSegMovetoAbs(x - w/2, y + h/2)
        p.pathSegList.initialize(m2) 
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(w, -h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(w, h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-w, h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-w, -h))
        p.setAttribute("stroke", "black")
        p.setAttribute("fill", rc.getAttribute("fill"))
        rc.parentNode.replaceChild(p, rc)
        p.id = id
     }
      case 4 => {
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path")
          .asInstanceOf[raw.SVGPathElement]
        val m2 = p.createSVGPathSegMovetoAbs(x, y)
        p.pathSegList.initialize(m2) 
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-10, h/2))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(10, h/2))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(w -10, 0))
        p.pathSegList.appendItem(p.createSVGPathSegArcRel(0, -h, h/2, h/2, 180, false, false))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(10 -w, 0))
        p.setAttribute("stroke", "black")
        p.setAttribute("fill", rc.getAttribute("fill"))
        rc.parentNode.replaceChild(p, rc)
        p.id = id
      }
      case 5 => {
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path")
          .asInstanceOf[raw.SVGPathElement]
        val m2 = p.createSVGPathSegMovetoAbs(x , y)
        p.pathSegList.initialize(m2) 
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(0, h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(w , 0))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(0, -h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-20, -10))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-w +40, 0))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-20, 10))
        p.setAttribute("stroke", "black")
        p.setAttribute("fill", rc.getAttribute("fill"))
        rc.parentNode.replaceChild(p, rc)
        p.id = id
      }

      case 6 => {
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path")
          .asInstanceOf[raw.SVGPathElement]
        val m2 = p.createSVGPathSegMovetoAbs(x , y)
        p.pathSegList.initialize(m2) 
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(0, h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(20, 10))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(w -40, 0))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(20, -10))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(0, -h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-w, 0))
        p.setAttribute("stroke", "black")
        p.setAttribute("fill", rc.getAttribute("fill"))
        rc.parentNode.replaceChild(p, rc)
        p.id = id
      }

      case 7 => {
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path")
          .asInstanceOf[raw.SVGPathElement]
        val m2 = p.createSVGPathSegMovetoAbs(x , y)
        p.pathSegList.initialize(m2) 
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-20, h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(w +20, 0))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(20, -h))
        p.pathSegList.appendItem(p.createSVGPathSegLinetoRel(-w-20, 0))
        p.setAttribute("stroke", "black")
        p.setAttribute("fill", rc.getAttribute("fill"))
        rc.parentNode.replaceChild(p, rc)
        p.id = id
      }
    }
  }
}
