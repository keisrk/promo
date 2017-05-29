package sketch.svg

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

class SVG {
  def makeRect(id: String, x: Double, y: Double, w: Double, h: Double): svg.RectElement = {
    val rc = document.createElementNS("http://www.w3.org/2000/svg", "rect").asInstanceOf[raw.SVGRectElement]
    rc.id = id 
    rc.x.baseVal.value = x
    rc.y.baseVal.value = y
    rc.width.baseVal.value = w
    rc.height.baseVal.value = h
    rc.setAttribute("stroke", "black")
    rc.setAttribute("fill", "lightyellow")
    rc
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
        val rcL = document.createElementNS("http://www.w3.org/2000/svg", "rect").asInstanceOf[raw.SVGRectElement]
        val rcR = document.createElementNS("http://www.w3.org/2000/svg", "rect").asInstanceOf[raw.SVGRectElement]
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
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[raw.SVGPathElement]
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
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[raw.SVGPathElement]
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
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[raw.SVGPathElement]
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
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[raw.SVGPathElement]
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
        val p = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[raw.SVGPathElement]
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
  def makeLabel(root: svg.SVG, id: String, s: String, x: Double, y: Double, i: Int): Unit = {
    val tx = document.createElementNS("http://www.w3.org/2000/svg", "text").asInstanceOf[raw.SVGTextElement]
    tx.textContent = s
    tx.setAttribute("font-size", "20px")
    tx.setAttribute("font-family", "Courier New")
    val svgx = root.createSVGLength(); svgx.value = x 
    val svgy = root.createSVGLength(); svgy.value = y 
    tx.x.baseVal.initialize(svgx)
    tx.y.baseVal.initialize(svgy)
    root.appendChild(tx)
    val w = tx.getBBox().width
    val h = tx.getBBox().height
    svgx.value = x - w/2; tx.x.baseVal.initialize(svgx)
    svgy.value = y + h/2; tx.y.baseVal.initialize(svgy)
    val rc = makeRect(id, x - w/2 -5, y - h, w + 10, h * 2)
    root.insertBefore(rc, tx)
    decoRect(rc, i)
  }
  def setColor(img: svg.SVG, q: String, qs: List[String]): Unit = {
    for (p <- qs) {
      if (p == q){
        img.getElementById(q).setAttribute("fill", "turquoise")
      } else {
        img.getElementById(p).setAttribute("fill", "white")
      }
    }
  }
}
