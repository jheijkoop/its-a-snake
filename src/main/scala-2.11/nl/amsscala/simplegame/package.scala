package nl.amsscala

import org.scalajs.dom

package object simplegame {

  case class Position(x: Int, y: Int) {
    def +(p: Position) = Position(x + p.x, y + p.y)

    def *(factor: Int) = Position(x * factor, y * factor)

    def isInTheCanvas(canvas: dom.html.Canvas, size: Int): Boolean =
      0 <= x && (x + size) <= canvas.width && 0 <= y && (y + size) <= canvas.height

    def touch(posB: Position, size: Int): Boolean = {
      x <= (posB.x + size) &&
        posB.x <= (x + size) &&
        y <= (posB.y + size) &&
        posB.y <= (y + size)
    }
  }

}
