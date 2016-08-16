package nl.amsscala
package simplegame

import org.scalajs.dom

trait Page {
  // Create the canvas
  val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
  val ctx = canvas.getContext("2d")// .asInstanceOf[dom.CanvasRenderingContext2D]
  val (bgImage, heroImage, monsterImage) = (Image("img/background.png"), Image("img/hero.png"), Image("img/monster.png"))

  // Draw everything
  def render(gs: GameState) = {
    if (bgImage.isReady && heroImage.isReady && monsterImage.isReady) {
      ctx.drawImage(bgImage.element, 0, 0, canvas.width, canvas.height)
      gs.heroes.reverse.foreach(hero => ctx.drawImage(heroImage.element, hero.pos.x, hero.pos.y))
      ctx.drawImage(monsterImage.element, gs.monster.pos.x, gs.monster.pos.y)

      // Score
      ctx.fillStyle = "rgb(250, 250, 250)"
      ctx.font = "24px Helvetica"
      ctx.textAlign = "left"
      ctx.textBaseline = "top"
      ctx.fillText("Goblins caught: " + gs.monstersCaught, 32, 32)
      Some(gs)
    }
    else None
  }

  class Image(src: String, var isReady: Boolean = false) {
    val element = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]

    element.onload = (e: dom.Event) => isReady = true
    element.src = src
  }

  object Image {
    def apply(src: String) = new Image(src)
  }

  canvas.width = dom.window.innerWidth.toInt - 8
  canvas.height = dom.window.innerHeight.toInt - 38
  dom.document.body.appendChild(canvas)
}
