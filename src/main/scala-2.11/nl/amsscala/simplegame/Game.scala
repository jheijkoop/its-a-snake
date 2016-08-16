package nl.amsscala
package simplegame

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode.{Down, Left, Right, Up}
import org.scalajs.dom.html
import org.scalajs.dom.html.Canvas

import scala.collection.mutable
import scala.scalajs.js

trait Game {

  def play(canvas: html.Canvas, headless: Boolean) {
    // Keyboard events store
    val keysPressed = mutable.Map.empty[Int, (Double, GameState)]
    var prev = js.Date.now()
    var oldUpdated: Option[GameState] = None

    // The main game loop
    def gameLoop = () => {
      val now = js.Date.now()
      val delta = now - prev
      val updated = oldUpdated.getOrElse(
        GameState.resetGame(canvas)
      ).updater(delta / 1000, keysPressed, canvas)

      if (oldUpdated.isEmpty || (oldUpdated.get.heroes.head.pos != updated.heroes.head.pos)) {
        oldUpdated = SimpleCanvasGame.render(updated)
      }

      prev = now
    }

    // Let's play this game!
    if (!headless) {
      dom.window.setInterval(gameLoop, 20)

      dom.window.addEventListener(
        "keydown", (e: dom.KeyboardEvent) =>
          e.keyCode match {
            case Left | Right | Up | Down if oldUpdated.isDefined => keysPressed += e.keyCode -> (js.Date.now(), oldUpdated.get)
            case _ =>
          }, useCapture = false
      )

      dom.window.addEventListener(
        "keyup", (e: dom.KeyboardEvent) => {
          keysPressed -= e.keyCode
        }, useCapture = false
      )
    }
  }
}

case class GameState(heroes: List[Hero], monster: Monster, monstersCaught: Int = 0) {
  def updater(modifier: Double, keysDown: mutable.Map[Int, (Double, GameState)], canvas: dom.html.Canvas): GameState = {
    val hero = heroes.head
    def directions = Map(
      Left -> Position(-1, 0), Right -> Position(1, 0), Up -> Position(0, -1), Down -> Position(0, 1)
    )

    val newHero = new Hero(
      keysDown.map(k => directions(k._1)). // Convert pressed keyboard keys to coordinates
        fold(hero.pos) { (z, i) => z + i * (400 * modifier).toInt }
    )
    val gridSize = 10
    // Compute new position by adding and multiplying.
    val snake: List[Hero] = (newHero :: heroes).take(gridSize * monstersCaught + 1)
    // Are they touching?
    if (!newHero.isValidPosition(canvas) || heroes.drop(2 * gridSize).exists(old => newHero.touch(old))) {
      GameState.resetGame(canvas)
    } else if (newHero.touch(monster)) {
      new GameState(snake, canvas, monstersCaught + 1)
    } else {
      copy(heroes = snake)
    }
  }

  def this(heroes: List[Hero], canvas: dom.html.Canvas, score: Int) = this(heroes, nextMonster(canvas), score)

  def nextMonster(canvas: Canvas): Monster = {
    Stream
      .continually(GameState.generateMonster(canvas))
//      .dropWhile(monster => heroes.exists(monster.touch)
      .head
  }
}

object GameState {
  def generateMonster(canvas: Canvas): Monster = {
    new Monster(
      Position(
        (math.random * (canvas.width - 64)).toInt,
        (math.random * (canvas.height - 64)).toInt
      )
    )
  }

  def resetGame(canvas: Canvas): GameState = {
    new GameState(
      List(Hero.freshHero(canvas)),
      canvas,
      0
    )
  }
}

trait Creature {
  val size = 30
  val pos: Position

  def touch(that: Creature): Boolean = pos.touch(that.pos, size / 2 + that.size / 2)
}

class Monster(val pos: Position) extends Creature

class Hero(val pos: Position) extends Creature {
  val speed = 400

  def isValidPosition(canvas: dom.html.Canvas): Boolean = pos.isInTheCanvas(canvas, size)
}

object Hero {
  def freshHero(canvas: Canvas): Hero = {
    new Hero(Position(canvas.width / 2, canvas.height / 2))
  }
}
