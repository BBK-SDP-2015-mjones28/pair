package connectfour

import java.util.concurrent.Semaphore

class Human(private var player: Player) extends Solver {

  private var waitSema: Semaphore = new Semaphore(0)

  private var nextColumn: Int = _

  override def getMoves(b: Board): Array[Move] = {
    try {
      waitSema.acquire()
    } catch {
      case e: InterruptedException => e.printStackTrace()
    }
    //make a new move that will get checked within move class
    Array(new Move(player, nextColumn))
  }

  def columnClicked(c: Int) {
    nextColumn = c
    waitSema.release()
  }
}

