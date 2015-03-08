package connectfour

import java.io.FileNotFoundException
import java.io.PrintWriter
import java.io.UnsupportedEncodingException
import State._
import scala.beans.BeanProperty

/**
 * An instance represents the state of a game of Connect Four.
 */

object State {

  val length0 = Array[State]()
}

class State(@BeanProperty var player: Player, @BeanProperty var board: Board, @BeanProperty var lastMove: Move) extends Comparable[Any] {

  /**
     * A State array of length 0.
     */
  @BeanProperty
  var children: Array[State] = length0

  @BeanProperty
  var value: Int = 0

    /**
     * Retrieves the possible moves and initializes this State's children.
     * The result is that this State's children reflect the possible States that can exist after the next move.
     * Remember, in the children it is the opposite player's turn. This method
     * initializes only this State's children; it does not recursively initialize all descendants.
     */
  def initializeChildren() {
    
    val arrayOfMoves = board.getPossibleMoves(player) //Array[Move]
    //opposite players move - this is to be called (believed) by createGameTree
    println("********************* " + arrayOfMoves.length)
    //children = next possible moves after our move
    val states = scala.collection.mutable.ArrayBuffer.empty[State]
    
    for (i <- 0 to arrayOfMoves.length -1)
    {
      //last moves - is this the move from the parent????
      
      var boardCopy: Board = new Board(board, arrayOfMoves(i))
      states += new State(player.opponent, boardCopy, lastMove)
    }
    children = states.toArray
  }

   /**
     * Write this State to a file called "output.txt", including its
     * children, their children, etc.. This method allows the State to
     * be viewed in a file even when it is too large to print to console.
     * Beep when printing is done.
     */
  def writeToFile() {
    try {
      var writer = new PrintWriter("output.txt", "UTF-8")
      writer.println(this)
      java.awt.Toolkit.getDefaultToolkit.beep()
    } catch {
      case e @ (_: FileNotFoundException | _: UnsupportedEncodingException) => e.printStackTrace()
    }
  }

  /**
     * Return a representation of this State.
     */
  override def toString(): String = {
    println("State.toString printing")
    toStringHelper(0, "")
  }

     /**
     * Return a string that contains a representation of this board indented
     * with string ind (expected to be a string of blank characters) followed
     * by a similar representation of all its children,
     * indented an additional ind characters. d is the depth of this state.
     */
  private def toStringHelper(d: Int, ind: String): String = {
    var str = ind + player + " to play\n"
    str = str + ind + "Value: " + value + "\n"
    str = str + board.toString(ind) + "\n"
    if (children != null && children.length > 0) {
      str = str + ind + "Children at depth " + (d + 1) + ":\n" + ind +
        "----------------\n"
      for (s <- children) {
        str = str + s.toStringHelper(d + 1, ind + "   ")
      }
    }
    str
  }

  override def compareTo(ob: AnyRef): Int = 0
}

