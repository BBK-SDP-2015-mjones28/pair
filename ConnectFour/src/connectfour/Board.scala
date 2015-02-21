package connectfour
import scala.collection.mutable.ListBuffer

/**
 * An instance represents a grid of pieces from two opposing
 * players in a game of Connect Four. The grid is 0-indexed first by rows
 * starting at the top, then by columns 0-indexed starting at the left.
 */


object Board {
  val NUM_ROWS = 6
  val NUM_COLS = 7

  def apply(b: Board) =
    new Board(b)
}

class Board {

  
  private val FOUR = 4

  /**
  * vertical, horizontal, uphill, downhill, directions from any position
  */
  private val deltas = Array(Array(1, 0), Array(0, 1), Array(-1, 1), Array(1, 1))

  //size of the board
  private val board = Array.ofDim[Player](Board.NUM_ROWS, Board.NUM_COLS)

   /**
     * Constructor: a duplicate of Board b.
   */
  def this(b: Board) 
  {
    this()
    for (r <- 0 until Board.NUM_ROWS; c <- 0 until Board.NUM_COLS) 
    {
      board(r)(c) = b.board(r)(c)
    }
  }

  /**
     * Return the element in row r col c.
     * Precondition: r and c give a position on the board
   */
  //returns type player
  def getPlayer(r: Int, c: Int): Player = 
  {
    //tests whether r is between 0 and 5, c is between 0 and 6
    assert(0 <= r && r < Board.NUM_ROWS && 0 <= c && c < Board.NUM_COLS)
    //board is an array of players - therefore returns player in position r & c
    board(r)(c) //return
  }

  /**
     * Constructor: a Board constructed by duplicating b and
     * applying nextMove to the new Board.
     */
  def this(b: Board, nextMove: Move) 
  {
    this(b)
    makeMove(nextMove)
  }

  /**
     * Return the Player at board position (row, col). Rows are
     * 0-indexed starting at the top and columns are 0-indexed starting
     * at the left. A null return value indicates an empty tile.
  */
  //possibly if empty tile (null) free to move
  def getTile(row: Int, col: Int): Player = board(row)(col)

  
  /**
     * Apply Move move to this Board by placing a piece from move's player into move's column
     * on this Board.  MOVE IS A CLASS
     * Throw an IllegalArgumentException if move's column is full on this Board.
  */
  
  //should call - possibly new board and new move
  def makeMove(move: Move): Unit =
  {
    //move = player and column 
 
    var num = 0
    for (l <- (Board.NUM_ROWS - 1) to 0 by -1)
    {      
      if (getTile(l, move.column) == null)
      {
        println("***DEBUG***: " + l)      
        if (l > num) num = l else l
        println(num)
       
      }
    }
     board(num)(move.column) = move.player

  }
//    }
      /**
     * Return an array of all moves that can possibly be made by Player p on this
     * board. The moves must be in order of increasing column number.
     * Note: The length of the array must be the number of possible moves.
     * Note: If the board has a winner (four things of the same colour in a row), no
     * move is possible because the game is over.
     * Note: If the game is not over, the number of possible moves is the number
     * of columns that are not full. Thus, if all columns are full, return an
     * array of length 0.
     */
  def getPossibleMoves(p: Player): Array[Move] = {
    val possibleMoves: Array[Move] = new Array(7)
    //only need to find empty columns
    for (col <- 0 to (Board.NUM_COLS - 1)) {
      if (getTile(0, col) == null) { 
        possibleMoves(col) = new Move(p,col);
      }
    }
    possibleMoves
  }
   
  override def toString(): String = toString("")

  def toString(prefix: String): String = {
    val str = new StringBuilder("")
    for (row <- board) {
      str.append(prefix + "|")
      for (spot <- row) {
        if (spot == null) {
          str.append(" |")
        } else if (spot == RED) {
          str.append("R|")
        } else {
          str.append("Y|")
        }
      }
      str.append("\n")
    }
    str.toString
  }

  def hasConnectFour(): Player = {
    winLocations().find(loc => loc(0) != null && loc(0) == loc(1) && loc(0) == loc(2) &&
      loc(0) == loc(3))
      .map(_(0))
      .orNull
  }

  def winLocations(): List[Array[Player]] = {
    val locations = ListBuffer[Array[Player]]()
    for (delta <- deltas; r <- 0 until Board.NUM_ROWS; c <- 0 until Board.NUM_COLS) {
      val loc = possibleWin(r, c, delta)
      if (loc != null) {
        locations += loc
      }
    }
    locations.toList
  }

  def possibleWin(r: Int, c: Int, delta: Array[Int]): Array[Player] = {
    val location = Array.ofDim[Player](FOUR)
    for (i <- 0 until FOUR) {
      val newR = r + i * delta(0)
      val newC = c + i * delta(1)
      if (0 <= newR && newR < Board.NUM_ROWS && 0 <= newC && newC < Board.NUM_COLS)
        location(i) = board(newR)(newC)
    }
    location
  }
}