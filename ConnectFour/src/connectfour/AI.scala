package connectfour

import java.util.List
import AI._
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An instance represents a Solver that intelligently determines
 * Moves using the Minimax algorithm.
 */

object AI {

  
   /**
     * Generate the game tree with root s of depth d.
     * The game tree's nodes are State objects that represent the state of a game
     * and whose children are all possible States that can result from the next move.
     * <p/>
     * NOTE: this method runs in exponential time with respect to d.
     * With d around 5 or 6, it is extremely slow and will start to take a very
     * long time to run.
     * <p/>
     * Note: If s has a winner (four in a row), it should be a leaf.
     */
  def createGameTree(s: State, d: Int) 
  {      
    s.initializeChildren()
    for (i <- 0 to d)
    {
      for (j <- 0 to s.children.length-1)
       {
        createGameTree(s.children(j), d-1)
      }
    }
    
    
//    if(d==0) s
//    s.initializeChildren()
//    createGameTree(s.children, d-1)

//    
//    
//    creategameTree(s.childre)
//    s.initializeChildren()
//    //for each state - initialise the children of that state
//    for(i <- 0 to 3) //d -1)
//    {
//      for(j <- i to s.children.length)
//      createGameTree(s.children(j), d-1)  
//    }
    
    
  }

  
  /**
     * Call minimax in ai with state s.
     */
  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

 /**
     * Constructor: an instance with player p who searches to depth d
     * when searching the game space for moves.
     */
class AI(private var player: Player, private var depth: Int) extends Solver {

  
    /**
     * See Solver.getMoves for the specification.
     */
  override def getMoves(b: Board): Array[Move] = ???

  
  
    /**
     * State s is a node of a game tree (i.e. the current State of the game).
     * Use the Minimax algorithm to assign a numerical value to each State of the
     * tree rooted at s, indicating how desirable that java.State is to this player.
     */
  def minimax(s: State) {
  }

  
   /**
     * Evaluate the desirability of Board b for this player
     * Precondition: b is a leaf node of the game tree (because that is most
     * effective when looking several moves into the future).
     */
  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (winner == null) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

