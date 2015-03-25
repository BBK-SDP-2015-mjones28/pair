package connectfour


class AI(private var player: Player, private var depth: Int) extends Solver {


  override def getMoves(b: Board): Array[Move] = {
    
    val bestMoves = scala.collection.mutable.ArrayBuffer.empty[Move]
    val currentState = new State(player, b, null)
    AI.createGameTree(currentState, depth)
    this.minimax(currentState) 
    
   // currentState.writeToFile() // check results in output.txt
    
    
    val tempChildren = currentState.getChildren
    
    //sort states by value 
    tempChildren.sortBy(x => x.value)
    
    tempChildren.filter(child => child.getValue == currentState.getValue)
                .map(child => bestMoves += child.getLastMove)
                
    bestMoves.toArray
  }  
    
  /**
   * State s is a node of a game tree (i.e. the current State of the game).
   * Use the Minimax algorithm to assign a numerical value to each State of the
   * tree rooted at s, indicating how desirable that java.State is to this player.
   */
  def minimax(s: State) : Unit = 
  {
     getMaxOrMin(s,true) 
      def getMaxOrMin(s: State, flag: Boolean): Int = 
      {    
       if(s.getChildren().isEmpty)
       {
         s.setValue(evaluateBoard(s.getBoard))
         s.getValue
       } 
       else {
         flag match {
                     case true => {     
                                   val bestMoves = scala.collection.mutable.ArrayBuffer.empty[Int]
                                   s.getChildren.map{child => bestMoves+= getMaxOrMin(child, false)}
                                   s.setValue(bestMoves.max)
                                   bestMoves.max

                                   }
                     case false => {
                                    val bestMoves = scala.collection.mutable.ArrayBuffer.empty[Int]
                                    s.getChildren.map{child => bestMoves+= getMaxOrMin(child, true)}
                                    s.setValue(bestMoves.min)
                                    bestMoves.min
                                   }
                     }
             }
     }    
  
  }  




  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
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
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

object AI {

  /**
   * Generate the game tree with root s of depth d.
   * The game tree's nodes are State objects that represent the state of a game
   * and whose children are all possible States that can result from the next move.
   *
   * NOTE: this method runs in exponential time with respect to d.
   * With d around 5 or 6, it is extremely slow and will start to take a very
   * long time to run.
   *
   * Note: If s has a winner (four in a row), it should be a leaf.
   */
  def createGameTree(s: State, d: Int): Unit = {
    if (d > 0) {
      s.initializeChildren()
      val tempChildren = s.getChildren()
      tempChildren.foreach(child => createGameTree(child, d-1))
    }
  }
   
  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

