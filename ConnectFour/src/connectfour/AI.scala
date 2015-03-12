package connectfour


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
  override def getMoves(b: Board): Array[Move] =
  {
    
    //martin odersky Corsera Video
    def mergeSort[T](xs: List[T])(f: (T, T) => Boolean): List[T] =
    {
    val n = xs.length / 2
    if(n == 0) xs
    else
    {
      def merge(xs:List[T], ys: List[T]):List[T] =      
        (xs,ys) match
        {
           case (Nil, ys) => ys
           case(xs, Nil) => xs
           //head and tail, head and tail
           case(x::xs1, y::ys1) => if(f(x,y)) x::merge(xs1,ys) 
                                   else y::merge(xs,ys1)                  
        }
      val(fst, snd) = xs splitAt n
      merge(mergeSort(fst)(f), mergeSort(snd)(f))
    }
  }    
    
    val moves = b.getPossibleMoves(player)
    // var arrState = Array[State]()
    var arrState = scala.collection.mutable.ArrayBuffer.empty[State]

    var arr = Array[Move]()
   
    
    for (i <- 0 to moves.length-1)
    {
      //get the max move of this states children
      var s = new State(player, b, moves(i))
    //  AI.createGameTree(s,depth)
        AI.createGameTree(s,depth)

      println(minimax(s))
      //s now hold the best of its children
      
     
      //at this point s should have a value and all its children should have value
      
      //need to look at this
          val tempArrayState = scala.collection.mutable.ArrayBuffer.empty[State]

         // var tempArrayState = Array[State]()
          for (j <- 0 to s.getChildren().length -1)
          {
           // if(s.getValue().equals(s.children(j).getValue()))
             // {
             //tempArrayState(j) = s.children(j)
              tempArrayState+= s.children(j)
              //possibly get last state here
             // }
          }
          var list = tempArrayState.toList
          
          
          for (i <- 0 to tempArrayState.length - 1)
          {
            println ("****DEBUG**** - array state values: " +  tempArrayState(i).getValue())
          }
          
          //list is passing list to the function and also the function we wish to use
          list = mergeSort(list)((x: State, y: State) => x.value < y.value)
          println("****DEBUG**** AI list Sort: " + list)
          
          //after sort pick lowest and add that lowest to the array of states
          var holdingArray: Array[State] = Array[State]() 
          holdingArray = list.toArray
//          println("****DEBUG**** AI list toArray " + holdingArray)
          
          //here take the first one
          if(holdingArray.length != 0)
          arrState += holdingArray(0)
          else
            println("error here!! no array found")
                  
    }    

    //have to make a move to get into state
    //here we need to make those moves
    var moveArray = scala.collection.mutable.ArrayBuffer.empty[Move]

    //var moveArray: Array[Move] = Array[Move]() 
    
    for (i <- 0 to arrState.length -1 )
    {
      moveArray += arrState(i).getLastMove()
    }
    
     
    //need to add moves to array and then sort the array in order 
    
    // Now visit each of the nodes recursively, looking for the
    var returnArray: Array[Move] = moveArray.toArray
    
    returnArray
        
    }
    
  
    /**
     * State s is a node of a game tree (i.e. the current State of the game).
     * Use the Minimax algorithm to assign a numerical value to each State of the
     * tree rooted at s, indicating how desirable that State is to this player.
     */
  def minimax(s: State) {   
    
    //if player = max value
    //if AI/opponent = min value
    
    s.value = evaluateBoard(s.getBoard())
    
//    println("In MINMAX - at depth: " + depth + " value: " + s.getValue())
           
    for (i <- 0 to depth)
    {
      for (j <- 0 to s.children.length-1)
       {
        minimax(s.children(j))
       }
    }
    
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

