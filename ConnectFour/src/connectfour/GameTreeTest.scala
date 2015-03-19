package connectfour

object GameTreeTest extends App{

  val p1 = new AI(RED,2)
  val p2 = new Human(YELLOW)
  
  //First placement
  val a = new Board()
  val move = new Move(RED,1)
  a.makeMove(move)
  println("Best move " + p1.evaluateBoard(a))

  
  
  val move1 = new Move(YELLOW,4)
  a.makeMove(move1)
  println("Best move " + p1.evaluateBoard(a))


  val move2 = new Move(RED,3)
  a.makeMove(move2)
  println("Best move " + p1.evaluateBoard(a))


  val move3 = new Move(YELLOW,2)
  a.makeMove(move3)  
  
    val move4 = new Move(RED,2)
  a.makeMove(move4)
  
    val move5 = new Move(YELLOW,2)
  a.makeMove(move5)
  
  
  val move6= new Move(RED,3)
  a.makeMove(move6)
    println("Best move " + p1.evaluateBoard(a))
  
  
  val currentState = new State(RED, a, move6)
    
  
 // AI.createGameTree(currentState, 1)
  
  println("Best move " + p1.evaluateBoard(a))
  
  
  println("MinMax" + p1.minimax(currentState))
  
  
  
  currentState.writeToFile()
}