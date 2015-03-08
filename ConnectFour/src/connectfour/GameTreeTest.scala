package connectfour

object GameTreeTest extends App{

  val p1 = new AI(RED,2)
  val p2 = new Human(YELLOW)
  
  //First placement
  val a = new Board()
  val move = new Move(RED,1)
  a.makeMove(move)
   
  val move1 = new Move(YELLOW,4)
  a.makeMove(move1)
  

  val move2 = new Move(RED,3)
  a.makeMove(move2)
  

  val move3 = new Move(YELLOW,2)
  a.makeMove(move3)
  
  val currentState = new State(YELLOW, a, move3)
  
  AI.createGameTree(currentState, 1)
  currentState.writeToFile()
}