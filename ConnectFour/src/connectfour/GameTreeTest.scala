package connectfour

/**
 * Testing object - testing methods created using Asserts
 */
object GameTreeTest extends App{

  val p1 = new AI(RED,2)
  val p2 = new Human(YELLOW)
  
//testing board - make move and move

  val a = new Board()
  val move = new Move(RED,1)
  a.makeMove(move)
  println("Best move " + p1.evaluateBoard(a))
  assert(a.getTile(5, 1) == RED) //Passes as Expected  
  
  val move1 = new Move(YELLOW,4)
  a.makeMove(move1)
  println("Best move " + p1.evaluateBoard(a))
//  assert(a.getTile(5, 4) == RED)  //Fails successfully

  val move2 = new Move(RED,3)
  a.makeMove(move2)
  println("Best move " + p1.evaluateBoard(a))
  assert(a.getTile(5, 3) == RED)


  val move3 = new Move(YELLOW,2)
  a.makeMove(move3)  
  
  val move4 = new Move(RED,2)
  a.makeMove(move4)
 // assert(a.getTile(4, 2) == YELLOW)  //Fails successfully   
   
  val move5 = new Move(YELLOW,2)
  a.makeMove(move5)  
  
  val move6= new Move(RED,3)
  a.makeMove(move6)
  println("Best move " + p1.evaluateBoard(a))
  
 //Testing writing to file
  val currentState = new State(RED, a, move6)
  currentState.writeToFile()
  assert("output.txt" != null)
  
  //Test - board - getpossibleMoves
  val moves: Array[Move] = a.getPossibleMoves(RED)
//  assert(moves.length == 6)  //fails as expected
  
  val moves1: Array[Move] = a.getPossibleMoves(YELLOW)
  assert(moves1.length == 7)  //Passes as expected
  
  //fill column 2 - test whether column is now full or not
  val move7 = new Move(YELLOW,2)
  a.makeMove(move7)  
  
  val move8 = new Move(RED,2)
  a.makeMove(move8)
 // assert(a.getTile(4, 2) == YELLOW)  //Fails successfully   
   
  val move9 = new Move(YELLOW,2)
  a.makeMove(move9)  
  
  val moves3: Array[Move] = a.getPossibleMoves(YELLOW)
  assert(moves3.length == 6)  //Passes as expected
  
  
 //Initialise Children Test and Create Game Tree
  val newState = new State(RED, a, null)
  AI.createGameTree(newState, 2)
//  assert(newState.getChildren().isEmpty) //Fails as expected
  
  val newState1 = new State(RED, a, null)
  AI.createGameTree(newState1, 2)
  assert(!newState1.getChildren().isEmpty)  //Passes as expected
  
//test Min max and BestMove Visually
  AI.createGameTree(currentState, 1)  
  println("Best move " + p1.evaluateBoard(a))  
  println("MinMax" + p1.minimax(currentState)) 
  
  
}