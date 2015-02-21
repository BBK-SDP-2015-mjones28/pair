package connectfour



 /**
     * Return this Solver's preferred Moves. If this Solver prefers one
     * Move above all others, return an array of length 1. Larger arrays
     * indicate equally preferred Moves.
     * An array of size 0 indicates that there are no possible moves.
     * Precondition: b is not null.
     */
trait Solver {

  def getMoves(b: Board): Array[Move]
}