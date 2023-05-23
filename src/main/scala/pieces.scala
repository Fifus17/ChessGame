import scala.collection.mutable.ArrayBuffer
object PieceVariables{

  def rMoves: List[(Int, Int)] = {
    val L: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()
    for (i <- 1 to 7) {
      L.addOne((i,0))
      L.addOne((0,i))
      L.addOne((0,-i))
      L.addOne((-i,0))
    }
    Some(L).toList.flatten
  }

  def bMoves: List[(Int, Int)] = {
    val L: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()
    for (i <- 1 to 7) {
      L.addOne((i, i))
      L.addOne((-i, i))
      L.addOne((i, -i))
      L.addOne((-i, -i))
    }
    Some(L).toList.flatten
  }

  val pawnMoves: List[(Int, Int)] = List((1, 0))
  val kingMoves: List[(Int, Int)] = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
  val knightMoves: List[(Int, Int)] = List((2, 1), (1, 2), (-2, 1), (-1, 2), (-1, -2), (-2, -1), (1, -2), (2, -1))
  val bishopMoves: List[(Int, Int)] = bMoves
  val rookMoves: List[(Int, Int)] = rMoves
  val queenMoves: List[(Int, Int)] = bishopMoves ++ rookMoves
}

trait Piece {
  var row: Int
  var col: Int
  val color:Int
  var value: Double
  var name: Char
  var moves: List[(Int, Int)]
  var has_moved: Boolean = false
  def place(pos:(Int,Int)): Unit ={
    row = pos._1
    col = pos._2
  }
  def attack_moves: List[(Int, Int)] = {
    if(this.name=='P')
      return List((1,1),(1,-1))
    this.moves
  }
  def pos: (Int, Int) = (row, col)
  def canPromote: Boolean ={
    if(name=='P'&&(color == 1 && row ==7 ||color ==0 && row==0)){
      return true
    }
    false
  }
  def promote(new_name:Char): Unit ={}
  def degrade(): Unit ={}
}
class Pawn(var row:Int, var col: Int, val color:Int) extends Piece{
  var value = 1.0
  var name ='P'
  var moves: List[(Int, Int)] = PieceVariables.pawnMoves
  override def promote(new_name:Char): Unit ={
    new_name match{
      case 'Q' =>
        value = 10.0
        moves = PieceVariables.queenMoves
      case 'N' =>
        value = 3.5
        moves = PieceVariables.knightMoves
      case 'R' =>
        value = 5.25
        moves = PieceVariables.rookMoves
      case 'B' =>
        value = 3.5
        moves = PieceVariables.bishopMoves
      case _ =>
        value = 1.0
        moves = PieceVariables.pawnMoves
    }
    if(new_name=='B'||new_name=='R'||new_name=='Q'||new_name=='N')
      name = new_name
  }
  override def degrade(): Unit ={
    value = 1.0
    name ='P'
    moves = PieceVariables.pawnMoves
  }
}
class King(var row:Int, var col: Int, val color:Int) extends Piece{
  var value = 50_000
  var name ='K'
  var moves: List[(Int, Int)] = PieceVariables.kingMoves
}
class Rook(var row:Int, var col: Int, val color:Int) extends Piece{
  var value = 5.25
  var name ='R'
  var moves: List[(Int, Int)] = PieceVariables.rookMoves
}
class Bishop(var row:Int, var col: Int, val color:Int) extends Piece{
  var value = 3.5
  var name ='B'
  var moves: List[(Int, Int)] = PieceVariables.bishopMoves
}
class Knight(var row:Int, var col: Int, val color:Int) extends Piece{
  var value = 3.5
  var name ='N'
  var moves: List[(Int, Int)] = PieceVariables.knightMoves
}
class Queen(var row:Int, var col: Int, val color:Int) extends Piece{
  var value = 10.0
  var name ='Q'
  var moves: List[(Int, Int)] = PieceVariables.queenMoves
}
object Bppl {
  def main(args: Array[String]): Unit = {
    val p:Pawn = new Pawn(3,3,0)
    val r:Rook = new Rook(3,3,0)
    val b:Bishop = new Bishop(3,3,0)
    val n:Knight = new Knight(3,3,0)
    val k:King = new King(3,3,0)
    val q:Queen = new Queen(3,3,0)
    val board = new Board(8,true,600,true)
    board.setup()
    println(board.get_available(p))
    println(board.get_available(r))
    println(b.moves)
    println(n.moves)
    println(k.moves)
    println(q.moves)
  }
}