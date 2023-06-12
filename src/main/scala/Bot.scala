import play.api.libs.json.Json
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.stage.Stage

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
class Bot(val board: Board, val depth: Int, val width: Int) {
  val matrix: List[List[List[Float]]] = read_matrix()
  val pieces_dict: Map[Char, Int] = Map(
    'P' -> 0,
    'N' -> 1,
    'Q' -> 2,
    'B' -> 3,
    'R' -> 4,
    'K' -> 5
  )

  implicit val ec = ExecutionContext.global

  def read_matrix(): List[List[List[Float]]] ={
    val jsonString = os.read(os.pwd / "src" / "main" / "scala" / "values.json")
    val jsonValue = Json.parse(jsonString)
    val floats = (jsonValue \ "valueMatrix").as[List[List[List[Float]]]]
    floats
  }
  def matrix_position(position:(Int,Int), color: Int): (Int, Int) = {
    val x = position._1
    val y = position._2
    if (color == 1)
      return (x, y)
    (7 - x, 7 - y)
  }

  private def test_move(x: Int, y: Int, piece: Piece, promotion: Char,first_move: Boolean): Double={
    val old_pos = piece.pos
    val captured = board.grid(x)(y)
    val was_promoted = board.move(piece, (x, y),promotion)
    val move_result = evaluate(piece.color)
    board.revert_move(piece, captured, old_pos,was_promoted,first_move)
    move_result
  }
  def pieceValueOf(a: Piece): Double = {a.value}

  def positionalValueOf(a: Piece): Float = {
    val position:(Int,Int) = matrix_position(a.pos,a.color)
    matrix(pieces_dict(a.name))(position._1)(position._2)
  }
  private def evaluate(color: Int): Double = {
    if(board.get_attacking(board.kings(color).pos,1-color).nonEmpty)
      return -5_000
    if(board.is_checkmate(color))
      return -50_000
    var side_0 = board.active(color).toList.map(pieceValueOf).sum
    var side_1 = board.active(1-color).toList.map(pieceValueOf).sum
    side_0 += board.active(color).toList.map(positionalValueOf).sum
    side_1 += board.active(1-color).toList.map(positionalValueOf).sum
    side_0 - side_1
  }
  private def add_move(best_queue: mutable.PriorityQueue[Move], piece: Piece): Unit = {
    val possible = board.get_available(piece)
    if(board.is_check(piece.color)) {
    }
    var i =0
    for(position <- possible){
      i+=1
      val x = position._1
      val y = position._2
      if(piece.canPromote) {
        for (promotion <- List('H', 'Q', 'R', 'B')) {
          val value = test_move(x, y, piece, promotion,!piece.has_moved)
          best_queue.enqueue(new Move(x, y, piece, value, promotion))
        }
      }
      val value = test_move(x, y, piece, 'P',!piece.has_moved)
      best_queue.enqueue(new Move(x, y, piece, value, 'P'))
    }
  }
  def moveOrder(m: Move): Double = m.value

  private def test_best_moves_depth1(color: Int): List[Move]={
    /*returns up to n best moves
    checking all would take too long*/
    val best : mutable.PriorityQueue[Move] = new mutable.PriorityQueue[Move]()(Ordering.by(moveOrder))
    val result = new ArrayBuffer[Move]
    for(piece <- board.active(color)) {
      add_move(best, piece)
    }
    var i = 0
    while(i < width && best.nonEmpty) {
      val move = best.dequeue()
      result.addOne(move)
      i+=1
    }
    result.toList
  }
  def best_move(color: Int, current_depth: Int): Option[Move]= {
        /*function, that returns the best move for given depth
        tests width moves for depth, for each calculating an opposite move with depth -1*/
    if (current_depth == 1) {
      val result = test_best_moves_depth1(color)
      if(result.nonEmpty && result.head.value > -10_000){
        return Some(result.head)
      }
      None
    }
    else {
      val potentialMoves = test_best_moves_depth1(color)
      var moveNo1: Option[Move] = None
      var OpponentMoveVal = 1000000.0
      for(move <- potentialMoves) {
        val x = move.x
        val y = move.y
        val piece = move.piece
        val old_pos = piece.pos
        val captured = board.grid(x)(y)
        val wasPromoted = board.move(piece, (x, y),move.promotion)
        val opposite_move = best_move(1 - color, current_depth - 1)
        board.revert_move(piece, captured, old_pos, wasPromoted,!piece.has_moved)
        if(opposite_move.isDefined && opposite_move.get.value< OpponentMoveVal) {
          moveNo1 = Some(new Move(x,y,piece,-opposite_move.get.value, move.promotion))
          OpponentMoveVal = opposite_move.get.value
        }
      }
      moveNo1
    }
  }

  def move(): Unit = {
    val move = best_move(1, depth)
    if (move.isEmpty) {
      if (board.is_check(1))
        println("mat")
      println("pat")
      return 2
    }
    val m = move.get
    board.move(m.piece, (m.x, m.y), m.promotion)
    board.end_turn()
  }
  def play_against_bot(bot:Bot): Int = {
          /*function for testing bot
          A bot vs bot game: returns COLOR of winner
          :param bot: another bot to play with*/
    var i = 0
    val bots: List[Bot] = List(this, bot)
    while(i < 200) {
        val color = board.turn_color
        if (board.is_checkmate(color)) {
          println("mat")
          return 1 - color
        }
        val move = bots(i % 2).best_move(color, depth)
        if (move.isEmpty) {
          if (board.is_check(color))
            println("mat")
          println("pat")
          return 2
        }
        val m = move.get
        board.move(m.piece, (m.x, m.y), m.promotion)
        board.end_turn()
        Thread.sleep(500)
        board.show()
        i += 1
    }
    val result = evaluate(1)
    if(result > 0)
      return 1
    0
  }
}
//object Cppl {
//  def main(args: Array[String]): Unit = {
//    val b = new Board(8,true,600,true)
//    b.show()
//    val bot1 = new Bot(b,3,3)
//    val bot2 = new Bot(b,3,3)
//    bot1.play_against_bot(bot2)
////    val board = document.createElement("board").asInstanceOf[html.Div]
//  }
//}