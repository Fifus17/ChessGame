import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, signum}
class Board(val size: Int, val is_pvp: Boolean, max_time: Int, val is_bot: Boolean) {
  var grid: Array[Array[Option[Piece]]] = Array.ofDim[Option[Piece]](8, 8)
  for (i <- 0 until 8; j <- 0 until 8) {
    grid(i)(j) = None
  }

  //first black, second white
  var active_black: mutable.HashSet[Piece] = new mutable.HashSet()
  var active_white: mutable.HashSet[Piece] = new mutable.HashSet()
  var active: List[mutable.HashSet[Piece]] = List(active_black, active_white)
  var kings: List[King] = setup()
  var turn_color: Int = 0
  //self.bot = Bot(self, 3, 4)

  def who_starts(option: String):Unit ={
    val rand = new scala.util.Random
    val side = rand.nextInt(1)
    option match {
      case "Black" => turn_color = 0
      case "White" => turn_color = 1
      case "Random" => turn_color = side
    }
  }

  def end_turn(): Unit = {
    turn_color = 1 - turn_color
    //self.clock.switch_to(self.turn_color.opposite())
  }

  def setup(): List[King] = {
    val kingsBuffer = new ArrayBuffer[King]()
    for (row: Int <- List(0, size - 1)) {
      var color = 0
      if (row == 0)
        color = 1

      val rook_l: Rook = new Rook(row, 0, color)
      val knight_l: Knight = new Knight(row, 1, color)
      val bishop_l: Bishop = new Bishop(row, 2, color)
      val queen: Queen = new Queen(row, 3, color)
      val king: King = new King(row, 4, color)
      val bishop_r: Bishop = new Bishop(row, 5, color)
      val knight_r: Knight = new Knight(row, 6, color)
      val rook_r: Rook = new Rook(row, 7, color)

      grid(row)(0) = Some(rook_l)
      grid(row)(1) = Some(knight_l)
      grid(row)(2) = Some(bishop_l)
      grid(row)(3) = Some(queen)
      grid(row)(4) = Some(king)
      grid(row)(5) = Some(bishop_r)
      grid(row)(6) = Some(knight_r)
      grid(row)(7) = Some(rook_r)

      active(color).addAll(Seq(rook_l, rook_r, knight_r, knight_l, bishop_r, bishop_l, queen, king))
      kingsBuffer.addOne(king)
    }
    for (row <- List(1, size - 2)) {
      var color = 0
      if (row == 1)
        color = 1
      for (col: Int <- 0 until size) {
        val pawn: Pawn = new Pawn(row, col, color)
        grid(row)(col) = Some(pawn)
        active(color).addOne(pawn)
      }
    }
    List(kingsBuffer(1),kingsBuffer(0))
  }

  def get_path(pos1: (Int, Int), pos2: (Int, Int)): ArrayBuffer[(Int, Int)] = {

    /*
        Returns diagonal, horizontal or vertical path if such exists between two squares at
        positions pos1 and pos2.
    */
    val row_diff = pos2._1 - pos1._1
    val col_diff = pos2._2 - pos1._2
    val path = ArrayBuffer.empty[(Int, Int)]
    if (abs(row_diff) == abs(col_diff)) {
      //Path is diagonal
      val row_step = signum(row_diff)
      val col_step = signum(col_diff)
      for (i: Int <- 1 to abs(row_diff)) {
        path.addOne((pos1._1 + i * row_step, pos1._2 + i * col_step))
      }
      return path
    }
    if (row_diff == 0) {
      //Path is horizontal
      val col_step = signum(col_diff)
      for (i: Int <- 1 to abs(col_diff)) {
        path.addOne((pos1._1, pos1._2 + i * col_step))
      }
      return path
    }
    if (col_diff == 0) {
      //Path is vertical
      val row_step = signum(row_diff)
      for (i: Int <- 1 to abs(row_diff)) {
        path.addOne((pos1._1 + i * row_step, pos1._2))
      }
      return path
    }
    path
  }
  def is_blocked(pos: (Int, Int), piece: Piece): Boolean = {
    /*
        Checks whether piece has a line of sight to square at position pos.
    */
    if(piece.name=='N') {
      val x = pos._1
      val y = pos._2
      return grid(x)(y).isDefined && grid(x)(y).get.color==piece.color
    }
    val path = get_path(piece.pos, pos)
    val n: Int = path.size
    for (i: Int <- 0 until n) {
      val pathPoint = path(i)
      val row = pathPoint._1
      val col = pathPoint._2
      if (grid(row)(col).isDefined && (row,col)!=pos)
        return true
    }
    false
  }

  def get_available(piece: Piece): ArrayBuffer[(Int, Int)] = {
    /*
        Returns available squares to which `piece` can move.
        getting moves of opposite color is inevitable for checking opposite moves in bot
        */
    var row: Int = 0
    var col: Int = 0
    if (piece.color != turn_color && !is_bot)
      ArrayBuffer[(Int, Int)]()
    else {
      val inbounds = (row: Int, col: Int) => 0 <= row && row < size && 0 <= col && col < size
      val available_pos = ArrayBuffer[(Int, Int)]()
      for (move <- piece.moves) {
        if (piece.color == 1) {
          row = piece.row + move._1
          col = piece.col + move._2
        }
        else {
          row = piece.row - move._1
          col = piece.col - move._2
        }
        if (inbounds(row, col) && !is_blocked((row, col), piece) && (grid(row)(col).isEmpty || grid(row)(col).get.color != piece.color)) {
          available_pos.addOne((row, col))
          if(grid(row)(col)!=None){
          }
        }
      }
      if (is_check(piece.color)) {
        val available_pos_new = ArrayBuffer[(Int, Int)]()
        val king = kings(piece.color)
        val attackers = get_attacking(king.pos, 1 - king.color)
        if (piece == king) {
          for (pos <- available_pos) {
            if (get_attacking(pos, 1 - piece.color).isEmpty)
              available_pos_new.addOne(pos)
          }
          return available_pos_new
        }
        else if (attackers.size > 1) {
          return ArrayBuffer[(Int, Int)]()
        }
        else {
          val attacker = attackers(0)
          for (pos <- available_pos) {
            val path = get_path((attacker.row, attacker.col), (king.row, king.col))
            if (pos == (attacker.row, attacker.col) || path.contains(pos)) {
              available_pos_new.addOne(pos)
            }
          }
          return available_pos_new
        }
      }
      available_pos
    }
  }

  def get_attacking(pos: (Int, Int), attacking_color: Int): ArrayBuffer[Piece] = {
    /*
    Returns set of all `attacking_color` pieces which attack square at pos `position`
    */
    val attack_pieces = new ArrayBuffer[Piece]()
    for (attack_piece <- active(attacking_color)) {
      var row = 0
      var col = 0
      val positions: mutable.HashSet[(Int, Int)] = new mutable.HashSet[(Int, Int)]()
      for (move <- attack_piece.moves) {
        if (attack_piece.color == 1) {
          row = attack_piece.row + move._1
          col = attack_piece.col + move._2
        }
        else {
          row = attack_piece.row - move._1
          col = attack_piece.col - move._2
        }
        if (row != attack_piece.row || col != attack_piece.col) {
          positions += ((row, col))
        }
      }
      if (positions.contains(pos) && !is_blocked(pos, attack_piece))
        attack_pieces.addOne(attack_piece)
    }
    attack_pieces
  }

  def move(piece: Piece, new_position: (Int, Int),promotion: Char): Boolean = {
    /*Moves piece to new position i.e. changes its internal position and
    changes piece's position on the board stored in structures grid and active.*/
    piece.promote(promotion)
    val captured = grid(new_position._1)(new_position._2)
    if (captured.isDefined && captured.get.color == 1 - piece.color) {
      capture(captured.get)
    }
    grid(piece.row)(piece.col) = None
    grid(new_position._1)(new_position._2) = Some(piece)
    piece.place(new_position)
    List('Q','N','R','B').contains(promotion)
  }

  def revert_move(piece: Piece, captured: Option[Piece], old_position: (Int, Int),was_promoted:Boolean): Unit = {
    if(was_promoted)
      piece.degrade()
    if (captured.isDefined) {
      if (captured.get.color == piece.color) {
        println("error in reverting moves")
      }
      captured.get.place(piece.pos)
      active(1 - piece.color).addOne(captured.get)
    }
    grid(piece.row)(piece.col) = captured
    grid(old_position._1)(old_position._2) = Some(piece)
    piece.place(old_position)
  }

  def capture(captured_piece: Piece): Boolean = {
    active(captured_piece.color).remove(captured_piece)
  }

  def is_check(color: Int): Boolean = {
    val king = kings(color)
    val attackers = get_attacking((king.row, king.col), 1 - king.color)
    if (attackers.isEmpty)
      return false
    true
  }

  def is_checkmate(color: Int): Boolean = {

    val king = kings(color)
    val attackers = get_attacking((king.row, king.col), 1 - king.color)

    //Check if the king is attacked
    if (attackers.isEmpty)
      return false

    //Check if the king can escape or capture the attacking piece
    for (pos <- get_available(king)) {
      if (get_attacking(pos, 1 - king.color).isEmpty)
        return false
    }
    //Check if any piece can block the attack
    if (attackers.size == 1) {
      val attacker = attackers(0)
      val path = new mutable.HashSet[(Int,Int)]()
      var r = 0
      var c = 0
      path.addAll(get_path((king.row, king.col), (attacker.row, attacker.col)))
      print(path)
      for (defender <- active(king.color)) {
        for (move <- get_available(defender)) {
            r = move._1
            c = move._2
          if (path.contains(Tuple2(r, c))) {
            return false
          }
        }
      }
    }

    //Check if any piece can capture the attacking piece
    if (attackers.size == 1) {
      val attacker = attackers(0)
      val defenders = new ArrayBuffer[Piece]()
      defenders.addAll(get_attacking((attacker.row, attacker.col), king.color).diff(Seq(king)))
      if (defenders.nonEmpty)
        return false
    }
    true
  }

  def show(): Unit ={
    //os.system('cls')
    print("\u001b[2J")
    print("  ")
    for(col <- 0 until size){
      print(col+" ")
    }
    println()
    for (row <- 0 until size) {
      print(row+" ")
      for (col <- 0 until size) {
        if(grid(row)(col).isDefined) {
          if(grid(row)(col).get.color == 0) {
            print(Console.RED+grid(row)(col).get.name+" "+Console.WHITE)
          }
          else {
            print(grid(row)(col).get.name+" ")
          }
        }
        else
          print("· ")
      }
      println()
    }
  }
  print("\n")
}
object Appl {
  def main(args: Array[String]): Unit = {
    val b = new Board(8,true,600,true)
    b.setup()
    b.show()
    val p: Piece = b.grid(1)(1).get
    b.move(p,(3,3),'X')
    b.show()
  }
}