import scalafx.Includes.handle
import scalafx.scene.control.Button
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.GridPane.{getColumnIndex, getRowIndex}
import scalafx.scene.layout.{GridPane, StackPane}
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.shape.Rectangle

import scala.jdk.CollectionConverters._

class BoardUI(var board: Board) {
  private val boardSize = 8

  def drawBoard(): GridPane = {
    val grid = new GridPane
    for (x <- 0 until boardSize; y <- 0 until boardSize) {
      var fillColor = if ((x + y) % 2 == 0) Color.rgb(235, 236, 211) else Color.rgb(125, 148 ,93)
      if (board.chosenX == y && board.chosenY == x) fillColor = Color.rgb(255, 255, 102)
      if (board.highlightedTiles.contains((y, x))) fillColor = Color.rgb(255, 255, 102)
      val square = new Rectangle {
        width = 80
        height = 80
        fill = fillColor
      }

      val stackPane = new StackPane()
      stackPane.children += square

      board.grid(y)(x).foreach { piece =>
        val pieceImage = new Image("file:" + "src/main/resources/" + piece.colorName + piece.name + ".png")
        val pieceImageView = new ImageView(pieceImage) {
          fitWidth = 60
          fitHeight = 60
        }
        stackPane.children += pieceImageView
      }

      val button: Button = new Button {
        minWidth = 80
        minHeight = 80
        style = "-fx-background-color: transparent;"
        onAction = _ => {
          println(y + " " + x)
          board.highlightTile(y, x)
          board.boardScene.content = board.UI.drawBoard()
        }
      }
      stackPane.children += button

      grid.add(stackPane, x, y)
    }
    grid
  }

}