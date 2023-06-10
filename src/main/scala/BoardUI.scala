import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{GridPane, StackPane}
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.shape.Rectangle

class BoardUI(var board: Board) {
  private val boardSize = 8

  def drawBoard(): GridPane = {
    val grid = new GridPane
    for (x <- 0 until boardSize; y <- 0 until boardSize) {
      var fillColor = if ((x + y) % 2 == 0) Color.rgb(235, 236, 211) else Color.rgb(125, 148 ,93)
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


      grid.add(stackPane, x, y)
    }
    grid
  }
}