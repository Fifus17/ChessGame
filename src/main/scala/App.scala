import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color._

object App extends JFXApp {

    val b = new Board(8, true, 600, true)
    val boardUI = new BoardUI(b)
    stage = new PrimaryStage {
        title = "Chess Board"
        scene = new Scene {
        fill = LightGray
        content = boardUI.drawBoard()
        }
    }
    b.show()
//    val bot1 = new Bot(b, 3, 3)
//    val bot2 = new Bot(b, 3, 3)
//    bot1.play_against_bot(bot2)

}
