import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.application.Platform
import scalafx.scene.control.Label

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn.{readInt, readLine}

object App extends JFXApp {

    implicit val ec = ExecutionContext.global


    val b = new Board(8, true, 600, true)
    stage = new PrimaryStage {
        title = "Chess Board"
        scene = b.boardScene
    }

    // Test demo
    Future {
        // Simulate long running computation
        while (true) {
//            Thread.sleep(3000)
            val x = readInt()
//            b.move(b.grid(x)(y).get, (xTo, yTo), 0)

            // Update GUI
            Platform.runLater {
                b.boardScene.content = b.UI.drawBoard()
            }
        }
    }

}
