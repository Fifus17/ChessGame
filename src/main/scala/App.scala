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


    stage = new PrimaryStage {
        title = "Chess Board"
        scene = new Scene()
    }

    val b = new Board(8, true, 600, false, stage)


}
