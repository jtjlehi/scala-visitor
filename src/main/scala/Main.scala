import SeperateModule.printVisitor
import PartialFunctionVisitor.*
import SeperateModule.printVisitor2
import SeperateModule.printDepthVisitor
@main def hello: Unit =
  val gui = Window(
    Grid(
      Label("grid label"),
      List(
        List(
          Button(
            Label("button label"),
            Grid(
              Label("button grid label"),
              List(List(Label("inner button label")))
            )
          )
        ),
        List(Label("button label"))
      )
    )
  )
  gui.accept(printVisitor)(0)
  gui.accept(printVisitor2)(0)
  gui.accept(printDepthVisitor(3))(0)
