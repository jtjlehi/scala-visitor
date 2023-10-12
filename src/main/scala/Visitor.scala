sealed trait Element
case class Label(text: String) extends Element
case class Button(label: Label, body: Grid) extends Element
// this is a real grid...
case class Grid(label: Label, children: List[List[Element]]) extends Element
case class Window(grid: Grid) extends Element

object PartialFunctionVisitor {
  extension (el: Element)
    /** function which takes a visitor and returns a function that does the
      * visiting.
      *
      * uses the type of the element to determine how to visit the nodes
      */
    def accept[Ret](v: Visitor[Ret]): Ret => Ret = el match {
      case l: Label  => v.visit(el)
      case b: Button => v.visit(el, List(b.body, b.label))
      case g: Grid   => v.visit(el, g.label :: g.children.flatten)
      case w: Window => v.visit(el, List(w.grid))
    }

  /** in this variation a visit function is a function that returns a closure
    *
    * the closure has knowledge about the current element
    */
  trait Visitor[Ret] {
    def preVisit(el: Element): Ret => Ret
    def postVisit(el: Element): Ret => Ret

    def flatten(el: Element, children: List[Element] = Nil): List[Ret => Ret] =
      preVisit(el)
        :: children.flatMap(child =>
          child match {
            case l: Label  => flatten(child)
            case b: Button => flatten(child, List(b.body, b.label))
            // children.flatten because children is a list of a list
            // it turns out that a double nested list is the wrong abstraction
            case g: Grid   => flatten(child, g.label :: g.children.flatten)
            case w: Window => flatten(child, List(w.grid))
          }
        )
        ::: List(postVisit(el))

    def visit(el: Element, nodes: List[Element] = Nil): Ret => Ret =
      flatten(el, nodes).foldLeft(identity[Ret])(_ andThen _)

  }
  type PartVisitor[Ret] = PartialFunction[Element, Ret => Ret]
  // Creates a new visitor based on the provided partial functions
  def visitor[Ret](
      postVisitPart: PartVisitor[Ret],
      preVisitPart: PartVisitor[Ret] = PartialFunction.empty
  ) = new Visitor[Ret] {
    // for both of the default case, just pass the accumulator on
    def preVisit(el: Element) =
      preVisitPart.applyOrElse(el, _ => identity[Ret])
    def postVisit(el: Element) =
      postVisitPart.applyOrElse(el, _ => identity[Ret])
  }

  val defaultVisitor = visitor[Unit](PartialFunction.empty)
  val countVisitor = visitor[Int]({ case _: Label => (_ + 1) })
}

object TraitClassVisitor {

  trait Acceptor[Node] {
    // there must be a visitor which returns the requested type
    extension [Ret](node: Node)
      /** traverse the node using a given or provided visitor */
      def accept(acc: Ret)(using Visitor[Ret]): Ret
  }
  trait Visitor[Ret] {
    extension (node: Element)
      def preVisit(acc: Ret): Ret
      def postVisit(acc: Ret): Ret
  }

  given Acceptor[Label] with {
    extension [Ret](node: Label)
      def accept(acc: Ret)(using Visitor[Ret]): Ret =
        val out = node.preVisit(acc)
        node.postVisit(out)
  }
  given Acceptor[Button] with {
    extension [Ret](node: Button)
      def accept(acc: Ret)(using Visitor[Ret]): Ret =
        val out = node.label.accept(acc)
        node.body.accept(acc)
  }
  given Acceptor[Grid] with {
    extension [Ret](node: Grid)
      def accept(acc: Ret)(using Visitor[Ret]): Ret =
        // for
        //   rows <- node.children
        //   cell <- rows
        // do cell.accept(acc)  // <-- this won't compile because extensions use static dispatch
        // the workaround would be to add another extension
        // that does the lookup dynamically
        ???
  }
  given printVisitor: Visitor[Unit] with {
    extension (node: Element)
      def preVisit(acc: Unit): Unit = ???
      def postVisit(acc: Unit): Unit = ???
  }
}

object SeperateModule {
  import PartialFunctionVisitor.visitor
  val printVisitor = visitor[Int](
    { _ => (_ - 1) },
    // before going to children, increment depth and print
    {
      case Label(text) =>
        depth => {
          println(s"${"\t" * depth}Label - text: $text")
          depth + 1
        }
      case Button(_, _) =>
        depth => {
          println(s"${"\t" * depth}Button ")
          depth + 1
        }
      case Window(grid) =>
        depth => {
          println(s"${"\t" * depth}Window:")
          depth + 1
        }
      case Grid(label, children) =>
        depth => {
          println(s"${"\t" * depth}Grid:")
          depth + 1
        }
    }
  )
  // this is a visitor that keeps track of depth and nothing else
  // useful abstraction I think
  def depthVisitor(partVisit: PartialFunction[(Element, Int), Unit]) =
    visitor[Int](
      { _ => (_ - 1) },
      { el => depth =>
        {
          if partVisit.isDefinedAt((el, depth)) then partVisit((el, depth))
          depth + 1
        }
      }
    )
  def printDepth(s: String, depth: Int) = println(s"${"\t" * depth}$s")
  val printVisitor2 = depthVisitor({
    case (Label(text), depth)  => printDepth(s"Label - text - '$text'", depth)
    case (Button(_, _), depth) => printDepth(s"Button -", depth)
    case (Window(_), depth)    => printDepth(s"Window - ", depth)
    case (Grid(_, _), depth)   => printDepth(s"Grid -", depth)
  })
  // this is the most complicated visitor with 2 lines per element
  // which could be reduced if I reduced the variable names
  def printDepthVisitor(maxDepth: Int) = depthVisitor({
    case (Label(text), depth) if maxDepth >= depth =>
      printDepth(s"Label - text - '$text'", depth)
    case (Button(_, _), depth) if maxDepth >= depth =>
      printDepth(s"Button -", depth)
    case (Window(_), depth) if maxDepth >= depth =>
      printDepth(s"Window - ", depth)
    case (Grid(_, _), depth) if maxDepth >= depth =>
      printDepth(s"Grid -", depth)
  })
}
