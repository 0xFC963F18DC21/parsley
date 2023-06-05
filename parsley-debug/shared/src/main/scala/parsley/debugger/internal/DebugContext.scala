/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable.ListBuffer

import parsley.debugger.ParseAttempt

import parsley.internal.deepembedding.frontend.{Iterative, LazyParsley}

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside an implicit variable.
private [parsley] class DebugContext {
  // Create a new dummy root of the tree that will act as filler for the rest of the tree to build
  // off of (as there is no "nil" representation for the tree... other than null, which should be
  // avoided in Scala wherever possible).
  private def dummyRoot: DebugTreeBuilder =
    DebugTreeBuilder(TransientDebugTree("ROOT", "ROOT", "NIL"))

  // Tracks where we are in the parser callstack.
  private var builderStack: ListBuffer[DebugTreeBuilder] =
    ListBuffer(dummyRoot)

  // Current raw parser stack.
  private var parserStack: ListBuffer[LazyParsley[_]] = ListBuffer()

  // Iterative child depth.
  private var iterativeChildren: ListBuffer[(Int, Int)] = ListBuffer()

  // Get the final DebugTreeBuilder from this context.
  def getFinalBuilder: DebugTreeBuilder =
    builderStack.head.bChildren.collectFirst { case (_, x) => x }.get

  // Add an attempt of parsing at the current stack point.
  def addParseAttempt(attempt: ParseAttempt): Unit =
    builderStack.head.node.parse = Some(attempt)

  // Is the top parser iterative?
  def topIsIterative(): Boolean =
    parserStack.headOption.exists(_.isInstanceOf[Iterative])

  // Is the parser just under the top iterative?
  def secondIsIterative(): Boolean =
    parserStack.drop(1).headOption.exists(_.isInstanceOf[Iterative])

  def pushIterative(count: Int): Unit =
    iterativeChildren.prepend((count, count))

  def popIterative(): Unit = {
    iterativeChildren.remove(0)
    () // XXX: Silences discarded non-unit value warning.
  }

  def setIterativeChildren(count: Int): Unit = {
    val (mx, _) = iterativeChildren.head
    iterativeChildren.remove(0)
    iterativeChildren.prepend((mx, count))
    () // XXX: Silences discarded non-unit value warning.
  }

  def getIterativeMax(): Int =
    iterativeChildren.headOption.getOrElse((0, 0))._1

  def decrementIterativeChildren(): Boolean = {
    val (mx, c) = iterativeChildren.head
    iterativeChildren.remove(0)
    iterativeChildren.prepend((mx, if (c == 1) mx else c - 1))

    c == 1
  }

  // Reset this context back to zero.
  def reset(): Unit = {
    iterativeChildren = ListBuffer()
    builderStack = ListBuffer(dummyRoot)
    parserStack = ListBuffer()
  }

  // Push a new parser onto the parser callstack.
  def push(fullInput: String, parser: LazyParsley[_]): Unit = {
    lazy val uniq: Unique[LazyParsley[_]] = Unique(parser)
    parserStack = parserStack.prepend(parser)

    if (builderStack.head.bChildren.contains(uniq)) {
      builderStack.prepend(builderStack.head.bChildren(uniq))
    } else {
      val newTree = TransientDebugTree(fullInput = fullInput)
      newTree.name = Rename(parser)
      newTree.internal = Rename.partial(parser)

      val dtb = DebugTreeBuilder(newTree)

      builderStack.head.bChildren(uniq) = dtb
      builderStack.prepend(dtb)
    }
  }

  // Pop a parser off the parser callstack.
  def pop(): LazyParsley[_] =
    if (builderStack.isEmpty) {
      // Shouldn't happen, but just in case.
      //noinspection ScalaStyle
      throw new IllegalStateException(
        "WARNING: Parser stack underflow on pop. This should not have happened!"
      )
    } else {
      // Remove first parser off stack, as if returning from that parser.
      builderStack.remove(0)
      parserStack.remove(0)
    }
}
