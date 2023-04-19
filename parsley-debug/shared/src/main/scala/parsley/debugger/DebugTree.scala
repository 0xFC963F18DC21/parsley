/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import scala.annotation.tailrec
import scala.collection.mutable

/** The tree representing a parser's parse tree.
  *
  * Initially unpopulated, it will be populated with information regarding the parser, such as
  * what it is (if it is a primitive such as [[parsley.internal.deepembedding.singletons.Pure]],
  * or a user-defined named parser if names are collected) as the parser itself runs on some input.
  *
  * Any tree node will store the input it has parsed (or attempted to parse) as well as its
  * success state as a list of [[ParseAttempt]] instances.
  *
  * Although this trait is unsealed, it is not useful to make a subtype of this trait, as this
  * trait's sole purpose is to provide safe methods into handling the frozen trees produced by
  * the debugger.
  */
trait DebugTree {
  /** The name of the parser that made this node. */
  def parserName: String

  /** The type name of the parser that formed this node. */
  def internalName: String

  /** A list of [[ParseAttempt]] instances that represent the chronological parse attempts made by
    * this parser.
    *
    * The topmost root node of a debug tree will always contain exactly one parse attempt, which is
    * the full input.
    */
  def parseResults: List[ParseAttempt]

  /** What are the child debug nodes for this node?
    *
    * The map provided by the implementation should be a linked map in order to preserve the
    * order of child parser occurrences within each parser.
    *
    * Internally, child nodes are given an arbitrary numeric suffix to disambiguate them in the map
    * if multiple child nodes have the same parser name.
    *
    * Those internal names are not represented if checking [[parserName]].
    */
  def nodeChildren: Map[String, DebugTree]

  /** Get the full input that was attempted to be parsed by the debugged parser.
    *
    * This is the whole input, unaltered, even parts where the parser did not attempt to parse.
    */
  def fullInput: String

  override def toString: String =
    prettyPrint(PrettyPrintHelper(new StringBuilder, Vector.empty)).acc.dropRight(1).toString()

  // Internal pretty-printer method.
  private def prettyPrint(helper: PrettyPrintHelper): PrettyPrintHelper = {
    val results = parseResults.map(printParseAttempt).mkString(", ")
    helper.bury(s"[ $parserName ]: $results")
    printChildren(helper, nodeChildren.toList)
    helper
  }

  // Print a parse attempt in a human-readable way.
  private def printParseAttempt(attempt: ParseAttempt): String =
    s"(\"${attempt.rawInput}\" [${attempt.fromPos} -> ${attempt.toPos}], ${if (attempt.success) "Success" else "Failure"})"

  // Print all the children, remembering to add a blank indent for the last child.
  @tailrec private def printChildren
    ( helper: PrettyPrintHelper
    , children: List[(String, DebugTree)]
    ): Unit =
    children match {
      case (_, t) :: Nil =>
        helper.bury("|", withMark = false)
        t.prettyPrint(helper.addBlankIndent())
      case (_, t) :: xs  =>
        helper.bury("|", withMark = false)
        t.prettyPrint(helper.addIndent())
        printChildren(helper, xs)
      case Nil           => ()
    }
}

// Utility class for aiding in the toString method for debug trees.
private case class PrettyPrintHelper(acc: mutable.StringBuilder, indents: Vector[String]) {
  // Indent a string with the given indenting delimiters.
  def bury(str: String, withMark: Boolean = true): Unit = {
    val pretty = if (indents.isEmpty) str
                 else if (withMark) indents.init.mkString + "+-" + str
                      else indents.mkString + str

    acc.append(pretty + "\n")
  }

  // Add a new indent delimiter to the current helper instance.
  // The accumulator is shared between new instances.
  def addIndent(): PrettyPrintHelper =
    PrettyPrintHelper(acc, indents :+ "| ")

  // Adds a two-blank-space indent instead for the last child of a node.
  def addBlankIndent(): PrettyPrintHelper =
    PrettyPrintHelper(acc, indents :+ "  ")
}
