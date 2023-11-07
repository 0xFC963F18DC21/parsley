/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.immutable.ListMap
import scala.collection.mutable

import parsley.debugger.{DebugTree, ParseAttempt}

/** A mutable implementation of [[DebugTree]], used when constructing the tree as a parser is
  * running.
  *
  * When viewing / analysing the parse tree, it is highly advised to call
  * [[TransientDebugTree#freeze]] to obtain a frozen, immutable version of the debug tree.
  *
  * @param name Name of parser.
  * @param parse What attempts to parse have been made?
  * @param children This debug tree node's children.
  */
private [parsley] case class TransientDebugTree(
    var name: String = "",
    var internal: String = "",
    fullInput: String,
    var parse: Option[ParseAttempt] = None,
    children: mutable.Map[String, TransientDebugTree] = new mutable.LinkedHashMap()
) extends DebugTree {
    override def parserName: String = name

    override def internalName: String = internal

    // The pair stores the input the parser attempted to parse and its success.
    override def parseResults: Option[ParseAttempt] = parse

    override def nodeChildren: Map[String, DebugTree] =
        children.foldRight[ListMap[String, DebugTree]](ListMap())((p, acc) => acc + p)

    /** Freeze the current debug tree into an immutable copy.
      *
      * It is highly advised to do this before analysing the tree.
      *
      * @return An anonymous immutable copy of this tree.
      */
    def freeze: DebugTree = {
        // Freeze any mutable values by copying them.
        // Also freeze all child trees because we don't want to have to manually freeze the whole tree.
        val immName = name

        val immInternal = internal

        val immParse = parse

        val immChildren = children.map {
            case (n, t: TransientDebugTree) => (n, t.freeze)
        }.toMap

        val immInp = fullInput

        // There doesn't seem to be much of a point in making a whole new class for immutable trees
        // as pattern-matching is less of a worry.
        new DebugTree {
            override def parserName: String = immName

            override def internalName: String = immInternal

            override def parseResults: Option[ParseAttempt] = immParse

            override def nodeChildren: Map[String, DebugTree] = immChildren

            override def fullInput: String = immInp
        }
    }
}
