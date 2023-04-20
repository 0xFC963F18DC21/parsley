/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import parsley.Parsley
import parsley.debugger.internal.{Found, Rename, ViaField, ViaMethod}
import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.token.Lexer

/** Miscellaneous utilities for enhancing the debugger.
  * Using these utilities is optional.
  */
package object util {
  /** Attempt to collect all the fields in a class or object that contain a
    * parser of type [[Parsley]].
    *
    * This information is used later in the debug tree-building process to rename certain parsers
    * so that they do not end up being named things like "packageanon".
    *
    * A caveat is that this only works with fields (lazy or otherwise). It will not work with
    * parsers defined in `def`-methods.
    *
    * You only need to run this once per parser-holding object.
    *
    * Currently, this method only fully functions on the JVM.
    *
    * @param obj Class or object to analyse.
    * @tparam A  Type of class or object to analyse.
    */
  def collectNames(obj: Any): Unit = {
    // Get the runtime class.
    val clazz = obj.getClass

    // Get all the getters for parser instances in a class.
    // We're using Parsley.unit as our dummy instance (type erasure at runtime saves us here).
    val methods = clazz.getDeclaredMethods.filter(m =>
      (m.getReturnType.isAssignableFrom(classOf[LazyParsley[_]])
        || m.getReturnType.isAssignableFrom(classOf[Parsley[_]]))
      && m.getParameterCount == 0
      && !m.getName.startsWith("copy"))

    // Note that depending on the Java version, setAccessible may print a warning or throw
    // an exception, so use with caution!
    val asMapM = methods.flatMap(mth => {
      // Try to make this method accessible if it is private.
      mth.setAccessible(true)

      // Extract our parser and add the method's name to our name map
      val contents = tryExtract(mth.invoke(obj))
      val name = if (mth.getName.contains("anonfun")) {
        mth.getName.split("\\$")(2)
      } else if (mth.getName.contains("lzycompute")) {
        mth.getName.split("\\$")(0)
      } else {
        mth.getName
      }

      List((ViaMethod(contents), name))
    }).toMap[Found[LazyParsley[_]], String]

    // Get all fields with a parser.
    val fields = clazz.getDeclaredFields.filter(f =>
      f.getClass.isAssignableFrom(classOf[LazyParsley[_]])
      || f.getClass.isAssignableFrom(classOf[Parsley[_]]))

    // Make a bunch of search functions from those fields.
    val asMapF = fields.flatMap(fld => {
      // Try to make this field accessible if it is private.
      fld.setAccessible(true)

      // Extract the internal parser and add its field name into our name map.
      val contents = tryExtract(fld.get(obj))
      List((ViaField(contents), fld.getName))
    }).toMap[Found[LazyParsley[_]], String]

    // Add our collected names into the global map.
    Rename.addNames(asMapM)
    Rename.addNames(asMapF)
  }

  // We want to automatically collect names for Parsley's internal parsers.
  collectNames(parsley.character)
  collectNames(parsley.combinator)
  collectNames(parsley.Parsley)
  collectNames(parsley.position)

  /** Attempt to collect names of parsers from a [[Lexer]] using repeated [[collectNames]] calls.
    *
    * This is useful if your parser makes heavy use of one.
    *
    * @param lexer
    */
  def collectLexer(lexer: Lexer): Unit = {
    collectNames(lexer)
    // TODO: Figure out why this fails miserably.
    // collectNames(lexer.space)
    collectNames(lexer.lexeme)
    collectNames(lexer.lexeme.names)
    collectNames(lexer.lexeme.text)
    collectNames(lexer.lexeme.enclosing)
    collectNames(lexer.lexeme.separators)
    collectNames(lexer.lexeme.symbol)
    collectNames(lexer.lexeme.numeric)
    collectNames(lexer.nonlexeme)
    collectNames(lexer.nonlexeme.names)
    collectNames(lexer.nonlexeme.numeric)
    collectNames(lexer.nonlexeme.symbol)
    collectNames(lexer.nonlexeme.text)

    val textPrivates = List(
      "_string", "_rawString", "_multiString", "_rawMultiString", "_character")
    val numericPrivates = List(
      "_integer", "_natural", "_real", "_positiveReal", "_signedCombined", "_unsignedCombined")

    { // Handle text private text members for lexeme
      val getters = lexer.lexeme.text.getClass.getMethods.filter(mth => textPrivates.contains(mth.getName))
      getters.foreach(_.setAccessible(true))
      for (getter <- getters) {
        collectNames(getter.invoke(lexer.lexeme.text))
      }
    }
    { // Handle text private text members for nonlexeme
      val getters = lexer.nonlexeme.text.getClass.getMethods.filter(mth => textPrivates.contains(mth.getName))
      getters.foreach(_.setAccessible(true))
      for (getter <- getters) {
        collectNames(getter.invoke(lexer.nonlexeme.text))
      }
    }
    { // Handle text private numeric members for lexeme
      val getters = lexer.lexeme.numeric.getClass.getMethods.filter(mth => numericPrivates.contains(mth.getName))
      getters.foreach(_.setAccessible(true))
      for (getter <- getters) {
        collectNames(getter.invoke(lexer.lexeme.numeric))
      }
    }
    { // Handle text private numeric members for nonlexeme
      val getters = lexer.nonlexeme.numeric.getClass.getMethods.filter(mth => numericPrivates.contains(mth.getName))
      getters.foreach(_.setAccessible(true))
      for (getter <- getters) {
        collectNames(getter.invoke(lexer.nonlexeme.numeric))
      }
    }
  }

  private def tryExtract(p: Any): LazyParsley[_] = {
    try {
      p.asInstanceOf[LazyParsley[_]]
    } catch {
      case _: ClassCastException => p.asInstanceOf[Parsley[_]].internal
    }
  }
}
