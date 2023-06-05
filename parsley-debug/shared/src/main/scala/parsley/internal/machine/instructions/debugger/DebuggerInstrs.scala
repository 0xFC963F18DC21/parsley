/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.debugger

import parsley.debugger.ParseAttempt
import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.Accumulator
import parsley.internal.deepembedding.frontend.debugger.helpers.childCount
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.{Instr, InstrWithLabel}

// Instructions used by the debugger itself.
private [internal] sealed trait DebuggerInstr extends Instr

// Enter into the scope of a parser in the current context.
private [internal] class EnterParser
  (var label: Int, origin: LazyParsley[_])
  (implicit dbgCtx: DebugContext) extends InstrWithLabel with DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    // I think we can get away with executing this unconditionally.
    dbgCtx.push(ctx.input, origin)

    // If iterative, push the counter.
    if (dbgCtx.topIsIterative()) {
      val (init, rest) = childCount(origin)
      dbgCtx.pushIterative(rest)
      dbgCtx.setIterativeChildren(init)
      dbgCtx.push(ctx.input, Accumulator)
    }

    ctx.pushCheck() // Save our location for inputs.
    ctx.pushHandler(label) // Mark the AddAttempt instruction as an exit handler.
    ctx.inc()
  }

  override def toString: String = s"EnterParser(exit: $label)"
}

// Add a parse attempt to the current context at the current callstack point, and leave the current
// parser's scope.
private [internal] class AddAttemptAndLeave(implicit dbgCtx: DebugContext) extends DebuggerInstr {
  // ix is 0-indexed.
  def nthPeek(ctx: Context, ix: Int): Any = {
    // Assumes a length check has already been done
    if (ix == 0) ctx.stack.peek.asInstanceOf[Any]
    else {
      val top    = ctx.stack.pop().asInstanceOf[Any]
      val result = nthPeek(ctx, ix - 1)

      ctx.stack.push(top)
      result
    }
  }

  //noinspection ScalaStyle
  override def apply(ctx: Context): Unit = {
    // XXX: This is a very long method that could probably be simplified.

    // These offsets will be needed to slice the specific part of the input that the parser has
    // attempted to parse during its attempt.
    val prevCheck = ctx.checkStack.offset
    val currentOff = ctx.offset

    // Slice based on current offset to see what a parser has attempted to parse,
    // and the 'good' member should indicate whether the previous parser has succeeded or not.
    // We add 1 to currentOff to see what character caused the parse failure.
    val input = ctx.input.slice(prevCheck, if (ctx.good) currentOff else currentOff + 1)
    val success = ctx.good

    val prevPos = {
      val inpAsLines = ctx.input.slice(0, prevCheck + 1).split('\n')
      (inpAsLines.length, inpAsLines.last.length)
    }

    // Construct a new parse attempt and add it in.
    // XXX: Cast to Any required as otherwise the Some creation is treated as dead code.
    dbgCtx.addParseAttempt(
      ParseAttempt(
        input,
        prevCheck,
        if (ctx.good) currentOff else currentOff + 1,
        prevPos,
        if (ctx.good) (ctx.line, ctx.col - 1) else (ctx.line, ctx.col),
        success,
        if (success) Some(ctx.stack.peek.asInstanceOf[Any]) else None
      )
    )

    // See above.
    if (dbgCtx.topIsIterative()) dbgCtx.popIterative()
    dbgCtx.pop()
    ctx.checkStack = ctx.checkStack.tail // Manually pop off our debug checkpoint.

    // If the top of the parser stack after popping is iterative, add the current accumulator value,
    // which should be the second value in the stack, but only if it is time to do so (all children
    // have been evaluated).
    if (dbgCtx.secondIsIterative() && dbgCtx.decrementIterativeChildren()) {
      dbgCtx.addParseAttempt(
        ParseAttempt(
          input,
          if (ctx.good) currentOff - 1 else currentOff,
          if (ctx.good) currentOff - 1 else currentOff,
          if (ctx.good) (ctx.line, ctx.col - 2) else (ctx.line, ctx.col - 1),
          if (ctx.good) (ctx.line, ctx.col - 2) else (ctx.line, ctx.col - 1),
          success = ctx.stack.size >= dbgCtx.getIterativeMax() + 1,
          if (ctx.stack.size >= dbgCtx.getIterativeMax() + 1)
            Some(nthPeek(ctx, dbgCtx.getIterativeMax()))
          else None
        )
      )
      dbgCtx.pop() // Pop the current accumulator
      if (ctx.good) dbgCtx.push(ctx.input, Accumulator) // Add a new accumulator only if we continue.
    }

    // Fail if the current context is not good, as required by how Parsley's machine functions.
    if (ctx.good) {
      ctx.handlers = ctx.handlers.tail
      ctx.inc()
    } else {
      ctx.fail()
    }
  }

  override def toString: String = "AddAttemptAndLeave"
}