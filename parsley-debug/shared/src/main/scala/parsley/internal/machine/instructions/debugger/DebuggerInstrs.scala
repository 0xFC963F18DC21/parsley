/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.debugger

import parsley.debugger.ParseAttempt
import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.{Instr, InstrWithLabel}

// Instructions used by the debugger itself.
private [internal] sealed trait DebuggerInstr extends Instr

// Enter into the scope of a parser in the current context.
private [internal] class EnterParser
    (var label: Int, origin: LazyParsley[_], optName: Option[String])
    (dbgCtx: DebugContext) extends InstrWithLabel with DebuggerInstr {
    override def apply(ctx: Context): Unit = {
        // I think we can get away with executing this unconditionally.
        dbgCtx.push(ctx.input, origin, optName)
        ctx.pushCheck() // Save our location for inputs.
        ctx.pushHandler(label) // Mark the AddAttempt instruction as an exit handler.
        ctx.inc()
    }

    override def toString: String = s"EnterParser(exit: $label)"
}

// Add a parse attempt to the current context at the current callstack point, and leave the current
// parser's scope.
private [internal] class AddAttemptAndLeave(dbgCtx: DebugContext) extends DebuggerInstr {
    override def apply(ctx: Context): Unit = {
        // These offsets will be needed to slice the specific part of the input that the parser has
        // attempted to parse during its attempt.
        val prevCheck = ctx.checkStack.offset
        val currentOff = ctx.offset

        // Slice based on current offset to see what a parser has attempted to parse,
        // and the 'good' member should indicate whether the previous parser has succeeded or not.
        // We add 1 to currentOff to see what character caused the parse failure.
        val success = ctx.good
        val input = ctx.input.slice(prevCheck, if (success) currentOff else currentOff + 1)

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
                if (success) currentOff else currentOff + 1,
                prevPos,
                if (success) (ctx.line, ctx.col - 1) else (ctx.line, ctx.col),
                success,
                if (success) Some(ctx.stack.peek.asInstanceOf[Any]) else None
            )
        )

        // See above.
        dbgCtx.pop()
        ctx.checkStack = ctx.checkStack.tail // Manually pop off our debug checkpoint.

        // Fail if the current context is not good, as required by how Parsley's machine functions.
        if (success) {
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        } else {
            ctx.fail()
        }
    }

    override def toString: String = "AddAttemptAndLeave"
}