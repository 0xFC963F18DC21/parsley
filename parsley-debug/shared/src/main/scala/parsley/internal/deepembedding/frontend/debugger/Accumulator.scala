/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LetFinderState, LetMap, RecMap}

//noinspection ScalaStyle
private [parsley] object Accumulator extends LazyParsley[Nothing] {
  // A dud parser that does absolutely nothing. It is used to expose accumulator values in iterative
  // parsers by creating fake nodes in the parse tree.
  override protected def findLetsAux[Cont[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] =
    throw new NotImplementedError("You really should not have evaluated this.")

  override protected def preprocess[Cont[_, +_] : ContOps, R, A_ >: Nothing](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
    throw new NotImplementedError("You really should not have evaluated this.")
}
