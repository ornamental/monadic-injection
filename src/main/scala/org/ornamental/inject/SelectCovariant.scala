package org.ornamental.inject

import shapeless.::
import shapeless.HList

/** Type family selecting one element of an `HList` by type or subtype.
  *
  * @tparam L the `HList` type to select an element from
  * @tparam T the type to select an element by; subtypes are eligible candidates
  */
trait SelectCovariant[L <: HList, +T] {

  def apply(list: L): T
}

object SelectCovariant {

  def apply[L <: HList, U](f: L => U): SelectCovariant[L, U] = f(_)

  implicit def select[H, T <: HList]: SelectCovariant[H :: T, H] =
    SelectCovariant[H :: T, H](_.head)

  implicit def recurse[H, L <: HList, U](implicit
      st: SelectCovariant[L, U]
  ): SelectCovariant[H :: L, U] =
    SelectCovariant[H :: L, U](list => st(list.tail))
}
