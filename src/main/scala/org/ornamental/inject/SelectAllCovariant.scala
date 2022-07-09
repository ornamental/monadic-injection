package org.ornamental.inject

import shapeless.::
import shapeless.HList
import shapeless.HNil

/**
 * Type family selecting elements of an `HList` according to the types specified by another
 * `HList` (or their respective subtypes).
 *
 * @tparam L
 *   the `HList` type to select elements from
 * @tparam S
 *   the `HList` type to determine elements of which types need be selected; subtypes are
 *   eligible candidates
 */
trait SelectAllCovariant[L <: HList, S <: HList] {

  def apply(list: L): S
}

object SelectAllCovariant {

  implicit def forNil[L <: HList]: SelectAllCovariant[L, HNil] = _ => HNil

  implicit def forCons[L <: HList, H, T <: HList](
      implicit sHead: SelectCovariant[L, H],
      sTail: SelectAllCovariant[L, T]): SelectAllCovariant[L, H :: T] = (lst: L) =>
    sHead(lst) :: sTail(lst)
}
