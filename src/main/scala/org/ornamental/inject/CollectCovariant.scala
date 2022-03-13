package org.ornamental.inject

import shapeless.HList
import shapeless.HNil
import shapeless.::

/**
 * Filters out all the elments of an `HList` which are not of the specified type
 *
 * @tparam L
 *   the type of `HList` to be filtered
 * @tparam C
 *   the type whose instances are to be retained
 */
trait CollectCovariant[L <: HList, +C] {

  def apply(list: L): List[C]
}

trait LowPriorityCollectCovariant {

  implicit def recurseMismatch[H, T <: HList, C](
      implicit cc: CollectCovariant[T, C]
  ): CollectCovariant[H :: T, C] = list => cc(list.tail)
}

object CollectCovariant extends LowPriorityCollectCovariant {

  implicit def forNil[C]: CollectCovariant[HNil, C] = _ => Nil

  implicit def recurseMatch[H <: C, T <: HList, C](
      implicit cc: CollectCovariant[T, C]
  ): CollectCovariant[H :: T, C] = list => list.head :: cc(list.tail)
}
