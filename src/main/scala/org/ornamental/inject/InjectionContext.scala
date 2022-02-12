package org.ornamental.inject

import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.apply._
import cats.syntax.flatMap._
import shapeless.::
import shapeless.Generic
import shapeless.HList
import shapeless.HNil
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Prepend

/**
 * Represents an accumulator of differently-typed values with capabilities of
 *
 *   1. adding values to the context as a result of pure computation, effecful computation, and
 *      resource allocation, using the values already present in the context if needed;
 *   1. running an effectful computation using the values present in the context, as the
 *      terminal action on the context object.
 */
final class InjectionContext[F[_]: MonadCancelThrow, T <: HList] private (ctx: Resource[F, T]) {

  /**
   * Uses the elements accumulated in the injection context to supply parameters for an action.
   *
   * @param f
   *   the action using the contents of the injection context
   */
  def use[M, I <: HList, R](
      f: M
  )(implicit fnHlist: FnToProduct.Aux[M, I => F[R]], selector: SelectAllCovariant[T, I]): F[R] =
    ctx.map(selector.apply).use(fnHlist(f))

  /**
   * Adds a single value to the injection context.
   *
   * @param a
   *   the value to add to the injection context
   */
  def provide[A](a: A): InjectionContext[F, A :: T] =
    InjectionContext(ctx.map(t => a :: t))

  /**
   * Adds a single value resulting from the specified action to the injection context.
   *
   * @param fa
   *   the action providing the value to add to the injection context
   */
  def provideF[A](fa: F[A]): InjectionContext[F, A :: T] =
    provideR(Resource.eval(fa))

  /**
   * Adds a single resource value to the injection context.
   *
   * @param fa
   *   the resource to add to the injection context
   */
  def provideR[A](fa: Resource[F, A]): InjectionContext[F, A :: T] =
    InjectionContext((fa, ctx).mapN(_ :: _))

  /**
   * Deconstructs an instance of a case class to fields and adds each of them to the injection
   * context, individually.
   *
   * @param a
   *   the object whose fields are to be added to the injection context
   */
  def provideFields[A, B <: HList, R <: HList](a: A)(
      implicit toHlist: Generic.Aux[A, B],
      prepend: Prepend.Aux[B, T, R]): InjectionContext[F, R] =
    InjectionContext(ctx.map(t => prepend(toHlist.to(a), t)))

  /**
   * Deconstructs an instance of a case class (provided by the given action) to fields and adds
   * each of them to the injection context, individually.
   *
   * @param fa
   *   the action providing the object whose fields are to be added to the injection context
   */
  def provideFieldsF[A, B <: HList, R <: HList](fa: F[A])(
      implicit toHlist: Generic.Aux[A, B],
      prepend: Prepend.Aux[B, T, R]): InjectionContext[F, R] =
    InjectionContext(ctx >>= (t => Resource.eval(fa).map(a => prepend(toHlist.to(a), t))))

  /**
   * Adds a value constructed from the values already in the injection context, to the context.
   *
   * @param f
   *   the constructor of the value to be added to the injection context
   */
  def add[M, I <: HList, A](f: M)(
      implicit fnHlist: FnToProduct.Aux[M, I => A],
      selector: SelectAllCovariant[T, I]): InjectionContext[F, A :: T] =
    InjectionContext(ctx.map(h => fnHlist(f)(selector(h)) :: h))

  /**
   * Adds a value resulting from an effectful computation over the values already in the
   * injection context, to the context.
   *
   * @param f
   *   the effectful constructor of the value to be added to the injection context
   */
  def addF[M, I <: HList, A](f: M)(
      implicit fnHlist: FnToProduct.Aux[M, I => F[A]],
      selector: SelectAllCovariant[T, I]): InjectionContext[F, A :: T] =
    InjectionContext(ctx >>= (t => Resource.eval(fnHlist(f)(selector(t))).map(_ :: t)))

  /**
   * Adds a resource constructed from the values already in the injection context, to the
   * context.
   *
   * @param f
   *   the constructor of the resource to add to the injection context
   */
  def addR[M, I <: HList, A](f: M)(
      implicit fnHlist: FnToProduct.Aux[M, I => Resource[F, A]],
      selector: SelectAllCovariant[T, I]): InjectionContext[F, A :: T] =
    InjectionContext(ctx >>= (c => fnHlist(f)(selector(c)).map(_ :: c)))
}

object InjectionContext {

  private def apply[F[_]: MonadCancelThrow, T <: HList](
      ctx: Resource[F, T]
  ): InjectionContext[F, T] =
    new InjectionContext[F, T](ctx)

  /**
   * Creates a new empty injection context.
   *
   * @return
   *   the empty injection context
   */
  def apply[F[_]: MonadCancelThrow](): InjectionContext[F, HNil] =
    InjectionContext[F, HNil](Resource.pure[F, HNil](HNil))
}
