package com.wheaties.choice.iteration

import com.wheaties.logical.{Connective, Not, Negation, PredicateLike}

/**
 * The whole scheme of a predicate composeable object that avoids high GC overhead while at the same time is not
 * mutable is something that I've struggled to represent. To grab the highest level of efficiency I need to use a
 * mutable accumulator of previous state. I don't like putting in mutable state unless it's within a self-contained
 * function and can never "leak" out. I'm going to violate this in the design of Choices.
 *
 * TODO: Restrict these to be only accessible to the "choice" package.
 * TODO: add in counting as a mutable field
 * TODO: redefine the Connectives so each accept is called, as expensive as that is
 */

//TODO: want first(n) satisfying(pred) to be "counted" up only when satisfying that condition, no?
trait IterationScheme extends PredicateLike[IterationScheme]{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int): Boolean

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int): IterationScheme = this

  protected[iteration] implicit def neg = new Negation[IterationScheme]{
    def not(that: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = !(that accept (value, count))

      override def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val thatN = that next (value, count)

        if(thatN == that) this else Not(thatN)
      }
    }
  }

  protected[iteration] implicit def con = new Connective[IterationScheme,IterationScheme,IterationScheme]{
    def and(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        (p accept (value, count)) && (q accept (value, count))

      def replace(pNext: IterationScheme, qNext: IterationScheme) = pNext and qNext
    }

    def or(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        (p accept (value, count)) || (q accept (value, count))

      def replace(pNext: IterationScheme, qNext: IterationScheme) = pNext or qNext
    }

    def xor(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) !(q accept (value, count)) else q accept (value, count)

      def replace(pNext: IterationScheme, qNext: IterationScheme) = pNext xor qNext
    }

    def nand(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) false else !(q accept (value, count))

      def replace(pNext: IterationScheme, qNext: IterationScheme) = pNext nand qNext
    }

    def nor(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) !(q accept (value, count)) else q accept (value, count)

      def replace(pNext: IterationScheme, qNext: IterationScheme) = pNext nor qNext
    }

    def nxor(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) q accept (value, count) else !(q accept (value, count))

      def replace(pNext: IterationScheme, qNext: IterationScheme) = pNext nxor qNext
    }
  }
}

trait ReplacingScheme extends IterationScheme{
  override def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
    if(accept(value, count)) replace else this

  protected[iteration] def replace: IterationScheme
}

abstract class MultiScheme(p: IterationScheme, q: IterationScheme) extends IterationScheme
    with Product2[IterationScheme, IterationScheme]{
  def _1 = p
  def _2 = q

  override def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
    val pNext = p next (value, count)
    val qNext = q next (value, count)

    if(pNext == p && qNext == q) this else replace(pNext, qNext)
  }

  protected[iteration] def replace(pNext: IterationScheme, qNext: IterationScheme): IterationScheme
}