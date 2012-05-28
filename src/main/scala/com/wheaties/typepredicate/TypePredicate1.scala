package com.wheaties.typepredicate

/**
 * Companion object for a TypePredicate of type airity 1. Contains the implicit definition of an unsatisfied type
 * boolean for implicit scope resolution.
 */
trait PredicateCompanion1[Cond[_] <: Conditional]{
  implicit def pred[B]:Cond[B]
}

/**
 * Base Predicate of airity 1. Arguments are the type parameters for the predicate, the condition to be satisfied
 * and discovered by implicit scope resolution, and the implicit which is used as an implicit scope switch.
 */
abstract class TypePredicate1[A,Cond[_] <: Conditional,Implicit[_]] extends Conditional{
  implicit def pred[B](implicit ev:Implicit[B]):Cond[B]

  private[wheaties] abstract class Answer(implicit ev: Cond[A]) extends Conditional{
    type Result = Cond[A]#Result
  }

  type Result = Answer#Result
}

trait Numerical[A] extends Conditional
private abstract class NumericTrue[A](implicit ev: Numeric[A]) extends Numerical[A]{
  type Result = True
}
private abstract class NumericFalse[A] extends Numerical[A]{
  type Result = False
}

object IsNumeric extends PredicateCompanion1[Numerical]{
  implicit def pred[A]:Numerical[A] = new NumericFalse[A] {}
}

abstract class IsNumeric[A] extends TypePredicate1[A,Numerical,Numeric]{
  implicit def pred[B](implicit ev: Numeric[B]):Numerical[B] = new NumericTrue[B] {}
}