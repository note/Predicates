package com.wheaties.typepredicate

/**
 * Companion object for a TypePredicate of type airity 2. Contains the implicit definition of an unsatisfied type
 * boolean for implicit scope resolution.
 */
trait PredicateCompanion2[Cond[_,_] <: Conditional]{
  implicit def pred[Child,Parent]:Cond[Child,Parent]
}

/**
 * Base Predicate of airity 2. Arguments are the type parameters for the predicate, the condition to be satisfied
 * and discovered by implicit scope resolution, and the implicit which is used as an implicit scope switch.
 */
abstract class TypePredicate2[A,B,Cond[_,_] <: Conditional,Implicit[_,_]] extends Conditional{
  implicit def pred[Child,Parent <: B](implicit ev:Implicit[Child,Parent]):Cond[Child,Parent]

  private[wheaties] abstract class Answer(implicit ev: Cond[A,B]) extends Conditional{
    type Result = Cond[A,B]#Result
  }

  type Result = Answer#Result
}

/**
 * Base trait for subtype checking.
 */
sealed trait SubType[A,B] extends Conditional
private abstract class SubTrue[Child,Parent](implicit ev: <:<[Child,Parent]) extends SubType[Child,Parent] with (Child => Parent){
  type Result = True
}
private abstract class SubFalse[Child,Parent] extends SubType[Child,Parent]{
  type Result = False
}

object IsSubtypeOf extends PredicateCompanion2[SubType]{
  implicit def pred[Child,Parent]:SubType[Child,Parent] = new SubFalse[Child,Parent]{}
}

//Force the evaluation of sub-typing by means of implicit scope precedence.
abstract class IsSubtypeOf[A,B] extends TypePredicate2[A,B,SubType,<:<]{
  implicit def pred[Child,Parent <: B](implicit ev: <:<[Child,Parent]):SubType[Child,Parent] =
    new SubTrue[Child,Parent]{ def apply(x: Child):Parent = x }
}