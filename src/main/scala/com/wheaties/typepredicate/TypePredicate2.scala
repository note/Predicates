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
 * Answers the question of "Is A a subtype of B."
 */
sealed abstract class SubType[A,B] extends Conditional
object IsSubtypeOf extends PredicateCompanion2[SubType]{
  implicit def pred[Child,Parent] = new SubType[Child,Parent]{ type Result = False }
}

abstract class IsSubtypeOf[A,B] extends TypePredicate2[A,B,SubType,<:<]{
  implicit def pred[Child,Parent <: B](implicit ev: <:<[Child,Parent]):SubType[Child,Parent] =
    new SubType[Child,Parent] with (Child => Parent){ def apply(x: Child):Parent = x
      type Result = True
    }
}