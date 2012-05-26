package com.wheaties.typeboolean

sealed trait TypeBoolean
sealed trait True extends TypeBoolean
sealed trait False extends TypeBoolean

trait Conditional{
  type Result <: TypeBoolean
}

sealed trait SubType[A,B] extends Conditional
private abstract class SubTrue[Child,Parent](implicit ev: <:<[Child,Parent]) extends SubType[Child,Parent] with (Child => Parent){
  type Result = True
}
private abstract class SubFalse[Child,Parent] extends SubType[Child,Parent]{
  type Result = False
}

object IsSubtypeOf{
  implicit def sub[Child,Parent]:SubType[Child,Parent] = new SubFalse[Child,Parent]{}
}

//Force the evaluation of sub-typing by means of implicit scope precedence.
abstract class IsSubtypeOf[A,B] extends Conditional{
  implicit def sub[Child,Parent <: B](implicit ev: <:<[Child,Parent]):SubType[Child,Parent] =
    new SubTrue[Child,Parent]{ def apply(x: Child):Parent = x }

  private[wheaties] abstract class Answer(implicit ev: SubType[A,B]) extends Conditional{
    type Result = SubType[A,B]#Result
  }

  type Result = Answer#Result
}