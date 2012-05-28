package com.wheaties.typepredicate

sealed trait TypeBoolean
sealed trait True extends TypeBoolean
sealed trait False extends TypeBoolean

trait Conditional{
  type Result <: TypeBoolean
}