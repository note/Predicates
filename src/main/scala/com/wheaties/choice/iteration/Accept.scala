package com.wheaties.choice.iteration

class AcceptAll extends IterationScheme{
  def accept[A](value: A) = true
}

class AcceptEvery(n: Int) extends IterationScheme{
  private var step = -1

  def accept[A](value: A) ={
    step += 1
    if(step == n) step = 0

    step == 0
  }
}

class AcceptFirst(n: Int) extends IterationScheme{
  private var count = -1

  def accept[A](value: A) ={
    count += 1

    count < n
  }
}

//TODO: This works for limiting up to but not over. It doesn't limit less than.
//TODO: throwing the exception here is bad. If composing two Choices with "and" condition, could cause problems.
class AcceptExactly(n: Int) extends IterationScheme{
  private var count = 0

  def accept[A](value: A) ={
    count += 1
    if(n < count) throw new Exception("Exceeded acceptable limit of %s" format n)

    true
  }
}

class AcceptIf[B](pred: B => Boolean) extends IterationScheme{
  def accept[A <: B](value: A) = pred(value)
}
