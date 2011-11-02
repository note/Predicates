package com.wheaties.application

trait ClosedFunctionFactory1[A,B] extends Function1[A,B]{
  def query(arg0: A):Function1[A,B]
  def memoize(arg0: A):(B,Function1[A,B])

  override def compose[C](func: Function1[C,A]):ClosedFunctionFactory1[C,B]
    = WrappedCompose(this, WrappedFunction(func))
  def compose[C](func: ClosedFunctionFactory1[C,A]) = WrappedCompose(this, func)
  override def andThen[C](func: Function1[B,C]):ClosedFunctionFactory1[A,C]
    = WrappedAndThen(this, WrappedFunction(func))
  def andThen[C](func: ClosedFunctionFactory1[B,C]) = WrappedAndThen(this, func)
}

//TODO: add in compose to these too?
trait UnclosedFunctionFactory1[A,B] extends Function1[A,Option[B]]{
  def query(arg0: A):Option[Function1[A,B]]
  def memoize(arg0: A):Option[(B,Function1[A,B])]
}

case class WrappedFunction[A,B](that: Function1[A,B]) extends ClosedFunctionFactory1[A,B]{
  override def compose[C](func: Function1[C,A]) = copy(that compose func)
  override def andThen[C](func: Function1[B,C]) = copy(that andThen func)

  def query(arg0: A) = that
  def memoize(arg0: A) = (apply(arg0), query(arg0))

  def apply(arg0: A) = that(arg0)
}

case class WrappedCompose[A,B,C](that: ClosedFunctionFactory1[A,B], thatCompose: ClosedFunctionFactory1[C,A])
  extends ClosedFunctionFactory1[C,B]{

  def query(arg0: C) ={
    val (out, outFunc) = thatCompose.memoize(arg0)
    that.query(out) compose outFunc
  }
  def memoize(arg0: C) ={
    val (out, outFunc) = thatCompose.memoize(arg0)
    val (out2, outFunc2) = that.memoize(out)
    (out2, outFunc2 compose outFunc)
  }

  def apply(arg0: C) = that(thatCompose(arg0))
}

case class WrappedAndThen[A,B,C](that: ClosedFunctionFactory1[A,B], thatAndThen: ClosedFunctionFactory1[B,C])
  extends ClosedFunctionFactory1[A,C]{

  def query(arg0: A) ={
    val (out, outFunc) = that.memoize(arg0)
    outFunc andThen thatAndThen.query(out)
  }
  def memoize(arg0: A) ={
    val (out, outFunc) = that.memoize(arg0)
    val (out2, outFunc2) = thatAndThen.memoize(out)
    (out2, outFunc andThen outFunc2)
  }

  def apply(arg0: A) = thatAndThen(that(arg0))
}