sealed trait Maybe[+A] {
  def isDefined: Boolean = {
    this match {
      case Just(value) => true
      case Empty() => false
    }
  }

  def isEmpty: Boolean = {
    !this.isDefined
  }

  // def contains(elem: A): Boolean = {
  //     this match {
  //         case Just(value) => value == elem
  //         case Empty() => false
  //     }
  // }

  def filter(pred: (A) => Boolean): Maybe[A] = {
    this match {
      case Just(value) => if (pred(value)) Just(value) else Empty()
      case Empty() => Empty()
    }
  }

  def map[B](f: (A) => B): Maybe[B] = {
    this match {
      case Just(value) => Just[B](f(value))
      case Empty() => Empty[B]()
    }
  }

  def flatMap[B](f: (A) => Maybe[B]): Maybe[B] = {
    this match {
      case Just(value) => f(value)
      case Empty() => Empty[B]()
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Just(value) => value
      case Empty() => default
    }
  }

  // def orElse(alternative: => Maybe[A]): Maybe[A] = {
  //     this match {
  //         case Just(value) => Just(value)
  //         case Empty() => alternative
  //     }
  // }
}

final case class Just[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

// We're going to implement a version of `Option` in the standard library.

// An `Maybe` of `A` is:

// - `Just` a value of type `A`; or
// - `Empty` indicating we have no value available.

// Implement the above, and then implement the methods:

// - `contains`
// - `map`
// - `filter`
// - `flatMap`
// - `getOrElse`
// - `orElse`

// Consult the standard library documentation for the method semantics.
