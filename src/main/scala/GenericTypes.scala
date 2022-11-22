import scala.annotation.tailrec
object GenericTypes extends App {
  sealed trait MyList[T] {

    override def toString(): String = {
      s"[${this._repr}]"
    }

    def _repr: String = {
      this match {
        case Pair(head, tail) if tail == End() => s"$head"
        case Pair(head, tail) => s"$head, ${tail._repr}"
        case End() => ""
      }
    }

    def length: Int = {
      this match {
        case End() => 0
        case Pair(head, tail) => 1 + tail.length
      }
    }
    def contains[T](item: T): Boolean = {
      this match {
        case End() => false
        case Pair(head, tail) => {
          head == item match {
            case true => true
            case false => tail.contains(item)
          }
        }
      }
    }

    def find(pred: T => Boolean): Option[T] = {
      this match {
        case End() => None
        case Pair(head, tail) => {
          pred(head) match {
            case true => Some(head)
            case false => tail.find(pred)
          }
        }
      }
    }

    def map[B](f: T => B): MyList[B] = {
      this match {
        case End() => End()
        case Pair(head, tail) => Pair(f(head), tail.map(f))
      }
    }

    def append(t: MyList[T]): MyList[T] = {
      this match {
        case End() => t
        case Pair(head, tail) => Pair(head, tail.append(t))
      }
    }

    def flatMap[B](f: T => MyList[B]): MyList[B] = {
      this match {
        case End() => End()
        case Pair(head, tail) => f(head).append(tail.flatMap(f))
      }
    }

    def push(item: T): MyList[T] = {
      this match {
        case Pair(head, tail) => Pair(head, tail.push(item))
        case End() => Pair(item, End())
      }
    }

    def forEach(f: T => Unit): Unit = {
      this match {
        case Pair(head, tail) => {
          f(head)
          tail.forEach(f)
        }
        case End() => ()
      }
    }

    def fold(start: T, f: (T, T) => T): T = {
      this match {
        case Pair(head, tail) => {
          val accum = f(start, head)
          tail.fold(accum, f)
        }
        case End() => start
      }
    }
  }

  def toCurry[A, B, C](f: ((A, B) => C)): (A => B => C) = { x => y =>
    f(x, y)
  }

  def fromCurried[A, B, C](f: (A => B => C)): (A, B) => C = { (x, y) =>
    f(x)(y)
  }

  // f: x => [x, x*2]
  // [1, 2, 3].map(f) =>=> [[1, 2], [2, 4], [3, 6]]
  // [1, 2, 3].flatMap(x) =>=> [1, 2, 2, 4, 3, 6]

  final case class Pair[T](head: T, tail: MyList[T]) extends MyList[T]
  final case class End[T]() extends MyList[T]
}
