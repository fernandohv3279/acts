enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])
  
  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + l.size + r.size

  // Solution to 3.27
  def map[B](f: A => B): Tree[B] = this match
    case Leaf(i) => Leaf(f(i))
    case Branch(l,r) => Branch(l.map(f),r.map(f))

import Tree.*
// val t = Branch(Branch(Leaf("a"),Leaf("b")),Branch(Leaf("c"),Leaf("hola")))
val t = Branch(Branch(Leaf(-1),Leaf(2)),Branch(Leaf(-3),Leaf(4)))

def firstPositive(t: Tree[Int]): Int = t match
  case Leaf(i) => i
  case Branch(l,r) =>
    val lpos = firstPositive(l)
    if lpos > 0 then lpos else firstPositive(r)

extension (t: Tree[Int]) def firstPositiveExt: Int = t match
  case Leaf(i) => i
  case Branch(l,r) =>
    val lpos = l.firstPositiveExt
    if lpos > 0 then lpos else r.firstPositiveExt

def main(args: Array[String]) = {
    println(t.size)
    println(firstPositive(t))
    println(firstPositiveExt(t))
    println(maximum(t))
    def f(i: Int): Int = i+1
    println(t.map[Int](f))
}

// Exercise 3.25
// Write a function, maximum, that returns the element in a Tree[Int].
// For the max of two integers x and y we can do x.max(y)

// Solution
def maximum(t: Tree[Int]): Int = t match
  case Leaf(i) => i
  case Branch(l,r) => maximum(l).max(maximum(r))

// Exercise 3.27
// Write a function, map, analogous to the method of the same name on
// List that modifies each element in a tree with a given function
// (See exercise 3.18)
