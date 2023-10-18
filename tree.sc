enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])
  
  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + l.size + r.size

import Tree.*
// val t = Branch(Branch(Leaf("a"),Leaf("b")),Branch(Leaf("c"),Leaf("hola")))
val t = Branch(Branch(Leaf(-1),Leaf(2)),Branch(Leaf(-3),Leaf(4)))

def firstPositive(t: Tree[Int]): Int = t match
  case Leaf(i) => i
  case Branch(l,r) =>
    val lpos = firstPositive(l)
    if lpos > 0 then lpos else firstPositive(r)

// extension (t: Tree[Int]) def firstPositive:
def main(args: Array[String]) = {
    println(t.size)
    println(firstPositive(t))
}
