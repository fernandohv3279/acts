enum Tree:
  case Leaf(value: Int)
  case Branch(left: Tree, right: Tree)
  
  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + l.size + r.size
