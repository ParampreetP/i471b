import Data.List
import Debug.Trace

-- Problem 1
-- Return pair containing roots of quadratic equation a*x**2 + b*x + c.
-- The first element in the returned pair should use the positive 
-- square-root of the discriminant, the second element should use the 
-- negative square-root of the discriminant.  Need not handle complex
-- roots.
quadraticRoots :: Floating t => t -> t -> t -> (t, t)
quadraticRoots a b c =
  let discr = sqrt $ b * b - 4 * a * c
  in ( (- b + discr) / (2 * a), (- b - discr) / (2 * a) )

-- Problem 2
-- Return infinite list containing [z, f(z), f(f(z)), f(f(f(z))), ...]
-- May use recursion.
iterateFunction :: (a -> a) -> a -> [a]
iterateFunction f z = z : iterateFunction f (f z)


-- Problem 3
-- Using iterateFunction return infinite list containing 
-- multiples of n by all the non-negative integers.
-- May NOT use recursion.
multiples n = [ y * n| y <- iterateFunction (\x->x+1) 0]

-- Problem 4
-- Use iterateFunction to return an infinite list containing list 
-- of hailstone numbers starting with n.
-- Specifically, if i is a hailstone number, and i is even, then
-- the next hailstone number is i divided by 2; if i is a hailstone
-- number and i is odd, then the next hailstone number is 3*i + 1.
-- May NOT use recursion.
-- See <https://en.wikipedia.org/wiki/Collatz_conjecture>
hailstones :: Integral a => a -> [a]
hailstones n =
  let f =  \i -> if i `mod` 2 == 0 then (i `div` 2) else 3 * i + 1
  in iterateFunction f n

-- Problem 5
-- Return length of hailstone sequence starting with n terminating
-- at the first 1.
-- May NOT use recursion.  Can use elemIndex from Data.List
hailstonesLen :: Integral a => a -> Int
hailstonesLen n =
  case elemIndex 1 (hailstones n) of
    (Just n) -> n + 1
    Nothing -> -1

-- Problem 6
-- Given a string s and char c, return list of indexes in s where c
-- occurs
occurrences s c =
  map snd $ filter (\p->fst p == c) (s `zip` [0..(length s)])

-- A tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)

-- Problem 7
-- Fold tree to a single value.  If tree is a Tree node, then it's
-- folded value is the result of applying ternary treeFn to the result
-- of folding the left sub-tree, the value stored in the Tree node and
-- the result of folding the right sub-tree; if it is a Leaf node,
-- then the result of the fold is the result of applying the unary
-- leafFn to the value stored within the Leaf node.
-- May use recursion.
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1
foldTree treeFn leafFn tree =
  undefined
  
-- Problem 8
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per 
-- an in-order traversal of the tree. Must be implemented using foldTree.
-- May NOT use recursion.
flattenTree :: Tree a -> [a]
flattenTree tree =
  undefined


-- Problem 9
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Must be implemented using flattenTree.
-- May NOT use recursion.
catenateTreeLists :: Tree [a] -> [a]
catenateTreeLists tree =
  undefined
