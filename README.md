# data-push

Push-throughs, pull-throughs and end functors for a variety of data types

## Push

Type classes and a number of instances for [`GHC.Generics`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html)
constructors are provided for:

- `Part`
- `Dart`
- `Push`
  + `push = dart . part`
- `Draw`
- `Bond`
- `Pull`
  + `pull = bond . draw`


### Push2

A type class and some comments provide a sketch of `Push` where
`g` is a `Bifunctor`.


## Cofree2

A "cofreely alternating" data type `Cofree2` is provided:

```haskell
data Cofree2 f a b where
  (:<<) :: b -> f (Cofree2 f b a) -> Cofree2 f a b
```

This can be used to encode the alternation resulting from a `push` through a sum, e.g. `Either`.


## End data types

Binary trees `BTree`
- `EndBTree`

Non-empty binary trees `BTree1`
- `EndBTree1`

Lists `[]`
- `EndList`

Non-empty lists
- `NonEmptyEnd`

Trees `Tree = Node a (Forest a)`
- `EndTree`

Forests `Forest a = [Tree a]`
- `EndForest`

```haskell
class HasEnd f where
  end :: f e a -> e
```

