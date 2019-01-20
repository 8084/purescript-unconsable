# purescript-unconsable

[![Build status](https://travis-ci.org/8084/purescript-unconsable.svg?branch=master)](https://travis-ci.org/8084/purescript-unconsable)

Type class for data structures which can be `uncons`ed + some utilities built on top of it.

# Why this library?

[uncons](https://pursuit.purescript.org/search?q=uncons) comes in so many different flavors and is so common that it is obvious that we need a type class for it:

```purescript
class Unconsable t where
  uncons :: forall a. t a -> Maybe { head :: a, tail :: t a }
```

# Applications of the `Unconsable` abstraction

## Comparing sizes of foldable structures

Comparing sizes of foldable structures by computing their lengths using `length :: forall a b f. Foldable f => Semiring b => f a -> b` is not very effective. In fact, it is not required to evaluate *both* lengths to get the `Ordering` value: it is more optimal to traverse both structures simultaneously and return after the end of the shortest one. `Foldable` does not allow to do this, but with `Unconsable` it is possible.

# Using this library

Make sure that your types are instances of `Unconsable` class and refactor your code using the following rules:


| Before                 | After                  |
| ---------------------- |:----------------------:|
| `length a < length b`  | `isShorterThan a b`    |
| `length a > length b`  | `isLongerThan a b`     |
| `length a == length b` | `isOfSameLength a b`   |
| `length a > n`         | `longerThan a n`       |
| `length a < n`         | `shorterThan a n`      |
| `length a == n`        | `hasLength a n`        |

# Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-unconsable).
