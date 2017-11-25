# Pine
Pine is a small, dependently typed language. It is meant to be used as the basis for an interactive
proof assistant.

# Samples
Addition of Natural Numbers 
```
λπ> N : Type 0 
λπ> s : N -> N
λπ> z : N
λπ> numeral := forall A : Type 0, (A -> A) -> (A -> A)
λπ> zero := fun (A : Type 0) (f: A -> A) (x: A) => x
λπ> one := fun (A : Type 0) (f: A -> A) (x: A) => f x
λπ> two := fun (A : Type 0) (f : A -> A) (x: A) => f (f x)
λπ> three := fun (A : Type 0) (f : A -> A) (x: A) => f (f (f x))
λπ> plus := fun (m : numeral) (n : numeral) (A : Type 0) (f : A -> A) (x : A) => m A f (n A f x)
λπ> :eval plus two N s z
    = s (s (s (s (s z))))
    : N
```


# Building
```
stack build
stack exec pine
```
