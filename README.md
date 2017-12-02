# Pine
Pine is a small, dependently typed language. It is meant to be used as the basis for an interactive
proof assistant.

# Samples
Addition of Natural Numbers 
```
inductive Nat :: Type 1 := Z :: Nat | S :: Nat -> Nat
natRec :: forall (m :: Nat -> Type 1), m Z -> (forall (l :: Nat), m l -> m (S l)) -> (forall (k :: Nat), m k)
let plus := natRec (fun (n :: Nat) => Nat -> Nat) (fun (n :: Nat) => n) (fun (k :: Nat) (rec :: Nat -> Nat) (n :: Nat) => S (rec n)) 
```


# Building
```
stack build
stack exec pine
```
