# Pine
Pine is a small, dependently typed language. It is meant to be used as the basis for an interactive
proof assistant.

# Usage
```
Top Level Commands:
:quit                         Quits
:help                         Prints this message
:context                      Prints the Current Set Of Parameters and Definitions
<var> :: <expr>                Declares a variable <var> to be of type <expr>
<var> := <expr>               Defines a variable <var> to be <expr>
:type <expr>                 Checks the type of an expression
:eval <expr>                  Evaluates an <expr>
```

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
