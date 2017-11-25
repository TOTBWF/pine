module Context where

import Syntax

type Type = Value

type Context = Map.Map Variable Type

empty :: Context
empty = Map.empty

extend :: Variable -> Type -> Context -> Context
extend = Map.insert
