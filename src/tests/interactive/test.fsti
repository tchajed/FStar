module Test

val pred: nat -> Type0
val x: nat

val f: x:nat{ pred x } -> y:int{ x <> y } -> unit
