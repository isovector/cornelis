module Hello where

data Bool : Set where
  true : Bool
  false : Bool

data Unit : Set where
  one : Unit

unit : Unit
unit = ?

test : Bool → Bool
test x = ?

unicodeTest₁ : Bool → Bool
unicodeTest₁ x = ?

slap : Bool → Bool
slap = λ { x → ? }

