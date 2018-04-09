module Rational
import SignedInt

%default total
%access public export

-- a/(1+b)
record Rational where
    constructor MkRat
    a: SignedInt
    b: Nat


implementation Num Rational where
    (MkRat a1 b1) + (MkRat a2 b2) = MkRat (a1 $* (S b2) + a2 $* (S b1)) (b1 * b2 + b1 + b2)
    (MkRat a1 b1) * (MkRat a2 b2) = MkRat (a1 * a2) (b1 * b2 + b1 + b2)
    fromInteger a = MkRat (fromInteger a) 0

implementation Neg Rational where
    negate (MkRat a b) = MkRat (negate a) b
    a - b = a + negate b

data RationalEq : Rational -> Rational -> Type where
    RationalRefl : (eq : a1 $* (S b2) `SignedEq` a2 $* (S b1)) -> RationalEq (MkRat a1 b1) (MkRat a2 b2)
