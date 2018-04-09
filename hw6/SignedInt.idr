module SignedInt

%default total
%access public export

record SignedInt where
    constructor SInt
    p : Nat
    n : Nat

implementation Num SignedInt where
    (SInt p1 n1) + (SInt p2 n2) = SInt (p1 + p2) (n1 + n2)
    (SInt p1 n1) * (SInt p2 n2) = SInt (p1 * p2 + n1 * n2) (p1 * n2 + p2 * n1)
    fromInteger i = if (i > 0)
      then SInt (fromIntegerNat i) 0
      else SInt 0 (fromIntegerNat (abs i))

implementation Neg SignedInt where
    negate (SInt p n) = SInt n p
    s1 - s2 = s1 + (negate s2)

infix 9 $*
($*) : SignedInt -> Nat -> SignedInt
(SInt p n) $* k = SInt (p * k) (n * k)

data SignedEq : SignedInt -> SignedInt -> Type where
    SignedRefl : (eq : (p1 + n2) = (p2 + n1)) -> SignedEq (SInt p1 n1) (SInt p2 n2)
