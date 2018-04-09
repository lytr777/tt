module SignedSetoid
import SignedInt
import Setoid

%default total
%access public export

signedReflx : Reflx SignedEq
signedReflx (SInt p n) = SignedRefl Refl

signedSymm : Symm SignedEq
signedSymm (SInt p1 n1) (SInt p2 n2) (SignedRefl sr) = SignedRefl (sym sr)

signedTrans : Trans SignedEq
signedTrans (SInt p1 n1) (SInt p2 n2) (SInt p3 n3) (SignedRefl sr12) (SignedRefl sr23) = SignedRefl sr13
  where
    _1 : (p1 + n2) + (p2 + n3) = ((p2 + n1) + p3) + n2
    _1  = trans (rewrite sr12 in rewrite sr23 in Refl) (plusAssociative (p2 + n1) p3 n2)

    _2 : ((p2 + n3) + p1) + n2 = (p1 + n2) + (p2 + n3)
    _2 = trans (sym (plusAssociative (p2 + n3) p1 n2)) (sym (plusCommutative (p1 + n2) (p2 + n3)))

    _3 : (p2 + n3) + p1 = (p2 + n1) + p3
    _3 = plusRightCancel _ _ n2 (trans _2 _1)


    _4 : (n3 + p1) + p2 = (p2 + n3) + p1
    _4 = trans (plusCommutative (n3 + p1) p2) (plusAssociative p2 n3 p1)

    _5 : (p2 + n1) + p3 = (n1 + p3) + p2
    _5 = sym (trans (plusCommutative (n1 + p3) p2) (plusAssociative p2 n1 p3))

    _6 : n3 + p1 = n1 + p3
    _6 = plusRightCancel _ _ p2 (trans (trans _4 _3) _5)

    sr13 : p1 + n3 = p3 + n1
    sr13 = trans (trans (plusCommutative p1 n3) _6) (plusCommutative n1 p3)

SignedSetoid : Setoid
SignedSetoid = MkSetoid SignedInt SignedEq (EqProof SignedEq signedReflx signedSymm signedTrans)
