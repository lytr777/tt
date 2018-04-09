module RationalSetoid
import Rational
import SignedInt
import Setoid
import SignedSetoid
import NatExtension

%default total
%access public export

rationalReflx : Reflx RationalEq
rationalReflx (MkRat a b) = RationalRefl (signedReflx (a $* (S b)))

rationalSymm : Symm RationalEq
rationalSymm (MkRat a1 b1) (MkRat a2 b2) (RationalRefl rr) = RationalRefl (signedSymm (a1 $* (S b2)) (a2 $* (S b1)) rr)

rationalTrans : Trans RationalEq
rationalTrans (MkRat (SInt p1 n1) b1) (MkRat (SInt p2 n2) b2) (MkRat (SInt p3 n3) b3)
      (RationalRefl (SignedRefl sr12)) (RationalRefl (SignedRefl sr23)) = RationalRefl (SignedRefl sr13)
  where
    _1 : (p1 * S b2) * S b3 + (n2 * S b1) * S b3 = (p2 * S b1) * S b3 + (n1 * S b2) * S b3
    _1 = trans (trans l_tr (cong {f = (* S b3)} sr12)) r_tr
        where
          l_tr : (p1 * S b2) * S b3 + (n2 * S b1) * S b3 = ((p1 * S b2) + (n2 * S b1)) * S b3
          l_tr = sym (multDistributesOverPlusLeft (p1 * S b2) (n2 * S b1) (S b3))

          r_tr : ((p2 * S b1) + (n1 * S b2)) * S b3 = (p2 * S b1) * S b3 + (n1 * S b2) * S b3
          r_tr = multDistributesOverPlusLeft (p2 * S b1) (n1 * S b2) (S b3)

    _2 : (p2 * S b3) * S b1 + (n3 * S b2) * S b1 = (p3 * S b2) * S b1 + (n2 * S b3) * S b1
    _2 = trans (trans l_tr (cong {f = (* S b1)} sr23)) r_tr
      where
        l_tr : (p2 * S b3) * S b1 + (n3 * S b2) * S b1 = ((p2 * S b3) + (n3 * S b2)) * S b1
        l_tr = sym (multDistributesOverPlusLeft (p2 * S b3) (n3 * S b2) (S b1))

        r_tr : ((p3 * S b2) + (n2 * S b3)) * S b1 = (p3 * S b2) * S b1 + (n2 * S b3) * S b1
        r_tr = multDistributesOverPlusLeft (p3 * S b2) (n2 * S b3) (S b1)

    _3 : ((p1 * S b2) * S b3 + (n2 * S b1) * S b3) + ((p2 * S b3) * S b1 + (n3 * S b2) * S b1) =
         ((p2 * S b1) * S b3 + (n1 * S b2) * S b3) + ((p3 * S b2) * S b1 + (n2 * S b3) * S b1)
    _3 = rewrite _1 in rewrite _2 in Refl

    _4 : (p1 * S b2) * S b3 + (n3 * S b2) * S b1 = (n1 * S b2) * S b3 + (p3 * S b2) * S b1
    _4 = rewrite plusCommutative (p1 * S b2 * S b3) (n3 * S b2 * S b1) in _47
      where
        _41 : ((p1 * S b2) * S b3 + (n2 * S b1) * S b3) + ((p2 * S b3) * S b1 + (n3 * S b2) * S b1) =
                   ((p2 * S b3) * S b1 + (n3 * S b2) * S b1) + ((p1 * S b2) * S b3 + (n2 * S b3) * S b1)
        _41 =
            rewrite sym $ multAssociative n2 (S b3) (S b1) in
            rewrite sym $ multAssociative n2 (S b1) (S b3) in
            rewrite sym $ multCommutative (S b3) (S b1) in
            rewrite plusCommutative ((p1 * S b2) * S b3 + n2 * (S (b1 + b3 * S b1))) ((p2 * S b3) * S b1 + (n3 * S b2) * S b1) in
            Refl

        _42 : ((p2 * S b3) * S b1 + (n3 * S b2) * S b1) + (p1 * S b2) * S b3 + (n2 * S b3) * S b1 =
               ((p2 * S b1) * S b3 + (n1 * S b2) * S b3) + (p3 * S b2) * S b1 + (n2 * S b3) * S b1
        _42 =
            rewrite sym $ plusAssociative ((p2 * S b3) * S b1 + (n3 * S b2) * S b1) ((p1 * S b2) * S b3) ((n2 * S b3) * S b1) in
            rewrite sym $ plusAssociative ((p2 * S b1) * S b3 + (n1 * S b2) * S b3) ((p3 * S b2) * S b1) ((n2 * S b3) * S b1) in
            rewrite sym _41 in
            _3

        _43 : (p2 * S b3) * S b1 + (n3 * S b2) * S b1 + (p1 * S b2) * S b3 =
               (p2 * S b1) * S b3 + (n1 * S b2) * S b3 + (p3 * S b2) * S b1
        _43 = plusRightCancel _ _ (n2 * S b3 * S b1) _42

        _44 : (p2 * S b3) * S b1 + ((n3 * S b2) * S b1 + (p1 * S b2) * S b3) =
               (p2 * S b1) * S b3 + ((n1 * S b2) * S b3 + (p3 * S b2) * S b1)
        _44 =
            rewrite plusAssociative ((p2 * S b3) * S b1) ((n3 * S b2) * S b1) ((p1 * S b2) * S b3) in
            rewrite plusAssociative ((p2 * S b1) * S b3) ((n1 * S b2) * S b3) ((p3 * S b2) * S b1) in
            _43

        _45 : (p2 * S b1) * S b3 + ((n3 * S b2) * S b1 + (p1 * S b2) * S b3) =
                   (p2 * S b3) * S b1 + ((n3 * S b2) * S b1 + (p1 * S b2) * S b3)
        _45 =
            rewrite sym $ multAssociative p2 (S b1) (S b3) in
            rewrite multCommutative (S b1) (S b3) in
            rewrite multAssociative p2 (S b3) (S b1) in
            Refl

        _46 : (p2 * S b1) * S b3 + ((n3 * S b2) * S b1 + (p1 * S b2) * S b3) =
               (p2 * S b1) * S b3 + ((n1 * S b2) * S b3 + (p3 * S b2) * S b1)
        _46 = rewrite _45 in _44

        _47 : (n3 * S b2) * S b1 + (p1 * S b2) * S b3 =
               (n1 * S b2) * S b3 + (p3 * S b2) * S b1
        _47 = plusLeftCancel (p2 * S b1 * S b3) _ _ _46

    _5 : S b2 * (p1 * S b3 + n3 * S b1) = S b2 * (n1 * S b3 + p3 * S b1)
    _5 =
        rewrite multDistributesOverPlusRight (S b2) (p1 * S b3) (n3 * S b1) in
        rewrite multDistributesOverPlusRight (S b2) (n1 * S b3) (p3 * S b1) in
        rewrite takeY (S b2) p1 (S b3) in
        rewrite takeY (S b2) n1 (S b3) in
        rewrite takeY (S b2) p3 (S b1) in
        rewrite takeY (S b2) n3 (S b1) in
        _4

    sr13 : p1 * S b3 + n3 * S b1 = p3 * S b1 + n1 * S b3
    sr13 =
        rewrite plusCommutative (p3 * S b1) (n1 * S b3) in
        multLeftCancel _ _ _ _5


RationalSetoid : Setoid
RationalSetoid = MkSetoid Rational RationalEq (EqProof RationalEq rationalReflx rationalSymm rationalTrans)
