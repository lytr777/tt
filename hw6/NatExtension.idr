module NatExtension

%default total
%access public export

zeroYCancel : (x : Nat) -> (y : Nat) -> x + y = 0 -> x = 0
zeroYCancel x Z e = rewrite plusCommutative 0 x in e
zeroYCancel x (S k) e = void $ SIsNotZ (trans (plusCommutative (S k) x) e)

multLeftCancel : (x : Nat) -> (y : Nat) -> (z : Nat) -> S x * y = S x * z -> y = z
multLeftCancel x Z z e = sym (zeroYCancel _ _ (sym (rewrite multCommutative 0 x in e)))
multLeftCancel x y Z e = zeroYCancel _ _ (rewrite multCommutative 0 x in e)
multLeftCancel x (S k) (S j) e = rewrite multLeftCancel x k j _ss3 in Refl
  where
    _ss1 : k + (x + x * k) = j + (x + x * j)
    _ss1 =
        rewrite sym $ multRightSuccPlus x k in
        rewrite sym $ multRightSuccPlus x j in
        succInjective _ _ e

    _ss2 : x + (k + x * k) = x + (j + x * j)
    _ss2 =
        rewrite plusAssociative x k (x * k) in
        rewrite plusAssociative x j (x * j) in
        rewrite plusCommutative x k in
        rewrite plusCommutative x j in
        rewrite sym $ plusAssociative k x (x * k) in
        rewrite sym $ plusAssociative j x (x * j) in
        _ss1

    _ss3 : k + x * k = j + x * j
    _ss3 = plusLeftCancel _ _ _ _ss2

takeY : (x : Nat) -> (y : Nat) -> (z : Nat) -> x * (y * z) = y * x * z
takeY x y z =
        rewrite multAssociative x y z in
        rewrite multCommutative x y in
        Refl
