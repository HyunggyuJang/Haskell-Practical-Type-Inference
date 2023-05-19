data Term v = Var v | App (Term v) (Term v) | Lam (Term (Incr v))
data Incr v = Zero | Succ v

{- incrInt's spec
>>> incrInt Zero
0
>>> incrInt (Succ 4)
4
-}
incrInt n =
  case n of
    Zero -> 0
    Succ m -> m

type MapT = forall a b. (a -> b) -> Term a -> Term b

fixMT :: (MapT -> MapT) -> MapT
fixMT f = f (fixMT f)

mapT :: MapT
mapT = fixMT (\ mt f t -> case t of
                   Var v -> Var (f v)
                   App t1 t2 -> App (mt f t1) (mt f t2)
                   Lam t -> Lam (mt (mapI f) t))

mapI :: (t -> v) -> Incr t -> Incr v
mapI f n =
  case n of
    Zero -> Zero
    Succ m -> Succ (f m)
