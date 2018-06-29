{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module FreeExample where

-- data Expr = Val Int | Add Expr Expr

-- eval :: Expr -> Int
-- eval e = case e of
    -- Val i -> i
    -- Add e0 e1 -> eval e0 + eval e1

-- render :: Expr -> String
-- render e = case e of
    -- Val i -> show i
    -- Add e0 e1 -> "(" ++ render e0 ++ " + " ++ render e1 ++ ")"

data Expr f = In (f (Expr f))

data Val e = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add

infixr 7 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Show)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118)))(In (Inl (Val 1219)))))

instance Functor Val where
    fmap f (Val x) = Val x

instance Functor Add where
    fmap f (Add e0 e1) = Add (f e0) (f e1)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e0) = Inl (fmap f e0)
    fmap f (Inr e1) = Inr (fmap f e1)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
    evalAlgebra :: f Int -> Int

instance Eval Val where
    evalAlgebra (Val x) = x
instance Eval Add where
    evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr x) = evalAlgebra x

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

-- val :: Int -> Expr Val
-- val = In . Val

-- infixl 6 .+.
-- (.+.) :: Expr Add -> Expr Add -> Expr Add
-- x .+. y = In (Add x y)

class (Functor sub, Functor sup) => sub ~> sup where
    inj :: sub a -> sup a

instance Functor f => f ~> f where
    inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f ~> (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f ~> g) => f ~> (h :+: g) where
    inj = Inr . inj

inject :: (g ~> f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val ~> f) => Int -> Expr f
val = inject . Val

infixl 6 .+.
(.+.) :: (Add ~> f) => Expr f -> Expr f -> Expr f
x .+. y = inject (Add x y)

data Mul x = Mul x x
instance Functor Mul where
    fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
    evalAlgebra (Mul x y) = x * y

infixl 7 .*.
(.*.) :: (Mul ~> f) => Expr f -> Expr f -> Expr f
x .*. y = inject (Mul x y)

class Render f where
    render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
    render (Val x) = show x

instance Render Add where
    render (Add e e1) = "(" ++ pretty e ++ " + " ++ pretty e1 ++ ")"

instance Render Mul where
    render (Mul e e1) = "(" ++ pretty e ++ " * " ++ pretty e1 ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr x) = render x