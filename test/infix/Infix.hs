module Main where

import Text.Show.Pretty

infixr 3 :->:
infix 2 :::
infixl 1 :$:

data T = TVar String | T :->: T | Int
  deriving (Show,Read,Eq)

data E = String ::: T | E :$: E | Lit Int
  deriving (Show,Read,Eq)

iii :: T
iii = Int :->: (Int :->: Int)

f :: E
f = "f" ::: iii

x :: E
x = "x" ::: Int

e :: E
e = f :$: x :$: Lit 5

es :: [E]
es = [e,(("*" ::: iii) :$: e) :$: e]

main :: IO ()
main = do
    let str = ppShow es
    putStrLn str
    print (read str == es)

