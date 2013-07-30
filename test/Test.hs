module Main where

import Text.Show.Pretty (ppShow,reify,parseValue)
import Test.QuickCheck
import Data.Maybe
import Control.Monad

import Control.Applicative

-- | A data type for testing read . ppShow = id
data D
    = A
    | B D
    | C D D
    | D `I` D
    | D :*: D
    | D :+: D
    | D :=: D
    | D :$: D
    | T (D,D)
    | L [D]
    | Int Int
    | Float Floaty
    | Char Char
    | String String
    | Ratio Rational
  deriving (Eq,Show,Read)

newtype Floaty = Floaty Float
  deriving (Show,Read)

instance Eq Floaty where
  Floaty x == Floaty y
    | isNaN x && isNaN y = True
    | otherwise          = x == y

instance Arbitrary Floaty where
  arbitrary = frequency
    [(10,Floaty <$> arbitrary)
    ,(1,return $ Floaty (0/0))
    ,(1,return $ Floaty (1/0))
    ,(1,return $ Floaty ((-1)/0))
    ]

instance Arbitrary D where
  arbitrary = sized arbD

  shrink k = case k of
      C x y   -> bin C x y
      x `I` y -> bin I x y
      x :*: y -> bin (:*:) x y
      x :+: y -> bin (:+:) x y
      x :=: y -> bin (:=:) x y
      x :$: y -> bin (:$:) x y
      T (x,y) -> bin (curry T) x y
      L xs    -> xs ++ map L (shrink xs)
      B x     -> x:map B (shrink x)
      _       -> [A]
    where
      bin k x y = x:y:[ k a b | a <- shrink x, b <- shrink y ]


arbD :: Int -> Gen D
arbD s = frequency $
    [ (1,return A)
    , (1,Int <$> arbitrary)
    , (1,Char <$> arbitrary)
    , (1,String <$> arbitrary)
    , (1,Ratio <$> arbitrary)
    , (1,Float <$> arbitrary)
    , (s,B <$> arbD (s - 1))
    , (s,do
        l <- choose (0,3)
        L <$> replicateM l (arbD (s `div` (4-l)))
      )
    ]
    ++
    [ (s,k <$> arbD s' <*> arbD s')
    | k <- [C,I,(:*:),(:+:),(:=:),(:$:),curry T]
    ]
 where
   s' = s `div` 2

infixr 5 :*:
infixr 4 :+:
infixl 3 :$:
infix 2 :=:

prop_can_parse :: D -> Bool
prop_can_parse = isJust . reify

prop_read_ppShow :: D -> Bool
prop_read_ppShow x = (read . ppShow) x == x

main :: IO ()
main = do
    quickCheck prop_can_parse
    quickCheck prop_read_ppShow

