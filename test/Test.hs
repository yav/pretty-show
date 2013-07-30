module Main where

import Text.Show.Pretty (ppShow,reify,parseValue)
import Test.QuickCheck
import Data.Maybe
import Control.Monad

import Control.Applicative

infixr 4 :+:
infixl 3 :$:
infix 2 `Eq`

-- | Testing infix constructors with different fixities
data D
    = X
    | D `Eq` D
    | D :+: D
    | D :$: D
  deriving (Eq,Show,Read)

infix :|:

-- | Testing interopability between the different value constructs
data D2
    = K D2
    | D2 :|: D2
    | Float Floaty
    | Char Char
    | String String
    | Ratio Rational
    | Int Int
    | L [D2]
    | T (D2,D2)
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

arbD :: Int -> Gen D
arbD s = frequency $
    [ (1,return X) ]
    ++
    [ (s,k <$> arbD s' <*> arbD s')
    | k <- [Eq,(:+:),(:$:)]
    ]
 where
   s' = s `div` 2

arbD2 :: Int -> Gen D2
arbD2 s = frequency $
    [ (1,Int <$> arbitrary)
    , (1,Char <$> arbitrary)
    , (1,String <$> arbitrary)
    , (1,Ratio <$> arbitrary)
    , (1,Float <$> arbitrary)
    , (s,K <$> arbD2 s')
    , (s,do
        l <- choose (0,3)
        L <$> replicateM l (arbD2 (s `div` (5-l)))
      )
    ]
    ++
    [ (s,k <$> arbD2 s' <*> arbD2 s')
    | k <- [(:|:),curry T]
    ]
 where
   s' = s `div` 2

instance Arbitrary D where
  arbitrary = sized arbD

  shrink k = case k of
    Eq x y  -> shrinkBin Eq x y
    x :+: y -> shrinkBin (:+:) x y
    x :$: y -> shrinkBin (:$:) x y
    _       -> [X]

instance Arbitrary D2 where
  arbitrary = sized arbD2

  shrink k = case k of
    x :|: y -> shrinkBin (:|:) x y
    T (x,y) -> shrinkBin (curry T) x y
    K x     -> shrinkUn K x
    L xs    -> xs ++ map L (shrink xs)
    _       -> [Int 0]

shrinkUn :: Arbitrary a => (a -> a) -> a -> [a]
shrinkUn k x = x:map k (shrink x)

shrinkBin :: Arbitrary a => (a -> a -> a) -> a -> a -> [a]
shrinkBin k x y = x:y:[ k a b | a <- shrink x, b <- shrink y ]

prop_can_parse :: D -> Bool
prop_can_parse = isJust . reify

prop_read_ppShow :: D -> Bool
prop_read_ppShow x = (read . ppShow) x == x

prop_can_parse2 :: D2 -> Bool
prop_can_parse2 = isJust . reify

prop_read_ppShow2 :: D2 -> Bool
prop_read_ppShow2 x = (read . ppShow) x == x

main :: IO ()
main = do
    quickCheck prop_can_parse
    quickCheckWith stdArgs { maxSize = 25 } prop_read_ppShow
    quickCheck prop_can_parse2
    quickCheck prop_read_ppShow2

