{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MongoDB.QueryBuilder where

import           Data.List   (intercalate)
import           Data.Monoid
import           Data.Set
import           Prelude     hiding (map)
import qualified Prelude

-- | Value in Query Document
type Value      = String
type ArrayValue = [Value]

-- | Pair embedded in Query Document
data Pair = ExactMatch (String, String)        |
            ArrayExactMatch (String, [String]) |
            In        (String, Set String)     |
            Lt        (String, String)
            deriving (Eq, Show, Ord)

-- | Query Document
data Document = Empty              |
                Pair Pair          |
                And (Set Document) |
                Or  (Set Document)
                deriving (Eq, Show, Ord)

-- Instances for exact match targets
class ExactMatchable a where
    exactMatch :: String -> a -> Pair

instance ExactMatchable Value where
    exactMatch k v = ExactMatch (k, v)

instance ExactMatchable ArrayValue where
    exactMatch k vs = ArrayExactMatch (k, vs)

class Escape a where
    escape :: a -> a

instance Escape Value where
    escape xs = "\"" ++ inner xs ++ "\""
      where
        inner [] = []
        inner ('"':xs) = "\\\"" ++ xs
        inner (x  :xs) = x:inner xs

instance Escape ArrayValue where
    escape = Prelude.map escape

-- | Document is a monoid
instance Monoid Document where
    mempty                          = Empty
    Empty      `mappend` Empty      = Empty
    Empty      `mappend` x          = x
    x@(Pair _) `mappend` y@(Pair _) = And $ fromList [x, y]
    x@(Pair _) `mappend` (And ys)   = And $ x `insert` ys
    x@(Pair _) `mappend` y@(Or _)   = And $ fromList  [x, y]
    And xs     `mappend` And ys     = And $ xs `union` ys
    Or xs      `mappend` Or ys      = Or  $ xs `union` ys
    x@(And _)  `mappend` y@(Or _)   = And $ fromList [x, y]
    x          `mappend` y          = y `mappend` x

-- | Render Document to a query
render :: Document -> String
render Empty                           = ""
render (Pair (ExactMatch (k,v)))       = "{" ++ k ++ ": " ++ escape v ++ "}"
render (Pair (ArrayExactMatch (k,vs))) = "{" ++ k ++ ": " ++ "[ " ++ concatPairs (escape vs) ++ " ] }"
render (Pair (Lt (k,v)))               = "{" ++ k ++ ": { $lt: " ++ escape v ++ " } }"
render (Pair (In (k,vs)))              = "{" ++ k ++ ": " ++ "{ $in: [" ++ concatPairs (escape $ toList vs) ++ " ] } }"
render (And xs)                        = "["      ++ concatPairs (toList $ map render xs) ++ "]"
render (Or xs)                         = "{$or: [ " ++ concatPairs (toList $ map render xs) ++ " ] }"

-- |
concatPairs ::  [String] -> String
concatPairs = intercalate ", "

-- | Disjunction of documents
(<||>) :: Document -> Document -> Document
Empty <||> y     = y
x     <||> Empty = x
x     <||> y     = Or $ fromList [x, y]
infixr 7 <||>

-- | Conjunction of documents
(<&&>) :: Document -> Document -> Document
(<&&>) = mappend
infixr 7 <&&>

-- | Builder of exact match document
(==>) :: ExactMatchable a => String -> a -> Document
x ==> y = Pair $ exactMatch x y
infixr 8 ==>

-- | Builder of $in document
in' :: String -> [String] -> Document
x `in'` ys = Pair $ In (x, fromList ys)

-- | Builder of $lt document
lt :: String -> String -> Document
x `lt` y = Pair $ Lt (x, y)
