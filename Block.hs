{-
    File : Block.hs
    Copyright : (c) Hangyi Wang (hangyi)
    Md2pdf
-}

module Block (
    Block(..),
    Markdown(..),
    mdAdd
)
where

import Data.Vector as DV
import Data.Functor
import Control.Applicative
import Data.Semigroup
import Data.Monoid

data Block
    -- Text
    = Text          String      
    -- # Heading    
    | Heading       Int Block
    -- *Italic*    
    | Italic        Block  
    -- **Emphasis**      
    | Emphasis      Block 
    -- ```code```     
    | Code          Block     
    -- `InlineCode`     
    | InlineCode    Block    
    -- [description](website)      
    | Hyperlink     Block
    -- - first - second - third   
    | DotList       Block 
    -- >quote
    | Quote         Block     
    deriving (Show, Eq)

newtype Markdown a = Markdown (DV.Vector a) deriving (Show, Eq)

instance Functor Markdown where
    fmap f (Markdown a) = Markdown $ DV.map f a

instance Applicative Markdown where
    pure a = Markdown DV.empty
    Markdown fs <*> Markdown xs = Markdown (DV.zipWith ($) fs xs)

instance Semigroup a => Semigroup (Markdown a) where
    Markdown a <> Markdown b = Markdown (a <> b)

instance Monoid a => Monoid (Markdown a) where
    mempty = Markdown DV.empty
    mappend = (<>)

instance Alternative Markdown where
    empty = Markdown DV.empty
    Markdown a <|> Markdown b = Markdown (a <> b)

{-
    mdAdd: add element into Markdown data type
-}
mdAdd :: a -> Markdown a -> Markdown a
mdAdd elt (Markdown (v)) = Markdown (snoc v elt)
