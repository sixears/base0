{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | A Prelude replacement, collecting all my favourite imports from
     the base system and external libraries; not including testing base. -}

module Base0
  ( module Prelude

    ------------------------------------
    --              base              --
    ------------------------------------
  , module Control.Applicative
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Either
  , module Data.Eq
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Identity
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Ord
  , module Data.Semigroup
  , module Data.String
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Typeable
  , module Data.Word
  , module GHC.Exts
  , module GHC.Stack
  , module System.Exit
  , module System.IO
  , module Text.Show

    ------------------------------------
    --      base-unicode-symbols      --
    ------------------------------------

  , module Data.Bool.Unicode
  , module Data.Eq.Unicode
  , module Data.Function.Unicode
  , module Data.List.Unicode
  , module Data.Ord.Unicode
  , module Data.Monoid.Unicode
  , module Numeric.Natural.Unicode
  , module Prelude.Unicode

    ------------------------------------
    --          data-default          --
    ------------------------------------
  , module Data.Default

    ------------------------------------
    --          data-textual          --
    ------------------------------------
  , module Data.Textual

    ------------------------------------
    --            hashable            --
    ------------------------------------
  , module Data.Hashable

    ------------------------------------
    --              lens              --
    ------------------------------------

  , module Control.Lens.Lens
  , module Control.Lens.Prism
  , module Control.Lens.Review

    ------------------------------------
    --              mtl               --
    ------------------------------------

  , module Control.Monad.Except

    ------------------------------------
    --              safe              --
    ------------------------------------
  , head, init, last, tail
  )
where

import Prelude  ( (+), (-), abs, fromInteger, fromIntegral, toInteger )

-- base --------------------------------

import Control.Applicative     ( Applicative
                               , (<*>), (*>), (<*), many, pure, some )
import Control.Exception       ( Exception )
import Control.Monad           ( Monad, (>>), (>>=), foldM, forM, forM_, mapM
                               , mapM_, join, return, sequence, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first, second )
import Data.Bool               ( Bool, (&&), (||), not, otherwise )
import Data.Either             ( Either( Left, Right ), either )
import Data.Eq                 ( Eq( (==), (/=) ) )
import Data.Foldable           ( foldl, foldl', foldl1, foldlM
                               , foldr, foldr', foldr1, foldrM, toList )
import Data.Function           ( (.), ($), (&), const, id )
import Data.Functor            ( (<$>), fmap )
import Data.Functor.Identity   ( Identity )
import Data.List.NonEmpty      ( NonEmpty( (:|) ) )
import Data.Maybe              ( Maybe( Just, Nothing ), maybe )
import Data.Ord                ( Ord, (>), (<), (>=), (<=), max, min )
import Data.Semigroup          ( Semigroup( (<>) ) )
import Data.String             ( String )
import Data.Traversable        ( Traversable, traverse )
import Data.Tuple              ( fst, snd )
import Data.Typeable           ( Typeable )
import Data.Word               ( Word8, Word16, Word32, Word64 )
import GHC.Exts                ( IsList( Item, fromList, fromListN ) )
import GHC.Stack               ( CallStack, HasCallStack, callStack )
import System.Exit             ( ExitCode(..) )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode        ( (∧), (∨) )
import Data.Eq.Unicode          ( (≡), (≢) )
import Data.Function.Unicode    ( (∘) )
import Data.List.Unicode        ( (∈), (∉) )
import Data.Monoid.Unicode      ( (⊕) )
import Data.Ord.Unicode         ( (≤), (≥) )
import Numeric.Natural.Unicode  ( ℕ )
import Prelude.Unicode          ( ℤ, (÷) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#), review )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- safe --------------------------------

import qualified Safe

--------------------------------------------------------------------------------

{- | Safe version of `Data.List.head`, returning a `Maybe`. -}
head ∷ ∀ α . [α] → Maybe α
head = Safe.headMay

{- | Safe version of `Data.List.last`, returning a `Maybe`. -}
last ∷ ∀ α . [α] → Maybe α
last = Safe.lastMay

{- | Safe version of `Data.List.init`, returning a `Maybe`. -}
init ∷ ∀ α . [α] → Maybe [α]
init = Safe.initMay

{- | Safe version of `Data.List.tail`, returning a `Maybe`. -}
tail ∷ ∀ α . [α] → Maybe [α]
tail = Safe.tailMay

-- that's all, folks! ----------------------------------------------------------
