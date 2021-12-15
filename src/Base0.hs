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
  , module Data.Either
  , module Data.Eq
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Identity
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Tuple
  , module Data.Word
  , module GHC.Exts
  , module GHC.Stack
  , module System.IO
  , module Text.Show

    ------------------------------------
    --      base-unicode-symbols      --
    ------------------------------------

  , module Data.Bool.Unicode
  , module Data.Eq.Unicode
  , module Data.Function.Unicode
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

  , module Text
  , module TextIO
  , module P
  )
where

import Prelude  ( (+), (-), fromIntegral )

-- base --------------------------------

import Control.Applicative     ( (<*>), (*>), (<*), many, pure, some )
import Control.Exception       ( Exception )
import Control.Monad           ( Monad, (>>), (>>=), foldM, forM, forM_, mapM
                               , mapM_, join, return, sequence, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first, second )
import Data.Either             ( Either( Left, Right ), either )
import Data.Eq                 ( Eq( (==), (/=) ) )
import Data.Function           ( ($), (&), const, id )
import Data.Functor            ( (<$>) )
import Data.Functor.Identity   ( Identity )
import Data.List.NonEmpty      ( NonEmpty( (:|) ) )
import Data.Maybe              ( Maybe( Just, Nothing ), maybe )
import Data.Tuple              ( fst, snd )
import Data.Word               ( Word8 )
import GHC.Exts                ( IsList( Item, fromList, fromListN, toList ) )
import GHC.Stack               ( CallStack, HasCallStack, callStack )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode        ( (∧) )
import Data.Eq.Unicode          ( (≡), (≢) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )
import Prelude.Unicode          ( ℤ )

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

-- text --------------------------------

import qualified Data.Text     as  Text
import qualified Data.Text.IO  as  TextIO

-- text-printer ------------------------

import qualified  Text.Printer  as  P

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
