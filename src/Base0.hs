{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | A Prelude replacement, collecting all my favourite imports from
     the base system and external libraries; including testing base. -}

module Base0
  ( -- Prelude
    (+), (-), fromIntegral

    ------------------------------------
    --              base              --
    ------------------------------------

    -- Control.Applicative
  , (<*>), (*>), (<*), many, pure, some
    -- Control.Exception
  , Exception
    -- Control.Monad
  , Monad, (>>), (>>=)
  , foldM, forM, forM_, mapM, mapM_, join, return, sequence, when
    -- Control.Monad.IO.Class
  , MonadIO, liftIO
    -- Data.Bifunctor
  , first, second
    -- Data.Either
  , Either( Left, Right ), either
    -- Data.Eq
  , Eq( (==), (/=) )
    -- Data.Function
  , ($), (&), const, id
    -- Data.Functor
  , (<$>)
    -- Data.Functor.Identity
  , Identity
    -- Data.List.NonEmpty
  , NonEmpty( (:|) )
    -- Data.Maybe
  , Maybe( Just, Nothing ), maybe
    -- Data.Tuple
  , fst, snd
    -- Data.Word
  , Word8
    -- GHC.Stack
  , CallStack, HasCallStack, callStack
    -- System.IO
  , IO
    -- Text.Show
  , Show( show )

    ------------------------------------
    --      base-unicode-symbols      --
    ------------------------------------

    -- Data.Bool.Unicode
  , (∧)
    -- Data.Eq.Unicode
  , (≡), (≢)
    -- Data.Function.Unicode
  , (∘)
    -- Data.Monoid.Unicode
  , (⊕)
    -- Numeric.Natural.Unicode
  , ℕ
    -- Prelude.Unicode
  , ℤ

    ------------------------------------
    --          data-textual          --
    ------------------------------------
    -- Data.Textual
  , Printable( print ), toString, toText

    ------------------------------------
    --              lens              --
    ------------------------------------

    -- Control.Lens.Lens
  , Lens', lens
    -- Control.Lens.Prism
  , Prism', prism'
    -- Control.Lens.Review
  , (#), review

    ------------------------------------
    --              mtl               --
    ------------------------------------

    -- Control.Monad.Except
  , ExceptT, MonadError, throwError

    ------------------------------------
    --              safe              --
    ------------------------------------
  , head, init, last, tail
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

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#), review )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- safe --------------------------------

import qualified Safe

--------------------------------------------------------------------------------

head ∷ ∀ α . [α] → Maybe α
head = Safe.headMay

last ∷ ∀ α . [α] → Maybe α
last = Safe.lastMay

init ∷ ∀ α . [α] → Maybe [α]
init = Safe.initMay

tail ∷ ∀ α . [α] → Maybe [α]
tail = Safe.tailMay

-- that's all, folks! ----------------------------------------------------------
