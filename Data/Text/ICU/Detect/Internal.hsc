{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Data.Text.ICU.Detect.Internal
    (

      -- * Unicode charset detection API
      CharsetDetector (..),
      UCharsetDetector,
      CharsetMatch(..),
      UCharsetMatch(..)
     ) where

import Data.Int (Int32)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.IORef (IORef)
import Data.Text (Text)

data CharsetDetector a = CD {
        cdText :: IORef Text
      , cdEncoding :: IORef Text
      , cdMatch :: IORef CharsetMatch
      , cdStatus :: Int32 -> a
      , cdDetector :: ForeignPtr UCharsetDetector
      }

data UCharsetDetector

data CharsetMatch = CM {
      cmMatch :: Ptr UCharsetMatch
  }

data UCharsetMatch = None | UCharsetMatch
