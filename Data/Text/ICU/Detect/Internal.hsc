{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Data.Text.ICU.Detect.Internal
    (

      -- * Unicode charset detection API
      CharsetDetector (..),
      UCharsetDetector
     ) where

import Data.Int (Int32)
import Foreign.ForeignPtr (ForeignPtr)
import Data.IORef (IORef)
import Data.Text (Text)

data CharsetDetector a = CD {
        cdText :: IORef Text
      , cdStatus :: Int32 -> a
      , cdDetector :: ForeignPtr UCharsetDetector
      }

data UCharsetDetector
