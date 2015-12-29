{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Data.Text.ICU.Detect.Internal
    (

      -- * Unicode charset detection API
      CharsetDetector (..),
      UCharsetDetector,
      CharsetMatch(..),
      UCharsetMatch
     ) where

import Data.Int (Int32)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Foreign.C.String (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

data CharsetDetector a = CD {
        cdText :: IORef Text
      , cdEncoding :: IORef Text
      , cdMatch :: IORef CharsetMatch
      , cdStatus :: Int32 -> a
      , cdDetector :: ForeignPtr UCharsetDetector
      }

data UCharsetDetector

data CharsetMatch = CM !(Ptr UCharsetMatch)
                  deriving (Eq, Typeable)

data UCharsetMatch

instance Show CharsetMatch where
    show c = "CharsetMatch " ++ show (getName c)

withMatch :: CharsetMatch -> (Ptr UCharsetMatch -> IO a) -> IO a
{-# INLINE withMatch #-}
withMatch (CM match) action = action match

getName :: CharsetMatch -> String
getName match = unsafePerformIO .
    withMatch match $ \ptr ->
        peekCString =<< handleError (ucsdet_getName ptr)

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getName" ucsdet_getName
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString
