{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, RecordWildCards #-}

module Data.Text.ICU.Detect.Internal
    (

      -- * Unicode charset detection API
      CharsetDetector (..),
      UCharsetDetector,
      CharsetMatch(..),
      UCharsetMatch,
      getConfidence,
      getName,
      getLanguage
     ) where

import Data.Int (Int32)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
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

data CharsetMatch = CM { cmMatch :: !(ForeignPtr UCharsetMatch) }
                  deriving (Eq, Typeable)

data UCharsetMatch

instance Show CharsetMatch where
    show c = "CharsetMatch " ++ show (getName c)

getName :: CharsetMatch -> String
getName CM{..} = unsafePerformIO $
  withForeignPtr cmMatch $ \p ->
                            peekCString =<< handleError (ucsdet_getName p)

getConfidence :: CharsetMatch -> Int32
getConfidence CM{..} = unsafePerformIO $
  withForeignPtr cmMatch $ \p ->
                            handleError (ucsdet_getConfidence p)

getLanguage :: CharsetMatch -> String
getLanguage CM{..} = unsafePerformIO $
  withForeignPtr cmMatch $ \p ->
                            peekCString =<< handleError (ucsdet_getLanguage p)

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getName" ucsdet_getName
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getLanguage" ucsdet_getLanguage
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getConfidence" ucsdet_getConfidence
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO Int32
