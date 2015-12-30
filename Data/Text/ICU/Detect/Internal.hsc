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
      getLanguage,
      detect
     ) where

import Data.Int (Int32)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Data.IORef (IORef)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Foreign.C.String (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

data CharsetDetector a = CD {
  cdBytestring :: IORef ByteString,
  cdEncoding :: IORef ByteString,
  cdMatch :: IORef CharsetMatch,
  cdStatus :: Int32 -> a,
  cdDetector :: ForeignPtr UCharsetDetector
}

data UCharsetDetector

data CharsetMatch = CM { cmMatch :: !(ForeignPtr UCharsetMatch) }
  deriving (Eq, Typeable)

data UCharsetMatch

instance Show CharsetMatch where
  show c = "CharsetMatch " ++ show (unsafePerformIO $ getName c)


detect :: CharsetDetector a -> IO String
detect CD{..} =
  withForeignPtr cdDetector $ \p -> do
    match <- handleError (ucsdet_detect p)
    peekCString =<< handleError (ucsdet_getName match)

getName :: CharsetMatch -> IO String
getName CM{..} =
  withForeignPtr cmMatch $ \p ->
                            peekCString =<< handleError (ucsdet_getName p)

getConfidence :: CharsetMatch -> IO Int32
getConfidence CM{..} =
  withForeignPtr cmMatch $ \p ->
                            handleError (ucsdet_getConfidence p)

getLanguage :: CharsetMatch -> IO String
getLanguage CM{..} =
  withForeignPtr cmMatch $ \p ->
                            peekCString =<< handleError (ucsdet_getLanguage p)


foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getName" ucsdet_getName :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getLanguage" ucsdet_getLanguage :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getConfidence" ucsdet_getConfidence :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_detect" ucsdet_detect :: Ptr UCharsetDetector -> Ptr UErrorCode -> IO (Ptr UCharsetMatch)
