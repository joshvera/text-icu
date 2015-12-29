{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Text.ICU.Detect where

import Data.Int (Int32)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Detect.Internal (CharsetDetector(..), UCharsetDetector)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.ForeignPtr (newForeignPtr)

open :: (Int32 -> a) -> IO (CharsetDetector a)
open f = do
  cd <- handleError ucsdet_open
  CD f `fmap` newForeignPtr ucsdet_close cd


foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setText" ucsdet_setText :: Ptr UCharsetDetector -> CString -> Int32 -> UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_open" ucsdet_open :: Ptr UErrorCode -> IO (Ptr UCharsetDetector)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucsdet_close" ucsdet_close :: FunPtr (Ptr UCharsetDetector -> IO ())
