{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Text.ICU.Detect where

import Data.Int (Int32)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Data.Text.ICU.Error.Internal (UErrorCode)
import Data.Text.ICU.Detect.Internal (UCharsetDetector)
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setText" ucsdet_setText :: Ptr UCharsetDetector -> CString -> Int32 -> UErrorCode -> IO ()
