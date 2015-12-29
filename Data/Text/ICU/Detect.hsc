{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}

module Data.Text.ICU.Detect where

import Data.Int (Int32)
import Foreign.C.String (CString)
import Data.IORef (newIORef, writeIORef)
import Data.Text (Text, empty)
import Data.Text.Foreign (withCStringLen)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Detect.Internal
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)

open :: (Int32 -> a) -> IO (CharsetDetector a)
open f = do
  cd <- handleError ucsdet_open
  r <- newIORef empty
  match <- newIORef (CM nullPtr)
  encoding <- newIORef empty
  CD r encoding match f `fmap` newForeignPtr ucsdet_close cd

setText :: CharsetDetector a -> Text -> IO ()
setText CD{..} t =
  withCStringLen t $ \(ptr, len) -> do
      withForeignPtr cdDetector $ \p -> handleError $
                                        ucsdet_setText p ptr (fromIntegral len)
      writeIORef cdText t

setDeclaredEncoding :: CharsetDetector a -> Text -> IO ()
setDeclaredEncoding CD{..} t =
  withCStringLen t $ \(ptr, len) -> do
    withForeignPtr cdDetector $ \p -> handleError $
                                      ucsdet_setDeclaredEncoding p ptr (fromIntegral len)
    writeIORef cdEncoding t

detect :: CharsetDetector a -> IO CharsetMatch
detect CD{..} = do
  cm <- withForeignPtr cdDetector $ \p -> handleError (ucsdet_detect p)
  let match = CM cm
  writeIORef cdMatch match
  return match

detectCharset :: Text -> IO CharsetMatch
detectCharset text = do
  detector <- open (const ())
  setText detector text
  detect detector


foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_open" ucsdet_open :: Ptr UErrorCode -> IO (Ptr UCharsetDetector)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucsdet_close" ucsdet_close :: FunPtr (Ptr UCharsetDetector -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setText" ucsdet_setText :: Ptr UCharsetDetector -> CString -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setDeclaredEncoding" ucsdet_setDeclaredEncoding :: Ptr UCharsetDetector -> CString -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_detect" ucsdet_detect :: Ptr UCharsetDetector -> Ptr UErrorCode -> IO (Ptr UCharsetMatch)
