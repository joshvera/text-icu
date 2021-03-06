{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}

module Data.Text.ICU.Detect (detectCharset, getName, getConfidence, getLanguage) where

import Data.Int (Int32)
import Foreign.C.String (CString)
import Data.IORef (newIORef, writeIORef)
import Data.ByteString (ByteString, empty, useAsCStringLen)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Detect.Internal
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.ForeignPtr (newForeignPtr, newForeignPtr_, withForeignPtr)

open :: (Int32 -> a) -> IO (CharsetDetector a)
open f = do
  cd <- handleError ucsdet_open
  r <- newIORef empty
  emptyMatch <- CM <$> newForeignPtr_ nullPtr
  match <- newIORef emptyMatch
  encoding <- newIORef empty
  CD r encoding match f `fmap` newForeignPtr ucsdet_close cd

setText :: CharsetDetector a -> ByteString -> IO ()
setText CD{..} t =
  useAsCStringLen t $ \(ptr, len) -> do
    withForeignPtr cdDetector $ \p -> handleError $
                                      ucsdet_setText p ptr (fromIntegral len)
    writeIORef cdBytestring t

setDeclaredEncoding :: CharsetDetector a -> ByteString -> IO ()
setDeclaredEncoding CD{..} t =
  useAsCStringLen t $ \(ptr, len) -> do
  withForeignPtr cdDetector $ \p -> handleError $
                                    ucsdet_setDeclaredEncoding p ptr (fromIntegral len)
  writeIORef cdEncoding t

detectCharset :: ByteString -> IO String
detectCharset text = do
  detector <- open (const ())
  setText detector text
  detect detector


foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_open" ucsdet_open :: Ptr UErrorCode -> IO (Ptr UCharsetDetector)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucsdet_close" ucsdet_close :: FunPtr (Ptr UCharsetDetector -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setText" ucsdet_setText :: Ptr UCharsetDetector -> CString -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setDeclaredEncoding" ucsdet_setDeclaredEncoding :: Ptr UCharsetDetector -> CString -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_detect" ucsdet_detect :: Ptr UCharsetDetector -> Ptr UErrorCode -> IO (Ptr UCharsetMatch)
