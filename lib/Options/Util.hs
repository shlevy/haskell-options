{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Options.Util
-- License: MIT
module Options.Util where

import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (chr, isAlphaNum, isLetter, isUpper)
import qualified Data.Set as Set
import           Foreign
import           Foreign.C

stringToGhc704 :: String -> String
#if __GLASGOW_HASKELL__ >= 702 || defined(CABAL_OS_WINDOWS)
-- NOTE: GHC 7.2 uses the range 0xEF80 through 0xEFFF to store its encoded
-- bytes. This range is also valid Unicode, so there's no way to discern
-- between a non-Unicode value, and just weird Unicode.
--
-- When decoding strings, FilePath assumes that codepoints in this range are
-- actually bytes because file paths are likely to contain arbitrary bytes
-- on POSIX systems.
--
-- Here, we make the opposite decision, because an option is more likely to
-- be intended as text.
stringToGhc704 = id
#else
stringToGhc704 = decodeUtf8 . Char8.pack
#endif

decodeUtf8 :: Char8.ByteString -> String
decodeUtf8 bytes = map (chr . fromIntegral) words where
	words = unsafePerformIO (unsafeUseAsCStringLen bytes io)
	io (bytesPtr, len) = allocaArray len $ \wordsPtr -> do
		nWords <- c_decodeString (castPtr bytesPtr) wordsPtr (fromIntegral len)
		peekArray (fromIntegral nWords) wordsPtr

foreign import ccall unsafe "hsoptions_decode_string"
	c_decodeString :: Ptr Word8 -> Ptr Word32 -> CInt -> IO CInt

validFieldName :: String -> Bool
validFieldName = valid where
	valid s = case s of
		[] -> False
		c : cs -> validFirst c && all validGeneral cs
	validFirst c = c == '_' || (isLetter c && not (isUpper c))
	validGeneral c = isAlphaNum c || c == '_' || c == '\''

validShortFlag :: Char -> Bool
validShortFlag = isAlphaNum

validLongFlag :: String -> Bool
validLongFlag = valid where
	valid s = case s of
		[] -> False
		c : cs -> validFirst c && all validGeneral cs
	validFirst = isAlphaNum
	validGeneral c = isAlphaNum c || c == '-' || c == '_'

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = Set.size (Set.fromList xs) /= length xs
