{-# LANGUAGE CPP #-}

-- |
-- Module: Options.Util
-- License: MIT
module Options.Util where

import qualified Data.ByteString.Char8 as Char8
import           Data.Char (chr, isAlpha, isAlphaNum, isLower)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

decodeString :: String -> String
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
decodeString = id
#else
decodeString s = Text.unpack (Text.decodeUtf8With step (Char8.pack s)) where
	step _ = fmap (\w -> chr (fromIntegral w + 0xDC00))
#endif

validFieldName :: String -> Bool
validFieldName = valid where
	valid s = case s of
		[] -> False
		c : cs -> validFirst c && all validGeneral cs
	validFirst c = isLower c || c == '_'
	validGeneral c = isAlphaNum c || c == '_' || c == '\''

validShortFlag :: Char -> Bool
validShortFlag = isAlphaNum

validLongFlag :: String -> Bool
validLongFlag = valid where
	valid s = case s of
		[] -> False
		_ -> all validChar s
	validChar c = isAlphaNum c || c == '-' || c == '_'

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = Set.size (Set.fromList xs) /= length xs
