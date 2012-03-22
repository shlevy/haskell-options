{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.OptionTypes
	( test_OptionTypes
	) where

import           Control.Monad (unless)
import           Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Word

import qualified Filesystem.Path.CurrentOS as Path
import           Test.Chell

import           Options.OptionTypes

test_OptionTypes :: Suite
test_OptionTypes = suite "option-types"
	[ test_Bool
	, test_String
	, test_Text
	, test_RawString
	, test_FilePath
	, test_Int
	, test_Int8
	, test_Int16
	, test_Int32
	, test_Int64
	, test_Word
	, test_Word8
	, test_Word16
	, test_Word32
	, test_Word64
	, test_Integer
	, test_Float
	, test_Double
	, test_Maybe
	, test_List
	, test_Set
	, test_Map
	, test_Enum
	]

parseValid :: (Show a, Eq a) => OptionType a -> String -> a -> Assertion
parseValid t s expected = equal (optionParser t s) (Right expected)

parseInvalid :: (Show a, Eq a) => OptionType a -> String -> String -> Assertion
parseInvalid t s err = equal (optionParser t s) (Left err)

optionParser :: OptionType a -> String -> Either String a
optionParser (OptionType _ _ p _) = p

test_Bool :: Suite
test_Bool = assertions "bool" $ do
	$expect (parseValid optionTypeBool "true" True)
	$expect (parseValid optionTypeBool "false" False)
	$expect (parseInvalid optionTypeBool "" "invalid boolean value: \"\"")

nativeAscii, nativeUnicode, nativeBytes, nativeBytesU, nativeBytesT :: String
#if defined(CABAL_OS_WINDOWS)
nativeAscii = "hello"
nativeUnicode = "\12371\12435\12395\12385\12399"
nativeBytes = ""
nativeBytesU = ""
nativeBytesT = ""
#else
#if __GLASGOW_HASKELL__ >= 704
nativeAscii = "hello"
nativeUnicode = "\12371\12435\12395\12385\12399"
nativeBytes = "a-\56507-c.txt"
nativeBytesU = "a-\56507-c.txt"
nativeBytesT = "a-\65533-c.txt"
#elif __GLASGOW_HASKELL__ >= 702
nativeAscii = "hello"
nativeUnicode = "\12371\12435\12395\12385\12399"
nativeBytes = "a-\61371-c.txt"
nativeBytesU = "a-\61371-c.txt"
nativeBytesT = "a-\61371-c.txt"
#else
nativeAscii = "hello"
nativeUnicode = "\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175"
nativeBytes = "a-\187-c.txt"
nativeBytesU = "a-\56507-c.txt"
nativeBytesT = "a-\65533-c.txt"
#endif
#endif

test_String :: Suite
test_String = assertions "string" $ do
	let valid = parseValid optionTypeString
	let invalid = parseInvalid optionTypeString
	
	$expect (valid "" "")
	$expect (valid nativeAscii "hello")
	$expect (valid nativeUnicode "\12371\12435\12395\12385\12399")
	unless (null nativeBytes) $ do
		$expect (valid nativeBytes nativeBytesU)

test_Text :: Suite
test_Text = assertions "text" $ do
	let p = Text.pack
	let valid = parseValid optionTypeText
	let invalid = parseInvalid optionTypeText
	
	$expect (valid "" (p ""))
	$expect (valid nativeAscii (p "hello"))
	$expect (valid nativeUnicode (p "\12371\12435\12395\12385\12399"))
	unless (null nativeBytes) $ do
		$expect (valid nativeBytes (p nativeBytesT))

test_RawString :: Suite
test_RawString = assertions "raw-string" $ do
	let valid = parseValid optionTypeRawString
	let invalid = parseInvalid optionTypeRawString
	
	$expect (valid "" "")
	$expect (valid nativeAscii nativeAscii)
	$expect (valid nativeUnicode nativeUnicode)
	unless (null nativeBytes) $ do
		$expect (valid nativeBytes nativeBytes)

test_FilePath :: Suite
test_FilePath = assertions "filepath" $ do
	let p = Path.decodeString
	let valid = parseValid optionTypeFilePath
	let invalid = parseInvalid optionTypeFilePath
	
	$expect (valid "" (p ""))
	$expect (valid nativeAscii (p nativeAscii))
	$expect (valid nativeUnicode (p nativeUnicode))
	unless (null nativeBytes) $ do
		$expect (valid nativeBytes (p nativeBytes))

test_Int :: Suite
test_Int = assertions "int" $ do
	let valid = parseValid optionTypeInt
	let invalid = parseInvalid optionTypeInt
	
	$expect (valid "-1" (-1 :: Int))
	$expect (valid "1" (1 :: Int))
	$expect (invalid "a" "invalid int: \"a\"")
	
	let pastMin = show (toInteger (minBound :: Int) - 1)
	let pastMax = show (toInteger (maxBound :: Int) + 1)
	$expect (invalid pastMin ("invalid int: " ++ show pastMin))
	$expect (valid (show (minBound :: Int)) minBound)
	$expect (valid (show (maxBound :: Int)) maxBound)
	$expect (invalid pastMax ("invalid int: " ++ show pastMax))

test_Int8 :: Suite
test_Int8 = assertions "int8" $ do
	let valid = parseValid optionTypeInt8
	let invalid = parseInvalid optionTypeInt8
	
	$expect (valid "-1" (-1 :: Int8))
	$expect (valid "1" (1 :: Int8))
	$expect (invalid "a" "invalid int8: \"a\"")
	
	let pastMin = show (toInteger (minBound :: Int8) - 1)
	let pastMax = show (toInteger (maxBound :: Int8) + 1)
	$expect (invalid pastMin ("invalid int8: " ++ show pastMin))
	$expect (valid (show (minBound :: Int8)) minBound)
	$expect (valid (show (maxBound :: Int8)) maxBound)
	$expect (invalid pastMax ("invalid int8: " ++ show pastMax))

test_Int16 :: Suite
test_Int16 = assertions "int16" $ do
	let valid = parseValid optionTypeInt16
	let invalid = parseInvalid optionTypeInt16
	
	$expect (valid "-1" (-1 :: Int16))
	$expect (valid "1" (1 :: Int16))
	$expect (invalid "a" "invalid int16: \"a\"")
	
	let pastMin = show (toInteger (minBound :: Int16) - 1)
	let pastMax = show (toInteger (maxBound :: Int16) + 1)
	$expect (invalid pastMin ("invalid int16: " ++ show pastMin))
	$expect (valid (show (minBound :: Int16)) minBound)
	$expect (valid (show (maxBound :: Int16)) maxBound)
	$expect (invalid pastMax ("invalid int16: " ++ show pastMax))

test_Int32 :: Suite
test_Int32 = assertions "int32" $ do
	let valid = parseValid optionTypeInt32
	let invalid = parseInvalid optionTypeInt32
	
	$expect (valid "-1" (-1 :: Int32))
	$expect (valid "1" (1 :: Int32))
	$expect (invalid "a" "invalid int32: \"a\"")
	
	let pastMin = show (toInteger (minBound :: Int32) - 1)
	let pastMax = show (toInteger (maxBound :: Int32) + 1)
	$expect (invalid pastMin ("invalid int32: " ++ show pastMin))
	$expect (valid (show (minBound :: Int32)) minBound)
	$expect (valid (show (maxBound :: Int32)) maxBound)
	$expect (invalid pastMax ("invalid int32: " ++ show pastMax))

test_Int64 :: Suite
test_Int64 = assertions "int64" $ do
	let valid = parseValid optionTypeInt64
	let invalid = parseInvalid optionTypeInt64
	
	$expect (valid "-1" (-1 :: Int64))
	$expect (valid "1" (1 :: Int64))
	$expect (invalid "a" "invalid int64: \"a\"")
	
	let pastMin = show (toInteger (minBound :: Int64) - 1)
	let pastMax = show (toInteger (maxBound :: Int64) + 1)
	$expect (invalid pastMin ("invalid int64: " ++ show pastMin))
	$expect (valid (show (minBound :: Int64)) minBound)
	$expect (valid (show (maxBound :: Int64)) maxBound)
	$expect (invalid pastMax ("invalid int64: " ++ show pastMax))

test_Word :: Suite
test_Word = assertions "word" $ do
	let valid = parseValid optionTypeWord
	let invalid = parseInvalid optionTypeWord
	
	$expect (invalid "-1" "invalid word: \"-1\"")
	$expect (valid "0" (0 :: Word))
	$expect (valid "1" (1 :: Word))
	$expect (invalid "a" "invalid word: \"a\"")
	
	let pastMax = show (toInteger (maxBound :: Word) + 1)
	$expect (valid (show (maxBound :: Word)) maxBound)
	$expect (invalid pastMax ("invalid word: " ++ show pastMax))

test_Word8 :: Suite
test_Word8 = assertions "word8" $ do
	let valid = parseValid optionTypeWord8
	let invalid = parseInvalid optionTypeWord8
	
	$expect (invalid "-1" "invalid word8: \"-1\"")
	$expect (valid "0" (0 :: Word8))
	$expect (valid "1" (1 :: Word8))
	$expect (invalid "a" "invalid word8: \"a\"")
	
	let pastMax = show (toInteger (maxBound :: Word8) + 1)
	$expect (valid (show (maxBound :: Word8)) maxBound)
	$expect (invalid pastMax ("invalid word8: " ++ show pastMax))

test_Word16 :: Suite
test_Word16 = assertions "word16" $ do
	let valid = parseValid optionTypeWord16
	let invalid = parseInvalid optionTypeWord16
	
	$expect (invalid "-1" "invalid word16: \"-1\"")
	$expect (valid "0" (0 :: Word16))
	$expect (valid "1" (1 :: Word16))
	$expect (invalid "a" "invalid word16: \"a\"")
	
	let pastMax = show (toInteger (maxBound :: Word16) + 1)
	$expect (valid (show (maxBound :: Word16)) maxBound)
	$expect (invalid pastMax ("invalid word16: " ++ show pastMax))

test_Word32 :: Suite
test_Word32 = assertions "word32" $ do
	let valid = parseValid optionTypeWord32
	let invalid = parseInvalid optionTypeWord32
	
	$expect (invalid "-1" "invalid word32: \"-1\"")
	$expect (valid "0" (0 :: Word32))
	$expect (valid "1" (1 :: Word32))
	$expect (invalid "a" "invalid word32: \"a\"")
	
	let pastMax = show (toInteger (maxBound :: Word32) + 1)
	$expect (valid (show (maxBound :: Word32)) maxBound)
	$expect (invalid pastMax ("invalid word32: " ++ show pastMax))

test_Word64 :: Suite
test_Word64 = assertions "word64" $ do
	let valid = parseValid optionTypeWord64
	let invalid = parseInvalid optionTypeWord64
	
	$expect (invalid "-1" "invalid word64: \"-1\"")
	$expect (valid "0" (0 :: Word64))
	$expect (valid "1" (1 :: Word64))
	$expect (invalid "a" "invalid word64: \"a\"")
	
	let pastMax = show (toInteger (maxBound :: Word64) + 1)
	$expect (valid (show (maxBound :: Word64)) maxBound)
	$expect (invalid pastMax ("invalid word64: " ++ show pastMax))

test_Integer :: Suite
test_Integer = assertions "integer" $ do
	let valid = parseValid optionTypeInteger
	let invalid = parseInvalid optionTypeInteger
	
	$expect (invalid "" "invalid integer: \"\"")
	$expect (valid "-1" (-1 :: Integer))
	$expect (valid "0" (0 :: Integer))
	$expect (valid "1" (1 :: Integer))
	$expect (invalid "a" "invalid integer: \"a\"")

test_Float :: Suite
test_Float = assertions "float" $ do
	let valid = parseValid optionTypeFloat
	let invalid = parseInvalid optionTypeFloat
	
	$expect (valid "-1" (-1 :: Float))
	$expect (valid "0" (0 :: Float))
	$expect (valid "1" (1 :: Float))
	$expect (valid "1.5" (1.5 :: Float))
	$expect (valid "3e5" (3e5 :: Float))
	$expect (invalid "a" "invalid float: \"a\"")

test_Double :: Suite
test_Double = assertions "double" $ do
	let valid = parseValid optionTypeDouble
	let invalid = parseInvalid optionTypeDouble
	
	$expect (valid "-1" (-1 :: Double))
	$expect (valid "0" (0 :: Double))
	$expect (valid "1" (1 :: Double))
	$expect (valid "1.5" (1.5 :: Double))
	$expect (valid "3e5" (3e5 :: Double))
	$expect (invalid "a" "invalid double: \"a\"")

test_Maybe :: Suite
test_Maybe = assertions "maybe" $ do
	let t = optionTypeMaybe optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Nothing)
	$expect (valid "1" (Just 1))
	$expect (invalid "a" "invalid int: \"a\"")

test_List :: Suite
test_List = assertions "list" $ do
	let t = optionTypeList ',' optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" [])
	$expect (valid "1" [1])
	$expect (valid "1,2,3" [1, 2, 3])
	$expect (valid "1,1,2,3" [1, 1, 2, 3])
	$expect (invalid "1,a,3" "invalid int: \"a\"")

test_Set :: Suite
test_Set = assertions "set" $ do
	let t = optionTypeSet ',' optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Set.empty)
	$expect (valid "1" (Set.fromList [1]))
	$expect (valid "1,2,3" (Set.fromList [1, 2, 3]))
	$expect (valid "1,1,2,3" (Set.fromList [1, 2, 3]))
	$expect (invalid "1,a,3" "invalid int: \"a\"")

test_Map :: Suite
test_Map = assertions "map" $ do
	let t = optionTypeMap ',' '=' optionTypeInt optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Map.empty)
	$expect (valid "1=100" (Map.fromList [(1, 100)]))
	$expect (valid "1=100,2=200,3=300" (Map.fromList [(1, 100), (2, 200), (3, 300)]))
	$expect (valid "1=100,2=200,1=300" (Map.fromList [(1, 300), (2, 200)]))
	$expect (invalid "a=1" "invalid int: \"a\"")
	$expect (invalid "1=a" "invalid int: \"a\"")
	$expect (invalid "1=" "invalid int: \"\"")
	$expect (invalid "1" "invalid map item with no value: \"1\"")

data TestEnum = Enum1 | Enum2 | Enum3
	deriving (Enum, Eq, Show)

test_Enum :: Suite
test_Enum = assertions "enum" $ do
	let t = optionTypeEnum ''TestEnum
		[ ("e1", Enum1)
		, ("e2", Enum2)
		, ("e3", Enum3)
		]
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "e1" Enum1)
	$expect (valid "e2" Enum2)
	$expect (invalid "e4" "invalid enum value: \"e4\"")
