{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.OptionTypes
	( suite_OptionTypes
	) where

import           Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Word

import qualified Filesystem.Path.Rules as Path
import qualified Filesystem.Path.CurrentOS ()
import           Test.Chell

import           Options.OptionTypes

suite_OptionTypes :: Suite
suite_OptionTypes = suite "option-types"
	test_Bool
	test_String
	test_Text
	test_FilePath
	test_Int
	test_Int8
	test_Int16
	test_Int32
	test_Int64
	test_Word
	test_Word8
	test_Word16
	test_Word32
	test_Word64
	test_Integer
	test_Float
	test_Double
	test_Maybe
	test_List
	test_Set
	test_Map
	test_Enum

parseValid :: (Show a, Eq a) => OptionType a -> String -> a -> Assertion
parseValid t s expected = equal (optionParser t s) (Right expected)

parseInvalid :: (Show a, Eq a) => OptionType a -> String -> String -> Assertion
parseInvalid t s err = equal (optionParser t s) (Left err)

optionParser :: OptionType a -> String -> Either String a
optionParser (OptionType _ _ p _) = p

test_Bool :: Test
test_Bool = assertions "bool" $ do
	$expect (parseValid optionTypeBool "true" True)
	$expect (parseValid optionTypeBool "false" False)
	$expect (parseInvalid optionTypeBool "" "\"\" is not in {\"true\", \"false\"}.")

test_String :: Test
test_String = assertions "string" $ do
	let valid = parseValid optionTypeString
	let invalid = parseInvalid optionTypeString
	
	$expect (valid "" "")
	$expect (valid "a" "a")
	$expect (valid "\12354" "\12354")
	$expect (valid "\56507" "\56507")
	$expect (valid "\61371" "\61371")

test_Text :: Test
test_Text = assertions "text" $ do
	let p = Text.pack
	let valid = parseValid optionTypeText
	let invalid = parseInvalid optionTypeText
	
	$expect (valid "" (p ""))
	$expect (valid "a" (p "a"))
	$expect (valid "\12354" (p "\12354"))
	$expect (valid "\56507" (p "\65533"))
	$expect (valid "\61371" (p "\61371"))

test_FilePath :: Test
test_FilePath = assertions "filepath" $ do
	let p = Path.decodeString Path.posix_ghc704
	let valid = parseValid optionTypeFilePath
	let invalid = parseInvalid optionTypeFilePath
	
	$expect (valid "" (p ""))
	$expect (valid "a" (p "a"))
	$expect (valid "a-\12403-c.txt" (p "a-\12403-c.txt"))
#if defined(CABAL_OS_WINDOWS)
	$expect (valid "a-\61371-c.txt" (p "a-\61371-c.txt"))
#elif __GLASGOW_HASKELL__ == 702
	$expect (valid "a-\61371-c.txt" (p "a-\56507-c.txt"))
#else
	$expect (valid "a-\56507-c.txt" (p "a-\56507-c.txt"))
	$expect (valid "a-\61371-c.txt" (p "a-\61371-c.txt"))
#endif

test_Int :: Test
test_Int = assertions "int" $ do
	let valid = parseValid optionTypeInt
	let invalid = parseInvalid optionTypeInt
	
	$expect (valid "-1" (-1 :: Int))
	$expect (valid "1" (1 :: Int))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int) - 1)
	let pastMax = show (toInteger (maxBound :: Int) + 1)
	let errBounds = " is not within bounds [" ++ show (minBound :: Int) ++ ":" ++ show (maxBound :: Int) ++ "] of type int."
	
	$expect (invalid pastMin (pastMin ++ errBounds))
	$expect (valid (show (minBound :: Int)) minBound)
	$expect (valid (show (maxBound :: Int)) maxBound)
	$expect (invalid pastMax (pastMax ++ errBounds))

test_Int8 :: Test
test_Int8 = assertions "int8" $ do
	let valid = parseValid optionTypeInt8
	let invalid = parseInvalid optionTypeInt8
	
	$expect (valid "-1" (-1 :: Int8))
	$expect (valid "1" (1 :: Int8))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int8) - 1)
	let pastMax = show (toInteger (maxBound :: Int8) + 1)
	$expect (invalid pastMin "-129 is not within bounds [-128:127] of type int8.")
	$expect (valid (show (minBound :: Int8)) minBound)
	$expect (valid (show (maxBound :: Int8)) maxBound)
	$expect (invalid pastMax "128 is not within bounds [-128:127] of type int8.")

test_Int16 :: Test
test_Int16 = assertions "int16" $ do
	let valid = parseValid optionTypeInt16
	let invalid = parseInvalid optionTypeInt16
	
	$expect (valid "-1" (-1 :: Int16))
	$expect (valid "1" (1 :: Int16))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int16) - 1)
	let pastMax = show (toInteger (maxBound :: Int16) + 1)
	$expect (invalid pastMin "-32769 is not within bounds [-32768:32767] of type int16.")
	$expect (valid (show (minBound :: Int16)) minBound)
	$expect (valid (show (maxBound :: Int16)) maxBound)
	$expect (invalid pastMax "32768 is not within bounds [-32768:32767] of type int16.")

test_Int32 :: Test
test_Int32 = assertions "int32" $ do
	let valid = parseValid optionTypeInt32
	let invalid = parseInvalid optionTypeInt32
	
	$expect (valid "-1" (-1 :: Int32))
	$expect (valid "1" (1 :: Int32))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int32) - 1)
	let pastMax = show (toInteger (maxBound :: Int32) + 1)
	$expect (invalid pastMin "-2147483649 is not within bounds [-2147483648:2147483647] of type int32.")
	$expect (valid (show (minBound :: Int32)) minBound)
	$expect (valid (show (maxBound :: Int32)) maxBound)
	$expect (invalid pastMax "2147483648 is not within bounds [-2147483648:2147483647] of type int32.")

test_Int64 :: Test
test_Int64 = assertions "int64" $ do
	let valid = parseValid optionTypeInt64
	let invalid = parseInvalid optionTypeInt64
	
	$expect (valid "-1" (-1 :: Int64))
	$expect (valid "1" (1 :: Int64))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMin = show (toInteger (minBound :: Int64) - 1)
	let pastMax = show (toInteger (maxBound :: Int64) + 1)
	$expect (invalid pastMin "-9223372036854775809 is not within bounds [-9223372036854775808:9223372036854775807] of type int64.")
	$expect (valid (show (minBound :: Int64)) minBound)
	$expect (valid (show (maxBound :: Int64)) maxBound)
	$expect (invalid pastMax "9223372036854775808 is not within bounds [-9223372036854775808:9223372036854775807] of type int64.")

test_Word :: Test
test_Word = assertions "word" $ do
	let valid = parseValid optionTypeWord
	let invalid = parseInvalid optionTypeWord
	
	let pastMax = show (toInteger (maxBound :: Word) + 1)
	let errBounds = " is not within bounds [0:" ++ show (maxBound :: Word) ++ "] of type word."
	
	$expect (invalid "-1" ("-1" ++ errBounds))
	$expect (valid "0" (0 :: Word))
	$expect (valid "1" (1 :: Word))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	$expect (valid (show (maxBound :: Word)) maxBound)
	$expect (invalid pastMax (pastMax ++ errBounds))

test_Word8 :: Test
test_Word8 = assertions "word8" $ do
	let valid = parseValid optionTypeWord8
	let invalid = parseInvalid optionTypeWord8
	
	$expect (invalid "-1" "-1 is not within bounds [0:255] of type word8.")
	$expect (valid "0" (0 :: Word8))
	$expect (valid "1" (1 :: Word8))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word8) + 1)
	$expect (valid (show (maxBound :: Word8)) maxBound)
	$expect (invalid pastMax "256 is not within bounds [0:255] of type word8.")

test_Word16 :: Test
test_Word16 = assertions "word16" $ do
	let valid = parseValid optionTypeWord16
	let invalid = parseInvalid optionTypeWord16
	
	$expect (invalid "-1" "-1 is not within bounds [0:65535] of type word16.")
	$expect (valid "0" (0 :: Word16))
	$expect (valid "1" (1 :: Word16))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word16) + 1)
	$expect (valid (show (maxBound :: Word16)) maxBound)
	$expect (invalid pastMax "65536 is not within bounds [0:65535] of type word16.")

test_Word32 :: Test
test_Word32 = assertions "word32" $ do
	let valid = parseValid optionTypeWord32
	let invalid = parseInvalid optionTypeWord32
	
	$expect (invalid "-1" "-1 is not within bounds [0:4294967295] of type word32.")
	$expect (valid "0" (0 :: Word32))
	$expect (valid "1" (1 :: Word32))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word32) + 1)
	$expect (valid (show (maxBound :: Word32)) maxBound)
	$expect (invalid pastMax "4294967296 is not within bounds [0:4294967295] of type word32.")

test_Word64 :: Test
test_Word64 = assertions "word64" $ do
	let valid = parseValid optionTypeWord64
	let invalid = parseInvalid optionTypeWord64
	
	$expect (invalid "-1" "-1 is not within bounds [0:18446744073709551615] of type word64.")
	$expect (valid "0" (0 :: Word64))
	$expect (valid "1" (1 :: Word64))
	$expect (invalid "a" "\"a\" is not an integer.")
	
	let pastMax = show (toInteger (maxBound :: Word64) + 1)
	$expect (valid (show (maxBound :: Word64)) maxBound)
	$expect (invalid pastMax "18446744073709551616 is not within bounds [0:18446744073709551615] of type word64.")

test_Integer :: Test
test_Integer = assertions "integer" $ do
	let valid = parseValid optionTypeInteger
	let invalid = parseInvalid optionTypeInteger
	
	$expect (invalid "" "\"\" is not an integer.")
	$expect (valid "-1" (-1 :: Integer))
	$expect (valid "0" (0 :: Integer))
	$expect (valid "1" (1 :: Integer))
	$expect (invalid "a" "\"a\" is not an integer.")

test_Float :: Test
test_Float = assertions "float" $ do
	let valid = parseValid optionTypeFloat
	let invalid = parseInvalid optionTypeFloat
	
	$expect (valid "-1" (-1 :: Float))
	$expect (valid "0" (0 :: Float))
	$expect (valid "1" (1 :: Float))
	$expect (valid "1.5" (1.5 :: Float))
	$expect (valid "3e5" (3e5 :: Float))
	$expect (invalid "a" "\"a\" is not a number.")

test_Double :: Test
test_Double = assertions "double" $ do
	let valid = parseValid optionTypeDouble
	let invalid = parseInvalid optionTypeDouble
	
	$expect (valid "-1" (-1 :: Double))
	$expect (valid "0" (0 :: Double))
	$expect (valid "1" (1 :: Double))
	$expect (valid "1.5" (1.5 :: Double))
	$expect (valid "3e5" (3e5 :: Double))
	$expect (invalid "a" "\"a\" is not a number.")

test_Maybe :: Test
test_Maybe = assertions "maybe" $ do
	let t = optionTypeMaybe optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Nothing)
	$expect (valid "1" (Just 1))
	$expect (invalid "a" "\"a\" is not an integer.")

test_List :: Test
test_List = assertions "list" $ do
	let t = optionTypeList ',' optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" [])
	$expect (valid "1" [1])
	$expect (valid "1,2,3" [1, 2, 3])
	$expect (valid "1,1,2,3" [1, 1, 2, 3])
	$expect (invalid "1,a,3" "\"a\" is not an integer.")

test_Set :: Test
test_Set = assertions "set" $ do
	let t = optionTypeSet ',' optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Set.empty)
	$expect (valid "1" (Set.fromList [1]))
	$expect (valid "1,2,3" (Set.fromList [1, 2, 3]))
	$expect (valid "1,1,2,3" (Set.fromList [1, 2, 3]))
	$expect (invalid "1,a,3" "\"a\" is not an integer.")

test_Map :: Test
test_Map = assertions "map" $ do
	let t = optionTypeMap ',' '=' optionTypeInt optionTypeInt
	let valid = parseValid t
	let invalid = parseInvalid t
	
	$expect (valid "" Map.empty)
	$expect (valid "1=100" (Map.fromList [(1, 100)]))
	$expect (valid "1=100,2=200,3=300" (Map.fromList [(1, 100), (2, 200), (3, 300)]))
	$expect (valid "1=100,2=200,1=300" (Map.fromList [(1, 300), (2, 200)]))
	$expect (invalid "a=1" "\"a\" is not an integer.")
	$expect (invalid "1=a" "\"a\" is not an integer.")
	$expect (invalid "1=" "\"\" is not an integer.")
	$expect (invalid "1" "Map item \"1\" has no value.")

data TestEnum = Enum1 | Enum2 | Enum3
	deriving (Enum, Eq, Show)

test_Enum :: Test
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
	$expect (invalid "e4" "\"e4\" is not in {\"e1\", \"e2\", \"e3\"}.")
