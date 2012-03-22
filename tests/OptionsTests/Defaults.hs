{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Defaults
	( test_Defaults
	) where

import           Prelude hiding (FilePath)
import           Data.Int
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Word

import           Filesystem.Path.CurrentOS (FilePath)
import           Test.Chell

import           Options

defineOptions "AllOptions" $ do
	-- Simple option definitions
	boolOption "s_Bool" "s_bool" True ""
	stringOption "s_String" "s_string" "abc" ""
	stringsOption "s_Strings" "s_strings" ["a", "b", "c"] ""
	textOption "s_Text" "s_text" "abc" ""
	textsOption "s_Texts" "s_texts" ["a", "b", "c"] ""
	pathOption "s_Path" "s_path" "a/b/c" ""
	intOption "s_Int" "s_int" 123 ""
	integerOption "s_Integer" "s_integer" 123 ""
	floatOption "s_Float" "s_float" 123.5 ""
	doubleOption "s_Double" "s_double" 123.5 ""
	
	-- Explicitly typed option definitions
	option "t_Bool" (\o -> o
		{ optionLongFlags = ["t_bool"]
		, optionType = optionTypeBool
		, optionDefault = "true"
		})
	option "t_String" (\o -> o
		{ optionLongFlags = ["t_String"]
		, optionType = optionTypeString
		, optionDefault = "abc"
		})
	option "t_Text" (\o -> o
		{ optionLongFlags = ["t_Text"]
		, optionType = optionTypeText
		, optionDefault = "abc"
		})
	option "t_RawString" (\o -> o
		{ optionLongFlags = ["t_RawString"]
		, optionType = optionTypeRawString
		, optionDefault = "abc"
		})
	option "t_FilePath" (\o -> o
		{ optionLongFlags = ["t_FilePath"]
		, optionType = optionTypeFilePath
		, optionDefault = "a/b/c"
		})
	option "t_Int" (\o -> o
		{ optionLongFlags = ["t_Int"]
		, optionType = optionTypeInt
		, optionDefault = "123"
		})
	option "t_Int8" (\o -> o
		{ optionLongFlags = ["t_Int8"]
		, optionType = optionTypeInt8
		, optionDefault = "123"
		})
	option "t_Int16" (\o -> o
		{ optionLongFlags = ["t_Int16"]
		, optionType = optionTypeInt16
		, optionDefault = "123"
		})
	option "t_Int32" (\o -> o
		{ optionLongFlags = ["t_Int32"]
		, optionType = optionTypeInt32
		, optionDefault = "123"
		})
	option "t_Int64" (\o -> o
		{ optionLongFlags = ["t_Int64"]
		, optionType = optionTypeInt64
		, optionDefault = "123"
		})
	option "t_Word" (\o -> o
		{ optionLongFlags = ["t_Word"]
		, optionType = optionTypeWord
		, optionDefault = "123"
		})
	option "t_Word8" (\o -> o
		{ optionLongFlags = ["t_Word8"]
		, optionType = optionTypeWord8
		, optionDefault = "123"
		})
	option "t_Word16" (\o -> o
		{ optionLongFlags = ["t_Word16"]
		, optionType = optionTypeWord16
		, optionDefault = "123"
		})
	option "t_Word32" (\o -> o
		{ optionLongFlags = ["t_Word32"]
		, optionType = optionTypeWord32
		, optionDefault = "123"
		})
	option "t_Word64" (\o -> o
		{ optionLongFlags = ["t_Word64"]
		, optionType = optionTypeWord64
		, optionDefault = "123"
		})
	option "t_Integer" (\o -> o
		{ optionLongFlags = ["t_Integer"]
		, optionType = optionTypeInteger
		, optionDefault = "123"
		})
	option "t_Float" (\o -> o
		{ optionLongFlags = ["t_Float"]
		, optionType = optionTypeFloat
		, optionDefault = "123.5"
		})
	option "t_Double" (\o -> o
		{ optionLongFlags = ["t_Double"]
		, optionType = optionTypeDouble
		, optionDefault = "123.5"
		})
	option "t_List" (\o -> o
		{ optionLongFlags = ["t_List"]
		, optionType = optionTypeList ',' optionTypeBool
		, optionDefault = "true,false"
		})
	option "t_Set" (\o -> o
		{ optionLongFlags = ["t_Set"]
		, optionType = optionTypeSet ',' optionTypeBool
		, optionDefault = "true,false"
		})
	option "t_Map" (\o -> o
		{ optionLongFlags = ["t_Map"]
		, optionType = optionTypeMap ',' '=' optionTypeString optionTypeBool
		, optionDefault = "true=true,false=false"
		})
	option "t_Enum" (\o -> o
		{ optionLongFlags = ["t_Enum"]
		, optionType = optionTypeEnum ''Bool
			[ ("true", True)
			, ("false", False)
			]
		, optionDefault = "true"
		})

test_Defaults :: Suite
test_Defaults = assertions "defaults" $ do
	let def = defaultOptions :: AllOptions
	
	-- Simple option definitions
	$expect (equal (s_Bool def) True)
	$expect (equal (s_String def) ("abc" :: String))
	$expect (equal (s_Strings def) (["a", "b", "c"] :: [String]))
	$expect (equal (s_Text def) ("abc" :: Text))
	$expect (equal (s_Texts def) (["a", "b", "c"] :: [Text]))
	$expect (equal (s_Path def) ("a/b/c" :: FilePath))
	$expect (equal (s_Int def) (123 :: Int))
	$expect (equal (s_Integer def) (123 :: Integer))
	$expect (equal (s_Float def) (123.5 :: Float))
	$expect (equal (s_Double def) (123.5 :: Double))
	
	-- Explicitly typed option definitions
	$expect (equal (t_Bool def) True)
	$expect (equal (t_String def) ("abc" :: String))
	$expect (equal (t_Text def) ("abc" :: Text))
	$expect (equal (t_RawString def) ("abc" :: String))
	$expect (equal (t_FilePath def) ("a/b/c" :: FilePath))
	$expect (equal (t_Int def) (123 :: Int))
	$expect (equal (t_Int8 def) (123 :: Int8))
	$expect (equal (t_Int16 def) (123 :: Int16))
	$expect (equal (t_Int32 def) (123 :: Int32))
	$expect (equal (t_Int64 def) (123 :: Int64))
	$expect (equal (t_Word def) (123 :: Word))
	$expect (equal (t_Word8 def) (123 :: Word8))
	$expect (equal (t_Word16 def) (123 :: Word16))
	$expect (equal (t_Word32 def) (123 :: Word32))
	$expect (equal (t_Word64 def) (123 :: Word64))
	$expect (equal (t_Integer def) (123 :: Integer))
	$expect (equal (t_Float def) (123.5 :: Float))
	$expect (equal (t_Double def) (123.5 :: Double))
	$expect (equal (t_List def) ([True,False] :: [Bool]))
	$expect (equal (t_Set def) (Set.fromList [True, False] :: Set Bool))
	$expect (equal (t_Map def) (Map.fromList [("true", True), ("false", False)] :: Map String Bool))
	$expect (equal (t_Enum def) True)
