{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Defaults
	( suite_Defaults
	) where

import           Prelude hiding (FilePath)
import           Data.Int
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Word

import           Test.Chell

import           Options

$(defineOptions "AllOptions" $ do
	-- Simple option definitions
	boolOption "s_Bool" "s_bool" True ""
	stringOption "s_String" "s_string" "abc" ""
	stringsOption "s_Strings" "s_strings" ["a", "b", "c"] ""
	intOption "s_Int" "s_int" 123 ""
	integerOption "s_Integer" "s_integer" 123 ""
	floatOption "s_Float" "s_float" 123.5 ""
	doubleOption "s_Double" "s_double" 123.5 ""
	
	-- Explicitly typed option definitions
	option "t_Bool" (\o -> o
		{ optionLongFlags = ["t_bool"]
		, optionType = optionType_bool
		, optionDefault = "true"
		})
	option "t_String" (\o -> o
		{ optionLongFlags = ["t_String"]
		, optionType = optionType_string
		, optionDefault = "abc"
		})
	option "t_Int" (\o -> o
		{ optionLongFlags = ["t_Int"]
		, optionType = optionType_int
		, optionDefault = "123"
		})
	option "t_Int8" (\o -> o
		{ optionLongFlags = ["t_Int8"]
		, optionType = optionType_int8
		, optionDefault = "123"
		})
	option "t_Int16" (\o -> o
		{ optionLongFlags = ["t_Int16"]
		, optionType = optionType_int16
		, optionDefault = "123"
		})
	option "t_Int32" (\o -> o
		{ optionLongFlags = ["t_Int32"]
		, optionType = optionType_int32
		, optionDefault = "123"
		})
	option "t_Int64" (\o -> o
		{ optionLongFlags = ["t_Int64"]
		, optionType = optionType_int64
		, optionDefault = "123"
		})
	option "t_Word" (\o -> o
		{ optionLongFlags = ["t_Word"]
		, optionType = optionType_word
		, optionDefault = "123"
		})
	option "t_Word8" (\o -> o
		{ optionLongFlags = ["t_Word8"]
		, optionType = optionType_word8
		, optionDefault = "123"
		})
	option "t_Word16" (\o -> o
		{ optionLongFlags = ["t_Word16"]
		, optionType = optionType_word16
		, optionDefault = "123"
		})
	option "t_Word32" (\o -> o
		{ optionLongFlags = ["t_Word32"]
		, optionType = optionType_word32
		, optionDefault = "123"
		})
	option "t_Word64" (\o -> o
		{ optionLongFlags = ["t_Word64"]
		, optionType = optionType_word64
		, optionDefault = "123"
		})
	option "t_Integer" (\o -> o
		{ optionLongFlags = ["t_Integer"]
		, optionType = optionType_integer
		, optionDefault = "123"
		})
	option "t_Float" (\o -> o
		{ optionLongFlags = ["t_Float"]
		, optionType = optionType_float
		, optionDefault = "123.5"
		})
	option "t_Double" (\o -> o
		{ optionLongFlags = ["t_Double"]
		, optionType = optionType_double
		, optionDefault = "123.5"
		})
	option "t_Maybe" (\o -> o
		{ optionLongFlags = ["t_Maybe"]
		, optionType = optionType_maybe optionType_bool
		, optionDefault = "true"
		})
	option "t_List" (\o -> o
		{ optionLongFlags = ["t_List"]
		, optionType = optionType_list ',' optionType_bool
		, optionDefault = "true,false"
		})
	option "t_Set" (\o -> o
		{ optionLongFlags = ["t_Set"]
		, optionType = optionType_set ',' optionType_bool
		, optionDefault = "true,false"
		})
	option "t_Map" (\o -> o
		{ optionLongFlags = ["t_Map"]
		, optionType = optionType_map ',' '=' optionType_string optionType_bool
		, optionDefault = "true=true,false=false"
		})
	option "t_Enum" (\o -> o
		{ optionLongFlags = ["t_Enum"]
		, optionType = optionType_enum ''Bool
			[ ("true", True)
			, ("false", False)
			]
		, optionDefault = "true"
		})
	)

suite_Defaults :: Suite
suite_Defaults = suite "defaults"
	test_Defaults

test_Defaults :: Test
test_Defaults = assertions "defaults" $ do
	let def = defaultOptions :: AllOptions
	
	-- Simple option definitions
	$expect (equal (s_Bool def) True)
	$expect (equal (s_String def) ("abc" :: String))
	$expect (equal (s_Strings def) (["a", "b", "c"] :: [String]))
	$expect (equal (s_Int def) (123 :: Int))
	$expect (equal (s_Integer def) (123 :: Integer))
	$expect (equal (s_Float def) (123.5 :: Float))
	$expect (equal (s_Double def) (123.5 :: Double))
	
	-- Explicitly typed option definitions
	$expect (equal (t_Bool def) True)
	$expect (equal (t_String def) ("abc" :: String))
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
	$expect (equal (t_Maybe def) (Just True :: Maybe Bool))
	$expect (equal (t_List def) ([True,False] :: [Bool]))
	$expect (equal (t_Set def) (Set.fromList [True, False] :: Set Bool))
	$expect (equal (t_Map def) (Map.fromList [("true", True), ("false", False)] :: Map String Bool))
	$expect (equal (t_Enum def) True)
