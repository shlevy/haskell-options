{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.StringParsing
	( test_StringParsing
	) where

import           Prelude hiding (FilePath)
import           Control.Monad (unless)
import           Data.Text ()

import qualified Filesystem.Path.Rules as Path
import qualified Filesystem.Path.CurrentOS ()
import           Test.Chell

import           Options

$(defineOptions "StringOptions" $ do
	stringOption "optString" "string" "" ""
	textOption "optText" "text" "" ""
	pathOption "optPath" "path" (Path.decodeString Path.posix_ghc704 "") ""
	
	-- String, ASCII default
	option "optString_defA" (\o -> o
		{ optionLongFlags = ["string_defA"]
		, optionDefault = "a"
		})
	
	-- String, Unicode default
	option "optString_defU" (\o -> o
		{ optionLongFlags = ["string_defU"]
		, optionDefault = "\12354"
		})
	
	-- Text, ASCII default
	option "optText_defA" (\o -> o
		{ optionLongFlags = ["text_defA"]
		, optionDefault = "a"
		, optionType = optionTypeText
		})
	
	-- Text, Unicode default
	option "optText_defU" (\o -> o
		{ optionLongFlags = ["text_defU"]
		, optionDefault = "\12354"
		, optionType = optionTypeText
		})
	
	-- FilePath, ASCII default
	option "optPath_defA" (\o -> o
		{ optionLongFlags = ["path_defA"]
		, optionDefault = "a/b/c"
		, optionType = optionTypeFilePath
		})
	
	-- FilePath, Unicode default
	option "optPath_defU" (\o -> o
		{ optionLongFlags = ["path_defU"]
		, optionDefault = "\12354/b/c"
		, optionType = optionTypeFilePath
		})
	
	-- FilePath, bytes default
	option "optPath_defB" (\o -> o
		{ optionLongFlags = ["path_defB"]
		, optionDefault = "\56507/b/c"
		, optionType = optionTypeFilePath
		})
	)

windowsBuild :: Bool
#ifdef CABAL_OS_WINDOWS
windowsBuild = True
#else
windowsBuild = False
#endif

test_StringParsing :: Suite
test_StringParsing = suite "string-parsing"
	[ test_Defaults
	, test_Ascii
	, test_UnicodeValid
	, skipIf windowsBuild test_UnicodeInvalid
	]

test_Defaults :: Suite
test_Defaults = assertions "defaults" $ do
	let opts = defaultOptions
	
	$expect (equal (optString_defA opts) "a")
	$expect (equal (optString_defU opts) "\12354")
	
	$expect (equal (optText_defA opts) "a")
	$expect (equal (optText_defU opts) "\12354")
	
	$expect (equal (optPath_defA opts) (Path.decodeString Path.posix_ghc704 "a/b/c"))
	$expect (equal (optPath_defU opts) (Path.decodeString Path.posix_ghc704 "\12354/b/c"))
	unless windowsBuild $ do
		$expect (equal (optPath_defB opts) (Path.decodeString Path.posix_ghc704 "\56507/b/c"))

test_Ascii :: Suite
test_Ascii = assertions "ascii" $ do
	let parsed = parseOptions ["--string=a", "--text=a", "--path=a"]
	let Just opts = parsedOptions parsed
	
	$expect (equal (optString opts) "a")
	$expect (equal (optText opts) "a")
	$expect (equal (optPath opts) (Path.decodeString Path.posix_ghc704 "a"))

test_UnicodeValid :: Suite
test_UnicodeValid = assertions "unicode-valid" $ do
#if __GLASGOW_HASKELL__ >= 702 || defined(CABAL_OS_WINDOWS)
	let parsed = parseOptions ["--string=\12354", "--text=\12354", "--path=\12354/b/c"]
#else
	let parsed = parseOptions ["--string=\227\129\130", "--text=\227\129\130", "--path=\227\129\130/b/c"]
#endif
	let Just opts = parsedOptions parsed
	
	$expect (equal (optString opts) "\12354")
	$expect (equal (optText opts) "\12354")
	$expect (equal (optPath opts) (Path.decodeString Path.posix_ghc704 "\12354/b/c"))

test_UnicodeInvalid :: Suite
test_UnicodeInvalid = assertions "unicode-invalid" $ do
#if __GLASGOW_HASKELL__ >= 704
	let parsed = parseOptions ["--string=\56507", "--text=\56507", "--path=\12354/\56507"]
	let expectedString = "\56507"
	let expectedText = "\65533"
#elif __GLASGOW_HASKELL__ >= 702
	let parsed = parseOptions ["--string=\61371", "--text=\61371", "--path=\12354/\61371"]
	let expectedString = "\61371"
	let expectedText = "\61371"
#else
	let parsed = parseOptions ["--string=\187", "--text=\187", "--path=\227\129\130/\187"]
	let expectedString = "\56507"
	let expectedText = "\65533"
#endif
	let Just opts = parsedOptions parsed
	
	$expect (equal (optString opts) expectedString)
	$expect (equal (optText opts) expectedText)
	$expect (equal (optPath opts) (Path.decodeString Path.posix_ghc704 "\12354/\56507"))
