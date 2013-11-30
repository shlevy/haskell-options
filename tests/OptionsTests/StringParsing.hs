{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.StringParsing
	( suite_StringParsing
	) where

import           Test.Chell

import           Options

$(defineOptions "StringOptions" $ do
	stringOption "optString" "string" "" ""
	
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
	)

suite_StringParsing :: Suite
suite_StringParsing = suite "string-parsing"
	test_Defaults
	test_Ascii
	test_UnicodeValid
	test_UnicodeInvalid

test_Defaults :: Test
test_Defaults = assertions "defaults" $ do
	let opts = defaultOptions
	
	$expect (equal (optString_defA opts) "a")
	$expect (equal (optString_defU opts) "\12354")

test_Ascii :: Test
test_Ascii = assertions "ascii" $ do
	let parsed = parseOptions ["--string=a"]
	let Just opts = parsedOptions parsed
	
	$expect (equal (optString opts) "a")

test_UnicodeValid :: Test
test_UnicodeValid = assertions "unicode-valid" $ do
#if defined(OPTIONS_ENCODING_UTF8)
	let parsed = parseOptions ["--string=\227\129\130"]
#else
	let parsed = parseOptions ["--string=\12354"]
#endif
	let Just opts = parsedOptions parsed
	
	$expect (equal (optString opts) "\12354")

test_UnicodeInvalid :: Test
test_UnicodeInvalid = assertions "unicode-invalid" $ do
#if __GLASGOW_HASKELL__ >= 704
	let parsed = parseOptions ["--string=\56507"]
	let expectedString = "\56507"
#elif __GLASGOW_HASKELL__ >= 702
	let parsed = parseOptions ["--string=\61371"]
	let expectedString = "\61371"
#else
	let parsed = parseOptions ["--string=\187"]
	let expectedString = "\56507"
#endif
	let Just opts = parsedOptions parsed
	
	$expect (equal (optString opts) expectedString)
