{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Util
	( test_Util
	) where

import           Test.Chell

import           Options.Util

test_Util :: Suite
test_Util = suite "util"
	[ test_StringToGhc704
	, test_DecodeString
	, test_ValidFieldName
	, test_ValidShortFlag
	, test_ValidLongFlag
	, test_HasDuplicates
	]

test_StringToGhc704 :: Suite
test_StringToGhc704 = assertions "stringToGhc704" $ do
	return ()

test_DecodeString :: Suite
test_DecodeString = assertions "decodeString" $ do
	return ()

test_ValidFieldName :: Suite
test_ValidFieldName = assertions "validFieldName" $ do
	$expect (validFieldName "a")
	$expect (validFieldName "abc")
	$expect (validFieldName "_abc_")
	$expect (validFieldName "abc'")
	$expect (validFieldName "\12354")
	$expect (not (validFieldName ""))
	$expect (not (validFieldName "'a"))
	$expect (not (validFieldName "a b"))
	$expect (not (validFieldName "Ab"))

test_ValidShortFlag :: Suite
test_ValidShortFlag = assertions "validShortFlag" $ do
	$expect (validShortFlag 'a')
	$expect (validShortFlag 'A')
	$expect (validShortFlag '0')
	$expect (validShortFlag '\12354')
	$expect (not (validShortFlag ' '))
	$expect (not (validShortFlag '-'))

test_ValidLongFlag :: Suite
test_ValidLongFlag = assertions "validLongFlag" $ do
	$expect (validLongFlag "a")
	$expect (validLongFlag "A")
	$expect (validLongFlag "abc")
	$expect (validLongFlag "0")
	$expect (validLongFlag "012")
	$expect (validLongFlag "a-b")
	$expect (validLongFlag "a_b")
	$expect (validLongFlag "\12354bc")
	$expect (not (validLongFlag ""))
	$expect (not (validLongFlag "a b"))
	$expect (not (validLongFlag "a+b"))
	$expect (not (validLongFlag "-"))
	$expect (not (validLongFlag "--"))

test_HasDuplicates :: Suite
test_HasDuplicates = assertions "hasDuplicates" $ do
	$expect (not (hasDuplicates ([] :: [Char])))
	$expect (not (hasDuplicates ['a', 'b']))
	$expect (hasDuplicates ['a', 'b', 'a'])
