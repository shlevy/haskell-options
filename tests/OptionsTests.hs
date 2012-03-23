-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Test.Chell (Suite, defaultMain)

import           OptionsTests.Defaults (test_Defaults)
import           OptionsTests.Help (test_Help)
import           OptionsTests.OptionTypes (test_OptionTypes)
import           OptionsTests.Tokenize (test_Tokenize)
import           OptionsTests.StringParsing (test_StringParsing)

tests :: [Suite]
tests =
	[ test_Defaults
	, test_Help
	, test_OptionTypes
	, test_Tokenize
	, test_StringParsing
	]

main :: IO ()
main = Test.Chell.defaultMain tests
