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
import           OptionsTests.StringParsing (test_StringParsing)
import           OptionsTests.Tokenize (test_Tokenize)
import           OptionsTests.Util (test_Util)

tests :: [Suite]
tests =
	[ test_Defaults
	, test_Help
	, test_OptionTypes
	, test_StringParsing
	, test_Tokenize
	, test_Util
	]

main :: IO ()
main = Test.Chell.defaultMain tests
