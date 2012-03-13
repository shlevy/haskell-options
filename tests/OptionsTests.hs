-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Test.Chell (Suite, defaultMain)

import           OptionsTests.Tokenize (test_Tokenize)
import           OptionsTests.Help (test_Help)

tests :: [Suite]
tests =
	[ test_Tokenize
	, test_Help
	]

main :: IO ()
main = Test.Chell.defaultMain tests
