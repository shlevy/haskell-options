-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Test.Chell (Suite, defaultMain)

import           OptionsTests.Tokenize (test_Tokenize)

tests :: [Suite]
tests =
	[ test_Tokenize
	]

main :: IO ()
main = Test.Chell.defaultMain tests
