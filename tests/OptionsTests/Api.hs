{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2014 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Api
	( suite_Api
	) where

import           Control.Applicative
import           Test.Chell

import           Options

data RepeatedStringOpts = RepeatedStringOpts [String]
	deriving (Eq, Show)

repeatedStringList :: OptionType [String]
repeatedStringList = (optionType "repeated-string-list" [] (\x -> Right [x]) show)
	{ optionTypeMerge = Just concat
	}

instance Options RepeatedStringOpts where
	defineOptions = pure RepeatedStringOpts
		<*> defineOption repeatedStringList (\o -> o
			{ optionShortFlags = ['s']
			})

suite_Api :: Suite
suite_Api = suite "api"
	test_RepeatedFlags

test_RepeatedFlags :: Test
test_RepeatedFlags = assertions "repeated-flags" $ do
	let parsed = parseOptions ["-sfoo", "-sbar", "-sbaz"]
	$assert (equal (parsedOptions parsed) (Just (RepeatedStringOpts ["foo", "bar", "baz"])))
