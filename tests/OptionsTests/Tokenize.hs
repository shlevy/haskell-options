{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Tokenize
	( test_Tokenize
	) where

import           Test.Chell

import           Options.Types
import           Options.Tokenize

test_Tokenize :: Suite
test_Tokenize = suite "tokenize"
	[ test_Empty
	, test_NoFlag
	, test_ShortFlag
	, test_ShortFlagUnknown
	, test_ShortFlagMissing
	, test_ShortFlagUnary
	, test_LongFlag
	, test_LongFlagUnknown
	, test_LongFlagMissing
	, test_LongFlagUnary
	, test_EndFlags
	, test_Subcommand
	, test_SubcommandUnknown
	]

commandDefs :: OptionDefinitions ()
commandDefs = OptionDefinitions
	[ OptionInfo "test.a" ['a'] ["long-a"] "default" False
	, OptionInfo "test.x" ['x'] ["long-x"] "default" True
	, OptionInfo "test.y" ['y'] ["long-y"] "default" True
	, OptionInfo "test.z" ['z'] ["long-z"] "default" True
	]
	[]

subcommandDefs :: OptionDefinitions ()
subcommandDefs = OptionDefinitions
	[ OptionInfo "test.a" ['a'] ["long-a"] "default" False
	, OptionInfo "test.b" ['b'] ["long-b"] "default" False
	, OptionInfo "test.x" ['x'] ["long-x"] "default" True
	, OptionInfo "test.y" ['y'] ["long-y"] "default" True
	, OptionInfo "test.z" ['z'] ["long-z"] "default" True
	]
	[ ("sub1",
		[ OptionInfo "sub.d" ['d'] ["long-d"] "default" False
		, OptionInfo "sub.e" ['e'] ["long-e"] "default" True
		])
	, ("sub2",
		[ OptionInfo "sub.d" ['d'] ["long-d"] "default" True
		, OptionInfo "sub.e" ['e'] ["long-e"] "default" True
		])
	]

test_Empty :: Suite
test_Empty = assertions "empty" $ do
	let (subcmd, eTokens) = tokenize commandDefs []
	$expect (equal Nothing subcmd)
	$assert (right eTokens)
	
	let Right (TokensFor tokens args) = eTokens
	$expect (equal [] tokens)
	$expect (equal [] args)

test_NoFlag :: Suite
test_NoFlag = assertions "no-flag" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["-", "foo", "bar"]
	$expect (equal Nothing subcmd)
	$assert (right eTokens)
	
	let Right (TokensFor tokens args) = eTokens
	$expect (equal [] tokens)
	$expect (equal ["-", "foo", "bar"] args)

test_ShortFlag :: Suite
test_ShortFlag = assertions "short-flag" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-a", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.a", "foo")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.a", "foo")] tokens)
		$expect (equal ["bar"] args)

test_ShortFlagUnknown :: Suite
test_ShortFlagUnknown = assertions "short-flag-unknown" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["-c", "foo", "bar"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "Unknown option: -c" err)

test_ShortFlagMissing :: Suite
test_ShortFlagMissing = assertions "short-flag-missing" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["-a"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "option -a requires an argument" err)

test_ShortFlagUnary :: Suite
test_ShortFlagUnary = assertions "short-flag-unary" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-x", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.x", "")] tokens)
		$expect (equal ["foo", "bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-xy", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.x", ""), ("test.y", "")] tokens)
		$expect (equal ["foo", "bar"] args)

test_LongFlag :: Suite
test_LongFlag = assertions "long-flag" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-a", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.a", "foo")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-a=foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.a", "foo")] tokens)
		$expect (equal ["bar"] args)

test_LongFlagUnknown :: Suite
test_LongFlagUnknown = assertions "long-flag-unknown" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-c", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Unknown option: --long-c" err)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-c=foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Unknown option: --long-c" err)

test_LongFlagMissing :: Suite
test_LongFlagMissing = assertions "long-flag-missing" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["--long-a"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "option --long-a requires an argument" err)

test_LongFlagUnary :: Suite
test_LongFlagUnary = assertions "long-flag-unary" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-x", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.x", "")] tokens)
		$expect (equal ["foo", "bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-x=foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.x", "foo")] tokens)
		$expect (equal ["bar"] args)

test_EndFlags :: Suite
test_EndFlags = assertions "end-flags" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["foo", "--", "-a", "bar"]
	$expect (equal Nothing subcmd)
	$assert (right eTokens)
	
	let Right (TokensFor tokens args) = eTokens
	$expect (equal [] tokens)
	$expect (equal ["foo", "-a", "bar"] args)

test_Subcommand :: Suite
test_Subcommand = assertions "subcommand" $ do
	do
		let (subcmd, eTokens) = tokenize subcommandDefs ["-x", "sub1", "-d", "foo", "--long-e", "bar"]
		$expect (equal (Just "sub1") subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.x", ""), ("sub.d", "foo"), ("sub.e", "")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize subcommandDefs ["-x", "sub2", "-d", "foo", "--long-e", "bar"]
		$expect (equal (Just "sub2") subcmd)
		$assert (right eTokens)
		
		let Right (TokensFor tokens args) = eTokens
		$expect (equal [("test.x", ""), ("sub.d", ""), ("sub.e", "")] tokens)
		$expect (equal ["foo", "bar"] args)

test_SubcommandUnknown:: Suite
test_SubcommandUnknown = assertions "subcommand-unknown" $ do
	let (subcmd, eTokens) = tokenize subcommandDefs ["foo"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "unknown subcommand \"foo\"" err)
