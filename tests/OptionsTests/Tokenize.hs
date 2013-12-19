{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Tokenize
	( suite_Tokenize
	) where

import qualified Data.Map as Map
import           Test.Chell

import           Options.Types
import           Options.Tokenize

suite_Tokenize :: Suite
suite_Tokenize = suite "tokenize"
	test_Empty
	test_NoFlag
	test_ShortFlag
	test_ShortFlagUnknown
	test_ShortFlagMissing
	test_ShortFlagUnary
	test_ShortFlagDuplicate
	test_LongFlag
	test_LongFlagUnknown
	test_LongFlagMissing
	test_LongFlagUnary
	test_LongFlagDuplicate
	test_EndFlags
	test_Subcommand
	test_SubcommandUnknown
	test_Unicode

commandDefs :: OptionDefinitions ()
commandDefs = OptionDefinitions
	[ OptionInfo (OptionKey "test.a") ['a'] ["long-a"] "default" False "" Nothing
	, OptionInfo (OptionKey "test.x") ['x'] ["long-x"] "default" True "" Nothing
	, OptionInfo (OptionKey "test.y") ['y'] ["long-y"] "default" True "" Nothing
	, OptionInfo (OptionKey "test.z") ['z'] ["long-z"] "default" True "" Nothing
	]
	[]

subcommandDefs :: OptionDefinitions ()
subcommandDefs = OptionDefinitions
	[ OptionInfo (OptionKey "test.a") ['a'] ["long-a"] "default" False "" Nothing
	, OptionInfo (OptionKey "test.b") ['b'] ["long-b"] "default" False "" Nothing
	, OptionInfo (OptionKey "test.x") ['x'] ["long-x"] "default" True "" Nothing
	, OptionInfo (OptionKey "test.y") ['y'] ["long-y"] "default" True "" Nothing
	, OptionInfo (OptionKey "test.z") ['z'] ["long-z"] "default" True "" Nothing
	]
	[ ("sub1",
		[ OptionInfo (OptionKey "sub.d") ['d'] ["long-d"] "default" False "" Nothing
		, OptionInfo (OptionKey "sub.e") ['e'] ["long-e"] "default" True "" Nothing
		])
	, ("sub2",
		[ OptionInfo (OptionKey "sub.d") ['d'] ["long-d"] "default" True "" Nothing
		, OptionInfo (OptionKey "sub.e") ['e'] ["long-e"] "default" True "" Nothing
		])
	]

unicodeDefs :: OptionDefinitions ()
unicodeDefs = OptionDefinitions
	[ OptionInfo (OptionKey "test.a") ['\12354'] ["long-\12354"] "default" False "" Nothing
	]
	[]

test_Empty :: Test
test_Empty = assertions "empty" $ do
	let (subcmd, eTokens) = tokenize commandDefs []
	$expect (equal Nothing subcmd)
	$assert (right eTokens)
	
	let Right (Tokens tokens args) = eTokens
	$expect (equalTokens [] tokens)
	$expect (equal [] args)

test_NoFlag :: Test
test_NoFlag = assertions "no-flag" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["-", "foo", "bar"]
	$expect (equal Nothing subcmd)
	$assert (right eTokens)
	
	let Right (Tokens tokens args) = eTokens
	$expect (equalTokens [] tokens)
	$expect (equal ["-", "foo", "bar"] args)

test_ShortFlag :: Test
test_ShortFlag = assertions "short-flag" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-a", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.a", Token "-a" "foo")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.a", Token "-a" "foo")] tokens)
		$expect (equal ["bar"] args)

test_ShortFlagUnknown :: Test
test_ShortFlagUnknown = assertions "short-flag-unknown" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["-c", "foo", "bar"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "Unknown flag -c" err)

test_ShortFlagMissing :: Test
test_ShortFlagMissing = assertions "short-flag-missing" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["-a"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "The flag -a requires an argument." err)

test_ShortFlagUnary :: Test
test_ShortFlagUnary = assertions "short-flag-unary" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-x", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.x", TokenUnary "-x")] tokens)
		$expect (equal ["foo", "bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-xy", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.x", TokenUnary "-x"), ("test.y", TokenUnary "-y")] tokens)
		$expect (equal ["foo", "bar"] args)

test_ShortFlagDuplicate :: Test
test_ShortFlagDuplicate = assertions "short-flag-duplicate" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-x", "-x"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Multiple values for flag -x were provided." err)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "-a", "foo"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Multiple values for flag -a were provided." err)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "-afoo"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Multiple values for flag -a were provided." err)

test_LongFlag :: Test
test_LongFlag = assertions "long-flag" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-a", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.a", Token "--long-a" "foo")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-a=foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.a", Token "--long-a" "foo")] tokens)
		$expect (equal ["bar"] args)

test_LongFlagUnknown :: Test
test_LongFlagUnknown = assertions "long-flag-unknown" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-c", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Unknown flag --long-c" err)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-c=foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Unknown flag --long-c" err)

test_LongFlagMissing :: Test
test_LongFlagMissing = assertions "long-flag-missing" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["--long-a"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "The flag --long-a requires an argument." err)

test_LongFlagUnary :: Test
test_LongFlagUnary = assertions "long-flag-unary" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-x", "foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.x", TokenUnary "--long-x")] tokens)
		$expect (equal ["foo", "bar"] args)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["--long-x=foo", "bar"]
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.x", Token "--long-x" "foo")] tokens)
		$expect (equal ["bar"] args)

test_LongFlagDuplicate :: Test
test_LongFlagDuplicate = assertions "long-flag-duplicate" $ do
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-x", "--long-x"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Multiple values for flag --long-x were provided." err)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "--long-a", "foo"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Multiple values for flag --long-a were provided." err)
	do
		let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "--long-a=foo"]
		$expect (equal Nothing subcmd)
		$assert (left eTokens)
		
		let Left err = eTokens
		$expect (equal "Multiple values for flag --long-a were provided." err)

test_EndFlags :: Test
test_EndFlags = assertions "end-flags" $ do
	let (subcmd, eTokens) = tokenize commandDefs ["foo", "--", "-a", "bar"]
	$expect (equal Nothing subcmd)
	$assert (right eTokens)
	
	let Right (Tokens tokens args) = eTokens
	$expect (equalTokens [] tokens)
	$expect (equal ["foo", "-a", "bar"] args)

test_Subcommand :: Test
test_Subcommand = assertions "subcommand" $ do
	do
		let (subcmd, eTokens) = tokenize subcommandDefs ["-x", "sub1", "-d", "foo", "--long-e", "bar"]
		$expect (equal (Just "sub1") subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.x", TokenUnary "-x"), ("sub.d", Token "-d" "foo"), ("sub.e", TokenUnary "--long-e")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize subcommandDefs ["-x", "sub2", "-d", "foo", "--long-e", "bar"]
		$expect (equal (Just "sub2") subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.x", TokenUnary "-x"), ("sub.d", TokenUnary "-d"), ("sub.e", TokenUnary "--long-e")] tokens)
		$expect (equal ["foo", "bar"] args)

test_SubcommandUnknown:: Test
test_SubcommandUnknown = assertions "subcommand-unknown" $ do
	let (subcmd, eTokens) = tokenize subcommandDefs ["foo"]
	$expect (equal Nothing subcmd)
	$assert (left eTokens)
	
	let Left err = eTokens
	$expect (equal "Unknown subcommand \"foo\"." err)

test_Unicode :: Test
test_Unicode = assertions "unicode" $ do
#if defined(OPTIONS_ENCODING_UTF8)
	let shortArgs = ["-\227\129\130", "foo", "bar"]
	let longArgs = ["--long-\227\129\130=foo", "bar"]
#else
	let shortArgs = ["-\12354", "foo", "bar"]
	let longArgs = ["--long-\12354=foo", "bar"]
#endif
	do
		let (subcmd, eTokens) = tokenize unicodeDefs shortArgs
		$expect (equal Nothing subcmd)
		case eTokens of
			Left err -> error err
			Right _ -> return ()
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.a", Token "-\12354" "foo")] tokens)
		$expect (equal ["bar"] args)
	do
		let (subcmd, eTokens) = tokenize unicodeDefs longArgs
		$expect (equal Nothing subcmd)
		$assert (right eTokens)
		
		let Right (Tokens tokens args) = eTokens
		$expect (equalTokens [("test.a", Token "--long-\12354" "foo")] tokens)
		$expect (equal ["bar"] args)

equalTokens :: [(String, Token)] -> Map.Map OptionKey Token -> Assertion
equalTokens tokens = equal (Map.fromList [(OptionKey k, t) | (k, t) <- tokens])
