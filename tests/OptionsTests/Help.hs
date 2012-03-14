{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Help
	( test_Help
	) where

import           Test.Chell

import           Options.Types
import           Options.Help

test_Help :: Suite
test_Help = suite "help"
	[ test_AddHelpFlags
	, test_CheckHelpFlag
	, test_ShowHelpSummary
	, test_ShowHelpSummary_Subcommand
	, test_ShowHelpAll
	, test_ShowHelpAll_Subcommand
	, test_ShowHelpGroup
	, test_ShowHelpGroup_Subcommand
	, test_ShowHelpGroup_SubcommandInvalid
	]

test_AddHelpFlags :: Suite
test_AddHelpFlags = suite "addHelpFlags"
	[ test_AddHelpFlags_None
	, test_AddHelpFlags_Short
	, test_AddHelpFlags_Long
	, test_AddHelpFlags_Both
	, test_AddHelpFlags_NoAll
	, test_AddHelpFlags_Subcommand
	]

groupInfoHelp :: Maybe GroupInfo
groupInfoHelp = Just (GroupInfo
	{ groupInfoName = "all"
	, groupInfoDescription = "Help Options"
	, groupInfoHelpDescription = "Show all help options."
	})

infoHelpSummary :: [Char] -> [String] -> OptionInfo
infoHelpSummary shorts longs = OptionInfo
	{ optionInfoKey = "main:Options.Help:optHelpSummary"
	, optionInfoShortFlags = shorts
	, optionInfoLongFlags = longs
	, optionInfoDefault = "false"
	, optionInfoUnary = True
	, optionInfoDescription = "Show option summary." 
	, optionInfoGroup = groupInfoHelp
	}

infoHelpAll :: OptionInfo
infoHelpAll = OptionInfo
	{ optionInfoKey = "main:Options.Help:optHelpGroup:all"
	, optionInfoShortFlags = []
	, optionInfoLongFlags = ["help-all"]
	, optionInfoDefault = "false"
	, optionInfoUnary = True
	, optionInfoDescription = "Show all help options." 
	, optionInfoGroup = groupInfoHelp
	}

test_AddHelpFlags_None :: Suite
test_AddHelpFlags_None = assertions "none" $ do
	let commandDefs = OptionDefinitions
		[ OptionInfo "test.help" ['h'] ["help"] "default" False "" Nothing
		]
		[]
	let helpAdded = addHelpFlags commandDefs
	let OptionDefinitions opts subcmds = helpAdded
	
	$expect (equal opts
		[ infoHelpAll
		, OptionInfo "test.help" ['h'] ["help"] "default" False "" Nothing
		])
	$expect (equal subcmds [])

test_AddHelpFlags_Short :: Suite
test_AddHelpFlags_Short = assertions "short" $ do
	let commandDefs = OptionDefinitions
		[ OptionInfo "test.help" [] ["help"] "default" False "" Nothing
		]
		[]
	let helpAdded = addHelpFlags commandDefs
	let OptionDefinitions opts subcmds = helpAdded
	
	$expect (equal opts
		[ infoHelpSummary ['h'] []
		, infoHelpAll
		, OptionInfo "test.help" [] ["help"] "default" False "" Nothing
		])
	$expect (equal subcmds [])

test_AddHelpFlags_Long :: Suite
test_AddHelpFlags_Long = assertions "long" $ do
	let commandDefs = OptionDefinitions
		[ OptionInfo "test.help" ['h'] [] "default" False "" Nothing
		]
		[]
	let helpAdded = addHelpFlags commandDefs
	let OptionDefinitions opts subcmds = helpAdded
	
	$expect (equal opts
		[ infoHelpSummary [] ["help"]
		, infoHelpAll
		, OptionInfo "test.help" ['h'] [] "default" False "" Nothing
		])
	$expect (equal subcmds [])

test_AddHelpFlags_Both :: Suite
test_AddHelpFlags_Both = assertions "both" $ do
	let commandDefs = OptionDefinitions [] []
	let helpAdded = addHelpFlags commandDefs
	let OptionDefinitions opts subcmds = helpAdded
	
	$expect (equal opts
		[ infoHelpSummary ['h'] ["help"]
		, infoHelpAll
		])
	$expect (equal subcmds [])

test_AddHelpFlags_NoAll :: Suite
test_AddHelpFlags_NoAll = assertions "no-all" $ do
	let commandDefs = OptionDefinitions
		[ OptionInfo "test.help" ['h'] ["help", "help-all"] "default" False "" Nothing
		]
		[]
	let helpAdded = addHelpFlags commandDefs
	let OptionDefinitions opts subcmds = helpAdded
	
	$expect (equal opts
		[ OptionInfo "test.help" ['h'] ["help", "help-all"] "default" False "" Nothing
		])
	$expect (equal subcmds [])

test_AddHelpFlags_Subcommand :: Suite
test_AddHelpFlags_Subcommand = assertions "subcommand" $ do
	let cmd1_a = OptionInfo "test.cmd1.a" ['a'] [] "" False "" (Just GroupInfo
		{ groupInfoName = "foo"
		, groupInfoDescription = "Foo Options"
		, groupInfoHelpDescription = "More Foo Options"
		})
	let cmd1_b = OptionInfo "test.cmd1.b" ['b'] [] "" False "" (Just GroupInfo
		{ groupInfoName = "all"
		, groupInfoDescription = "All Options"
		, groupInfoHelpDescription = "More All Options"
		})
	let commandDefs = OptionDefinitions
		[]
		[("cmd1", [cmd1_a, cmd1_b])]
	let helpAdded = addHelpFlags commandDefs
	let OptionDefinitions opts subcmds = helpAdded
	
	let helpFoo = OptionInfo
		{ optionInfoKey = "main:Options.Help:optHelpGroup:foo"
		, optionInfoShortFlags = []
		, optionInfoLongFlags = ["help-foo"]
		, optionInfoDefault = "false"
		, optionInfoUnary = True
		, optionInfoDescription = "More Foo Options" 
		, optionInfoGroup = Just (GroupInfo
			{ groupInfoName = "all"
			, groupInfoDescription = "Help Options"
			, groupInfoHelpDescription = "Show all help options."
			})
		}
	
	$expect (equal opts
		[ infoHelpSummary ['h'] ["help"]
		, infoHelpAll
		])
	$expect (equal subcmds [("cmd1", [helpFoo, cmd1_a, cmd1_b])])

test_CheckHelpFlag :: Suite
test_CheckHelpFlag = assertions "checkHelpFlag" $ do
	let checkFlag keys = equal (checkHelpFlag (TokensFor [(k, "true") | k <- keys] []))
	
	$expect (checkFlag [] Nothing)
	$expect (checkFlag ["main:Options.Help:optHelpSummary"] (Just HelpSummary))
	$expect (checkFlag ["main:Options.Help:optHelpGroup:all"] (Just HelpAll))
	$expect (checkFlag ["main:Options.Help:optHelpGroup:foo"] (Just (HelpGroup "foo")))

variedOptions :: OptionDefinitions ()
variedOptions = addHelpFlags $ OptionDefinitions
	[ OptionInfo "test.a" ['a'] ["long-a"] "def" False "a description here" Nothing
	, OptionInfo "test.long1" [] ["a-looooooooooooong-option"] "def" False "description here" Nothing
	, OptionInfo "test.long2" [] ["a-loooooooooooooong-option"] "def" False "description here" Nothing
	, OptionInfo "test.b" ['b'] ["long-b"] "def" False "b description here" Nothing
	, OptionInfo "test.g" ['g'] ["long-g"] "def" False "g description here" (Just GroupInfo
		{ groupInfoName = "group"
		, groupInfoDescription = "Grouped options"
		, groupInfoHelpDescription = "Show grouped options."
		})
	]
	[ ("cmd1",
		[ OptionInfo "test.cmd1.z" ['z'] ["long-z"] "def" False "z description here" Nothing
		])
	, ("cmd2",
		[ OptionInfo "test.cmd2.y" ['y'] ["long-y"] "def" False "y description here" Nothing
		, OptionInfo "test.cmd2.g2" [] ["long-g2"] "def" False "g2 description here" (Just GroupInfo
			{ groupInfoName = "group"
			, groupInfoDescription = "Grouped options"
			, groupInfoHelpDescription = "Show grouped options."
			})
		])
	]

test_ShowHelpSummary :: Suite
test_ShowHelpSummary = assertions "showHelpSummary" $ do
	let expected = "\
	\Help Options:\n\
	\  -h, --help                  Show option summary.\n\
	\  --help-all                  Show all help options.\n\
	\  --help-group                Show grouped options.\n\
	\\n\
	\Application Options:\n\
	\  -a, --long-a                a description here\n\
	\  --a-looooooooooooong-option description here\n\
	\  --a-loooooooooooooong-option\n\
	\    description here\n\
	\  -b, --long-b                b description here\n\
	\\n\
	\Subcommands:\n\
	\  cmd1\n\
	\  cmd2\n\
	\\n"
	$expect (equalLines expected (helpFor HelpSummary variedOptions Nothing))

test_ShowHelpSummary_Subcommand :: Suite
test_ShowHelpSummary_Subcommand = assertions "showHelpSummary-subcommand" $ do
	let expected = "\
	\Help Options:\n\
	\  -h, --help                  Show option summary.\n\
	\  --help-all                  Show all help options.\n\
	\  --help-group                Show grouped options.\n\
	\\n\
	\Application Options:\n\
	\  -a, --long-a                a description here\n\
	\  --a-looooooooooooong-option description here\n\
	\  --a-loooooooooooooong-option\n\
	\    description here\n\
	\  -b, --long-b                b description here\n\
	\\n\
	\Options for subcommand \"cmd1\":\n\
	\  -z, --long-z                z description here\n\
	\\n"
	$expect (equalLines expected (helpFor HelpSummary variedOptions (Just "cmd1")))

test_ShowHelpAll :: Suite
test_ShowHelpAll = assertions "showHelpAll" $ do
	let expected = "\
	\Help Options:\n\
	\  -h, --help                  Show option summary.\n\
	\  --help-all                  Show all help options.\n\
	\  --help-group                Show grouped options.\n\
	\\n\
	\Grouped options:\n\
	\  -g, --long-g                g description here\n\
	\\n\
	\Application Options:\n\
	\  -a, --long-a                a description here\n\
	\  --a-looooooooooooong-option description here\n\
	\  --a-loooooooooooooong-option\n\
	\    description here\n\
	\  -b, --long-b                b description here\n\
	\\n\
	\Options for subcommand \"cmd1\":\n\
	\  -z, --long-z                z description here\n\
	\\n\
	\Options for subcommand \"cmd2\":\n\
	\  -y, --long-y                y description here\n\
	\  --long-g2                   g2 description here\n\
	\\n"
	$expect (equalLines expected (helpFor HelpAll variedOptions Nothing))

test_ShowHelpAll_Subcommand :: Suite
test_ShowHelpAll_Subcommand = assertions "showHelpAll-subcommand" $ do
	let expected = "\
	\Help Options:\n\
	\  -h, --help                  Show option summary.\n\
	\  --help-all                  Show all help options.\n\
	\  --help-group                Show grouped options.\n\
	\\n\
	\Grouped options:\n\
	\  -g, --long-g                g description here\n\
	\\n\
	\Application Options:\n\
	\  -a, --long-a                a description here\n\
	\  --a-looooooooooooong-option description here\n\
	\  --a-loooooooooooooong-option\n\
	\    description here\n\
	\  -b, --long-b                b description here\n\
	\\n\
	\Options for subcommand \"cmd1\":\n\
	\  -z, --long-z                z description here\n\
	\\n"
	$expect (equalLines expected (helpFor HelpAll variedOptions (Just "cmd1")))

test_ShowHelpGroup :: Suite
test_ShowHelpGroup = assertions "showHelpGroup" $ do
	let expected = "\
	\Grouped options:\n\
	\  -g, --long-g                g description here\n\
	\\n"
	$expect (equalLines expected (helpFor (HelpGroup "group") variedOptions Nothing))

test_ShowHelpGroup_Subcommand :: Suite
test_ShowHelpGroup_Subcommand = assertions "showHelpGroup-subcommand" $ do
	let expected = "\
	\Grouped options:\n\
	\  -g, --long-g                g description here\n\
	\  --long-g2                   g2 description here\n\
	\\n"
	$expect (equalLines expected (helpFor (HelpGroup "group") variedOptions (Just "cmd2")))

test_ShowHelpGroup_SubcommandInvalid :: Suite
test_ShowHelpGroup_SubcommandInvalid = assertions "showHelpGroup-subcommand-invalid" $ do
	let expected = "\
	\Grouped options:\n\
	\  -g, --long-g                g description here\n\
	\\n"
	$expect (equalLines expected (helpFor (HelpGroup "group") variedOptions (Just "noexist")))
