{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Options.Help
-- License: MIT
--
-- TODO: documentation here
module Options.Help
	( addHelpFlags
	, checkHelpFlag
	, helpFor
	, HelpFlag(..)
	) where

import           Control.Monad.Writer
import           Data.List (intercalate, partition, stripPrefix)
import           Data.Maybe (isNothing, listToMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Map as Map

import           Language.Haskell.TH (location, loc_package, loc_module)

import           Options.Types

data HelpFlag = HelpSummary | HelpAll | HelpGroup String

addHelpFlags :: OptionDefinitions a -> OptionDefinitions a
addHelpFlags (OptionDefinitions opts subcmds) = OptionDefinitions withHelp subcmds where
	shortFlags = Set.fromList $ do
		opt <- opts
		optionInfoShortFlags opt
	longFlags = Set.fromList $ do
		opt <- opts
		optionInfoLongFlags opt
	
	withHelp = optHelpSummary ++ optsGroupHelp ++ opts
	
	groupHelp = GroupInfo
		{ groupInfoName = "all"
		, groupInfoDescription = "Help Options"
		, groupInfoHelpDescription = "Show all help options."
		}
	
	optSummary = OptionInfo
		{ optionInfoKey = keyFor "optHelpSummary"
		, optionInfoShortFlags = []
		, optionInfoLongFlags = []
		, optionInfoDefault = "false"
		, optionInfoUnary = True
		, optionInfoDescription = "Show option summary."
		, optionInfoGroup = Just groupHelp
		}
	
	optHelpSummary = if Set.member 'h' shortFlags
		then if Set.member "help" longFlags
			then []
			else [optSummary
				{ optionInfoLongFlags = ["help"]
				}]
		else if Set.member "help" longFlags
			then [optSummary
				{ optionInfoShortFlags = ['h']
				}]
			else [optSummary
				{ optionInfoShortFlags = ['h']
				, optionInfoLongFlags = ["help"]
				}]
	
	optsGroupHelp = do
		let (groupsAndOpts, _) = uniqueGroupInfos opts
		let groups = [g | (g, _) <- groupsAndOpts]
		group <- (groupHelp : groups)
		let flag = "help-" ++ groupInfoName group
		if Set.member flag longFlags
			then []
			else [OptionInfo
				{ optionInfoKey = keyFor "optHelpGroup" ++ ":" ++ groupInfoName group
				, optionInfoShortFlags = []
				, optionInfoLongFlags = [flag]
				, optionInfoDefault = "false"
				, optionInfoUnary = True
				, optionInfoDescription = groupInfoHelpDescription group
				, optionInfoGroup = Just groupHelp
				}]

checkHelpFlag :: TokensFor a -> Maybe HelpFlag
checkHelpFlag (TokensFor tokens _) = flag where
	flag = listToMaybe helpKeys
	helpKeys = do
		(k, _) <- tokens
		if k == keySummary
			then return HelpSummary
			else if k == keyAll
				then return HelpAll
				else do
					groupName <- maybeToList (stripPrefix keyGroupPrefix k)
					return (HelpGroup groupName)
	keySummary = keyFor "optHelpSummary"
	keyAll = keyFor "optHelpGroup:all"
	keyGroupPrefix = keyFor "optHelpGroup:"

helpFor :: HelpFlag -> OptionDefinitions a -> Maybe String -> String
helpFor flag defs maybeSubcommand = case (flag, maybeSubcommand) of
	(HelpSummary, Nothing) -> -- show all global options
	                          -- show summaries of option groups
	                          -- show summaries of all subcommands
		execWriter (showHelpSummary defs)
	(HelpSummary, Just subcmd) -> -- show all global options
	                              -- show summaries of all global option groups
	                              -- show all options in subcommand 'subcmd'
	                              -- show summaries of all option groups for subcommand 'subcmd'
		undefined
	(HelpAll, Nothing) -> -- show all global options
	                      -- show all options in all global option groups
	                      -- show all options in all subcommands
	                      -- show all options in all subcommand option groups
		execWriter (showHelpAll defs)
	(HelpAll, Just subcmd) -> -- show all global options
	                          -- show all options in all global option groups
	                          -- show all options in subcommand 'subcmd'
	                          -- show all options in option groups for subcommand 'subcmd'
		undefined
	(HelpGroup groupName, Nothing) -> -- show all options in group 'groupName', which must not be in a subcommand
		execWriter (showHelpOneGroup defs groupName)
	(HelpGroup groupName, Just subcmd) -> -- show all options in group 'groupName', which may be either global or in the subcommand 'subcmd'
		undefined

showOptionHelp :: OptionInfo -> Writer String ()
showOptionHelp info = do
	let safeHead xs = case xs of
		[] -> []
		(x:_) -> [x]
	let shorts = optionInfoShortFlags info
	let longs = optionInfoLongFlags info
	let optStrings = map (\x -> ['-', x]) (safeHead shorts) ++ map (\x -> "--" ++ x) (safeHead longs)
	unless (null optStrings) $ do
		let optStringCsv = intercalate ", " optStrings
		tell "  "
		tell optStringCsv
		
		let desc = optionInfoDescription info
		unless (null desc) $ do
			if length optStringCsv > 27
				then do
					tell "\n"
					tell "    "
					tell (optionInfoDescription info)
					tell "\n"
				else do
					tell (replicate (28 - length optStringCsv) ' ')
					tell (optionInfoDescription info)
		
		tell "\n"

showHelpSummary :: OptionDefinitions a -> Writer String ()
showHelpSummary (OptionDefinitions opts subcmds) = do
	let (groupInfos, ungroupedOptions) = uniqueGroupInfos opts
	
	-- Always print --help group
	let hasHelp = filter (\(g,_) -> groupInfoName g == "all") groupInfos
	forM_ hasHelp showHelpGroup
	
	tell "Application Options:\n"
	forM_ ungroupedOptions showOptionHelp

showHelpAll :: OptionDefinitions a -> Writer String ()
showHelpAll (OptionDefinitions opts subcmds) = do
	let (groupInfos, ungroupedOptions) = uniqueGroupInfos opts
	
	-- Always print --help group first, if present
	let (hasHelp, noHelp) = partition (\(g,_) -> groupInfoName g == "all") groupInfos
	forM_ hasHelp showHelpGroup
	forM_ noHelp showHelpGroup
	
	tell "Application Options:\n"
	forM_ ungroupedOptions showOptionHelp

showHelpGroup :: (GroupInfo, [OptionInfo]) -> Writer String ()
showHelpGroup (groupInfo, opts) = do
	tell (groupInfoDescription groupInfo ++ ":\n")
	forM_ opts showOptionHelp
	tell "\n"

showHelpOneGroup :: OptionDefinitions a -> String -> Writer String ()
showHelpOneGroup (OptionDefinitions opts subcmds) groupName = do
	let (groupInfos, _) = uniqueGroupInfos opts
	
	-- Always print --help group
	let group = filter (\(g,_) -> groupInfoName g == groupName) groupInfos
	forM_ group showHelpGroup

keyFor :: String -> String
keyFor fieldName = this_pkg ++ ":" ++ this_mod ++ ":" ++ fieldName where
	(this_pkg, this_mod) = $(do
		loc <- location
		let pkg = loc_package loc
		let mod' = loc_module loc
		[| (pkg, mod') |])

uniqueGroupInfos :: [OptionInfo] -> ([(GroupInfo, [OptionInfo])], [OptionInfo])
uniqueGroupInfos allOptions = (Map.elems infoMap, ungroupedOptions) where
	infoMap = Map.fromListWith merge $ do
		opt <- allOptions
		case optionInfoGroup opt of
			Nothing -> []
			Just g -> [(groupInfoName g, (g, [opt]))]
	merge (g, opts1) (_, opts2) = (g, opts2 ++ opts1)
	ungroupedOptions = [o | o <- allOptions, isNothing (optionInfoGroup o)]
