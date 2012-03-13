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
	deriving (Eq, Show)

addHelpFlags :: OptionDefinitions a -> OptionDefinitions a
addHelpFlags (OptionDefinitions opts subcmds) = OptionDefinitions withHelp subcmdsWithHelp where
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
	
	optGroupHelp group flag = OptionInfo
		{ optionInfoKey = keyFor "optHelpGroup" ++ ":" ++ groupInfoName group
		, optionInfoShortFlags = []
		, optionInfoLongFlags = [flag]
		, optionInfoDefault = "false"
		, optionInfoUnary = True
		, optionInfoDescription = groupInfoHelpDescription group
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
			else [optGroupHelp group flag]
	
	subcmdsWithHelp = do
		(subcmdName, subcmdOpts) <- subcmds
		let subcmdLongFlags = Set.fromList $ do
			opt <- subcmdOpts ++ optsGroupHelp
			optionInfoLongFlags opt
		
		let (groupsAndOpts, _) = uniqueGroupInfos subcmdOpts
		let groups = [g | (g, _) <- groupsAndOpts]
		let newOpts = do
			group <- groups
			let flag = "help-" ++ groupInfoName group
			if Set.member flag (Set.union longFlags subcmdLongFlags)
				then []
				else [optGroupHelp group flag]
		return (subcmdName, newOpts ++ subcmdOpts)

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
helpFor flag defs subcmd = case flag of
	HelpSummary -> execWriter (showHelpSummary defs subcmd)
	HelpAll -> execWriter (showHelpAll defs subcmd)
	HelpGroup groupName -> execWriter (showHelpOneGroup defs groupName subcmd)

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
				else do
					tell (replicate (28 - length optStringCsv) ' ')
					tell (optionInfoDescription info)
		
		tell "\n"

showHelpSummary :: OptionDefinitions a -> Maybe String -> Writer String ()
showHelpSummary (OptionDefinitions mainOpts subcmds) subcmd = do
	let subcmdOptions = do
		subcmdName <- subcmd
		opts <- lookup subcmdName subcmds
		return (subcmdName, opts)
	
	let (groupInfos, ungroupedMainOptions) = uniqueGroupInfos mainOpts
	
	-- Always print --help group
	let hasHelp = filter (\(g,_) -> groupInfoName g == "all") groupInfos
	forM_ hasHelp showHelpGroup
	
	tell "Application Options:\n"
	forM_ ungroupedMainOptions showOptionHelp
	unless (null subcmds) (tell "\n")
	
	case subcmdOptions of
		Nothing -> unless (null subcmds) $ do
			tell "Subcommands:\n"
			forM_ subcmds $ \(subcmdName, _) -> do
				tell "  "
				tell subcmdName
				-- TODO: subcommand help description
				tell "\n"
			tell "\n"
		Just (n, subOpts) -> do
			-- TODO: subcommand description
			-- TODO: handle grouped options in subcommands?
			tell ("Options for subcommand " ++ show n ++ ":\n")
			forM_ subOpts showOptionHelp
			tell "\n"

showHelpAll :: OptionDefinitions a -> Maybe String -> Writer String ()
showHelpAll (OptionDefinitions mainOpts subcmds) subcmd = do
	let subcmdOptions = do
		subcmdName <- subcmd
		opts <- lookup subcmdName subcmds
		return (subcmdName, opts)
	
	let (groupInfos, ungroupedMainOptions) = uniqueGroupInfos mainOpts
	
	-- Always print --help group first, if present
	let (hasHelp, noHelp) = partition (\(g,_) -> groupInfoName g == "all") groupInfos
	forM_ hasHelp showHelpGroup
	forM_ noHelp showHelpGroup
	
	tell "Application Options:\n"
	forM_ ungroupedMainOptions showOptionHelp
	unless (null subcmds) (tell "\n")
	
	case subcmdOptions of
		Nothing -> forM_ subcmds $ \(subcmdName, subcmdOpts) -> do
			-- no subcommand description
			tell ("Options for subcommand " ++ show subcmdName ++ ":\n")
			forM_ subcmdOpts showOptionHelp
			tell "\n"
		Just (n, subOpts) -> do
			-- TODO: subcommand description
			-- TODO: handle grouped options in subcommands?
			tell ("Options for subcommand " ++ show n ++ ":\n")
			forM_ subOpts showOptionHelp
			tell "\n"

showHelpGroup :: (GroupInfo, [OptionInfo]) -> Writer String ()
showHelpGroup (groupInfo, opts) = do
	tell (groupInfoDescription groupInfo ++ ":\n")
	forM_ opts showOptionHelp
	tell "\n"

showHelpOneGroup :: OptionDefinitions a -> String -> Maybe String -> Writer String ()
showHelpOneGroup (OptionDefinitions mainOpts subcmds) groupName subcmd = do
	let opts = case subcmd of
		Nothing -> mainOpts
		Just n -> case lookup n subcmds of
			Just infos -> mainOpts ++ infos -- both
			Nothing -> mainOpts
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
