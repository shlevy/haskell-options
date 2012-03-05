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
import           Data.List (intercalate)
import qualified Data.Set as Set

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
	
	-- TODO: option groups
	withHelp = optHelpSummary ++ optHelpAll ++ opts
	
	optHelpSummary = if Set.member 'h' shortFlags
		then if Set.member "help" longFlags
			then []
			else [OptionInfo
				{ optionInfoKey = keyFor "optHelpSummary"
				, optionInfoShortFlags = []
				, optionInfoLongFlags = ["help"]
				, optionInfoDefault = "false"
				, optionInfoUnary = True
				, optionInfoDescription = "Show help options."
				}]
		else if Set.member "help" longFlags
			then [OptionInfo
				{ optionInfoKey = keyFor "optHelpSummary"
				, optionInfoShortFlags = ['h']
				, optionInfoLongFlags = []
				, optionInfoDefault = "false"
				, optionInfoUnary = True
				, optionInfoDescription = "Show help options."
				}]
			else [OptionInfo
				{ optionInfoKey = keyFor "optHelpSummary"
				, optionInfoShortFlags = ['h']
				, optionInfoLongFlags = ["help"]
				, optionInfoDefault = "false"
				, optionInfoUnary = True
				, optionInfoDescription = "Show help options."
				}]
	
	optHelpAll = if Set.member "help-all" longFlags
		then []
		else [OptionInfo
			{ optionInfoKey = keyFor "optHelpAll"
			, optionInfoShortFlags = []
			, optionInfoLongFlags = ["help-all"]
			, optionInfoDefault = "false"
			, optionInfoUnary = True
			, optionInfoDescription = "Show all help options."
			}]

checkHelpFlag :: TokensFor a -> Maybe HelpFlag
checkHelpFlag (TokensFor tokens _) = case lookup (keyFor "optHelpSummary") tokens of
	Nothing -> case lookup (keyFor "optHelpAll") tokens of
		Nothing -> Nothing
		Just _ -> Just HelpAll
	Just _ -> Just HelpSummary

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
		undefined
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
	{-
	hPutStrLn h "Help Options:"
	hPutStrLn h "  -h, --help             Show help options"
	hPutStrLn h "  --help-all             Show all help options"
	forM_ subGroups $ \(GroupInfo (Just slug) doc _) -> do
		hPutStrLn h ("  --help-" ++ slug ++ "             " ++ (helpSummary doc))
	
	hPutStrLn h ""
	-}
	
	-- hPutStrLn h (helpSummary mainDoc ++ ":")
	-- hPutStrLn h (helpDescription mainDoc ++ ":")
	forM_ opts showOptionHelp
	tell "\n"

showHelpAll :: OptionDefinitions a -> Writer String ()
showHelpAll (OptionDefinitions opts subcmds) = do
	{-
	hPutStrLn h "Help Options:"
	hPutStrLn h "  -h, --help             Show help options"
	hPutStrLn h "  --help-all             Show all help options"
	forM_ subGroups $ \(GroupInfo (Just slug) doc _) -> do
		hPutStrLn h ("  --help-" ++ slug ++ "             " ++ (helpSummary doc))
	hPutStrLn h ""
	-}
	
	{-
	forM_ subGroups $ \(GroupInfo _ doc opts) -> do
		hPutStrLn h (helpSummary doc ++ ":")
		hPutStrLn h (helpDescription doc) -- TODO: indent
		hPutStrLn h ""
		forM_ opts (showOptionHelp h)
		hPutStrLn h ""
	-}
	
	-- hPutStrLn h (helpSummary mainDoc ++ ":")
	-- hPutStrLn h (helpDescription mainDoc ++ ":")
	forM_ opts showOptionHelp
	tell "\n"

{-
showHelpGroup :: Handle -> GroupInfo -> IO ()
showHelpGroup h (GroupInfo _ doc opts) = do
	hPutStrLn h (helpSummary doc ++ ":")
	hPutStrLn h (helpDescription doc) -- TODO: indent
	hPutStrLn h ""
	forM_ opts (showOptionHelp h)
	hPutStrLn h ""
-}

keyFor :: String -> String
keyFor fieldName = this_pkg ++ ":" ++ this_mod ++ ":" ++ fieldName where
	(this_pkg, this_mod) = $(do
		loc <- location
		let pkg = loc_package loc
		let mod' = loc_module loc
		[| (pkg, mod') |])
