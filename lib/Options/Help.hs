-- |
-- Module: Options.Help
-- License: MIT
--
-- TODO: documentation here
module Options.Help
	( addHelpFlags
	, helpFor
	, HelpFlag(..)
	) where

import           Options.Types

data HelpFlag = HelpSummary | HelpAll | HelpGroup String

addHelpFlags :: OptionDefinitions a -> OptionDefinitions a
addHelpFlags = undefined
	-- add flags for --help, --help-<group>, etc.
	-- if any flag already exists in the definitions, it is not replaced.

helpFor :: HelpFlag -> OptionDefinitions a -> Maybe String -> String
helpFor flag defs maybeSubcommand = case (flag, maybeSubcommand) of
	(HelpSummary, Nothing) -> -- show all global options
	                          -- show summaries of option groups
	                          -- show summaries of all subcommands
		undefined
	(HelpSummary, Just subcmd) -> -- show all global options
	                              -- show summaries of all global option groups
	                              -- show all options in subcommand 'subcmd'
	                              -- show summaries of all option groups for subcommand 'subcmd'
		undefined
	(HelpAll, Nothing) -> -- show all global options
	                      -- show all options in all global option groups
	                      -- show all options in all subcommands
	                      -- show all options in all subcommand option groups
		undefined
	(HelpAll, Just subcmd) -> -- show all global options
	                          -- show all options in all global option groups
	                          -- show all options in subcommand 'subcmd'
	                          -- show all options in option groups for subcommand 'subcmd'
		undefined
	(HelpGroup groupName, Nothing) -> -- show all options in group 'groupName', which must not be in a subcommand
		undefined
	(HelpGroup groupName, Just subcmd) -> -- show all options in group 'groupName', which may be either global or in the subcommand 'subcmd'
		undefined
