-- |
-- Module: Options.Types
-- License: MIT
module Options.Types
	( OptionDefinitions(..)
	, Group(..)
	, OptionKey(..)
	, OptionInfo(..)
	, Token(..)
	, tokenFlagName
	, Tokens(..)
	) where

import qualified Data.Map as Map

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data Group = Group
	{
	  groupName :: String
	
	-- | A short title for the group, which is used when printing
	-- @--help@ output.
	, groupTitle :: String
	
	-- | A description of the group, which is used when printing
	-- @--help@ output.
	, groupDescription :: String
	}
	deriving (Eq, Show)

data OptionKey
	= OptionKey String
	| OptionKeyHelpSummary
	| OptionKeyHelpGroup String
	deriving (Eq, Ord, Show)

data OptionInfo = OptionInfo
	{ optionInfoKey :: OptionKey
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	, optionInfoDescription :: String
	, optionInfoGroup :: Maybe Group
	, optionInfoTypeName :: String
	}
	deriving (Eq, Show)

data Token
	= TokenUnary String -- flag name
	| Token String String -- flag name, flag value
	deriving (Eq, Show)

tokenFlagName :: Token -> String
tokenFlagName (TokenUnary s) = s
tokenFlagName (Token s _) = s

data Tokens = Tokens
	{ tokensMap :: Map.Map OptionKey Token
	, tokensArgv :: [String]
	}
	deriving (Show)
