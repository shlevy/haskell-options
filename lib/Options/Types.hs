-- |
-- Module: Options.Types
-- License: MIT
module Options.Types
	( OptionDefinitions(..)
	
	, OptionType
	, optionType
	, optionTypeName
	, optionTypeDefault
	, optionTypeParse
	, optionTypeShow
	, optionTypeUnary
	
	, Group(..)
	, OptionKey(..)
	, Location(..)
	, OptionInfo(..)
	, Token(..)
	, tokenFlagName
	, Tokens(..)
	) where

import qualified Data.Map as Map

data OptionDefinitions = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

-- | An option's type determines how the option will be parsed, and which
-- Haskell type the parsed value will be stored as. There are many types
-- available, covering most basic types and a few more advanced types.
data OptionType val = OptionType
	{ optionTypeName_ :: String
	, optionTypeDefault_ :: val
	, optionTypeParse_ :: String -> Either String val
	, optionTypeShow_ :: val -> String

	-- | TODO docs
	, optionTypeUnary :: Maybe val
	}

-- | TODO docs
optionType :: String -> val -> (String -> Either String val) -> (val -> String) -> OptionType val
optionType name def parse show' = OptionType name def parse show' Nothing

-- | TODO docs
optionTypeName :: OptionType val -> String
optionTypeName = optionTypeName_

-- | TODO docs
optionTypeDefault :: OptionType val -> val
optionTypeDefault = optionTypeDefault_

-- | TODO docs
optionTypeParse :: OptionType val -> String -> Either String val
optionTypeParse = optionTypeParse_

-- | TODO docs
optionTypeShow :: OptionType val -> val -> String
optionTypeShow = optionTypeShow_

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
	| OptionKeyGenerated Integer
	deriving (Eq, Ord, Show)

data Location = Location
	{ locationPackage :: String
	, locationModule :: String
	, locationFilename :: String
	, locationLine :: Integer
	}
	deriving (Eq, Show)

data OptionInfo = OptionInfo
	{ optionInfoKey :: OptionKey
	, optionInfoShortFlags :: [Char]
	, optionInfoLongFlags :: [String]
	, optionInfoDefault :: String
	, optionInfoUnary :: Bool
	, optionInfoDescription :: String
	, optionInfoGroup :: Maybe Group
	, optionInfoLocation :: Maybe Location
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
