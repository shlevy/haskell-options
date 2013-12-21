-- |
-- Module: Options.Types
-- License: MIT
module Options.Types
	( OptionDefinitions(..)
	, OptionType(..)
	, Group(..)
	, OptionKey(..)
	, Location(..)
	, OptionInfo(..)
	, Token(..)
	, tokenFlagName
	, Tokens(..)
	) where

import qualified Data.Map as Map
import           Language.Haskell.TH (Exp, Q, Type)

data OptionDefinitions a = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

-- | An option's type determines how the option will be parsed, and which
-- Haskell type the parsed value will be stored as. There are many types
-- available, covering most basic types and a few more advanced types.
data OptionType val = OptionType
	{ optionTypeTemplateType :: Type
	, optionTypeUnary :: Bool
	, optionTypeParse :: String -> Either String val
	, optionTypeTemplateParse :: Q Exp
	, optionTypeName :: String
	}

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
