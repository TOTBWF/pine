module Pine.Parser.Stack
    (Parser)
where

import qualified Text.Megaparsec as P 
import Data.Void (Void)
import Data.Text (Text)

type Parser = P.Parsec Void Text