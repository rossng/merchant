module ParsecExperiments where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Combinator

--expr    = term   `chainl1` addop
--term    = factor `chainl1` mulop
--factor  = (parens expr) <|> integer

--mulop   =   do{ symbol "*"; return (*)   }
--        <|> do{ symbol "/"; return (div) }
--
--addop   =   do{ symbol "+"; return (+) }
--        <|> do{ symbol "-"; return (-) }