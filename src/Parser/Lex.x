{
{-# OPTIONS_GHC -w #-}
module Parser.Lex
  ( alexScanTokens
  ) where

import Parser.Tokens

}

%wrapper "posn"

@identifier = [a-zA-Z_] [a-zA-Z0-9_]*
@bits       = [01\-]+

tokens :-

  "case"   { tok KW_case  }
  "if"     { tok KW_if    }
  "else"   { tok KW_else  }
  "do"     { tok KW_do    }
  "of"     { tok KW_of    }
  "where"  { tok KW_where }
  "()"     { tok Unit     }
  "("      { tok ParenL   }
  ")"      { tok ParenR   }

  @identifier    { tok Id          }
  $white         ;
  .              { tok Unknown     }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c
}

