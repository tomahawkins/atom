{
{-# OPTIONS_GHC -w #-}
module Parser.Lex
  ( alexScanTokens
  ) where

import Common
import Parser.Tokens

}

%wrapper "posn"

@idUpper  = [A-Z] [a-zA-Z0-9_]*
@idLower  = [a-z] [a-zA-Z0-9_]*
@operator = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]+

tokens :-

  "case"       { tok KW_case      }
  "class"      { tok KW_class     }
  "datatype"   { tok KW_datatype  }
  "do"         { tok KW_do        }
  "else"       { tok KW_else      }
  "if"         { tok KW_if        }
  "instance"   { tok KW_instance  }
  "intrinsic"  { tok KW_intrinsic }
  "let"        { tok KW_let       }
  "of"         { tok KW_of        }
  "then"       { tok KW_then      }
  "where"      { tok KW_where     }

  "()"         { tok Unit         }
  "("          { tok ParenL       }
  ")"          { tok ParenR       }
  "="          { tok Equal        }
  "::"         { tok ColonColon   }
  ";"          { tok Semi         }
  --"`"          { tok Tic          }
  "|"          { tok Pipe         }
  "\"          { tok Backslash    }
  "_"          { tok Underscore   }
  "@"          { tok At           }

  @idUpper       { tok IdUpper  }
  @idLower       { tok IdLower  }
  @operator      { tok InfixL9  }

  $white         ;
  .              { tok Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Location "" l c
}

