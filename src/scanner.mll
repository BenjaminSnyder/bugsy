(* Ocamllex scanner for bugsy *)

{ open Parser }

let digit = ['0'-'9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment 0 lexbuf }           (* Comments *)
| "//"     { linecomment lexbuf }       (* single line comments *)

(* Symbols *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACKET }
| ']'      { RSQBRACKET }
| ':'      { COLON  }
| ';'      { SEMI   }
| ','      { COMMA  }
| '.'      { DOT    }
| '?'      { QMARK  }

(* Arithmetic Tokens *)
| '+'      { PLUS   }
| "+="     { PLUSEQ }
| '-'      { MINUS  }
| "-="     { MINUSEQ    }
| '*'      { MULT  }
| '/'      { DIV   }
| "*="     { MULTEQ     }
| "/="     { DIVEQ      }
| '%'      { MODULO }
| "++"     { INCREMENT  }
| "--"     { DECREMENT  }
| '='      { ASSIGN }

(* Boolean Expression Tokens *)
| "=?"     { EQ     }
| "!="     { NEQ    }
| '<'      { LT     }
| "<="     { LEQ    }
| ">"      { GT     }
| ">="     { GEQ    }
| "and"    { AND    }
| "or"     { OR     }
| "!"      { NOT    }

(* Control Flow *)
| "if"     { IF     }
| "else"   { ELSE   }
| "elif"   { ELIF   }
| "for"    { FOR    }
| "while"  { WHILE  }
| "return" { RETURN }
| "continue" { CONTINUE }
| "break"  { BREAK  }

(* Classes *)
| "class"  { CLASS  }
| "constructor" { CONSTRUCTOR }
| "new"  { NEW }

(* Errors *)
| "try"    { TRY    }
| "catch"  { CATCH  }
| "raise"  { RAISE  }

(* Builtin Types *)
| "null"   { NULL   }
| "num"    { NUM    }
| "bool"   { BOOL   }
| "void"   { VOID   }
| "string" { STRING }
| "True" |"true"   { BLIT(true)  }
| "False"|"false"  { BLIT(false) }

(* Numeric Literal *)
| digits '.'?  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { NUMLIT(lxm) }
| ['a'-'z' '_' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
(* String Literal *)
|'\"' ([^ '\"' '\n' '\r' '\t' '\b']* as lxm)  '\"' { STRLIT(lxm) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment nest_level = parse
  "*/"  { match nest_level with
            0 -> token lexbuf
          | _ -> comment (nest_level - 1) lexbuf
        }
| "/*"  { comment (nest_level + 1) lexbuf }
| _     { comment nest_level lexbuf }

and linecomment = parse
  "\n"  { token lexbuf }
| eof   { token lexbuf }
| _     { linecomment lexbuf }
