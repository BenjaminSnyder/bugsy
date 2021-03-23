(* Ocamllex scanner for bugsy *)

{ open Parser }

let digit = ['0'-'9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment 0 lexbuf }           (* Comments *)
| "//"     { linecomment lexbuf }       (* single line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACKET }
| ']'      { RSQBRACKET }
| ':'      { COLON  }
| ';'      { SEMI   }
| ','      { COMMA  }
| '?'      { QMARK  }
| '+'      { PLUS   }
| "+="     { PLUSEQ }
| '-'      { MINUS  }
| "-="     { MINUSEQ    }
| "++"     { INCREMENT  }
| "--"     { DECREMENT  }
| '*'      { MULT  }
| '/'      { DIV   }
| "*="     { MULTEQ     }
| "/="     { DIVEQ      }
| '%'      { MODULO }
| '='      { ASSIGN }
| "=?"     { EQ     }
| "!="     { NEQ    }
| '<'      { LT     }
| "<="     { LEQ    }
| ">"      { GT     }
| ">="     { GEQ    }
| "and"    { AND    }
| "or"     { OR     }
| "!"      { NOT    }
| "if"     { IF     }
| "else"   { ELSE   }
| "elif"   { ELIF   }
| "for"    { FOR    }
| "while"  { WHILE  }
| "return" { RETURN }
| "continue" { CONTINUE }
| "break"  { BREAK  }
| "num"    { NUM    }
| "bool"   { BOOL   }
| "void"   { VOID   }
| "class"  { CLASS  }
| "constructor" { CONSTRUCTOR }
| "null"   { NULL   }
| "try"    { TRY    }
| "catch"  { CATCH  }
| "raise"  { RAISE  }
| "string" { STRING }
(*| "char"   { CHAR   }*)
| "True" |"true"   { BLIT(true)  }
| "False"|"false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { NLIT(lxm) }
| ['a'-'z' '_' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '\"' ['a'-'z' 'A'-'Z' '0'-'9'
   '_' '+' '-' '!' '@' '#' 
   '$' '%' '^' '&' '*'
   '(' ')' '{' '}' '[' ']' 
   ';' ':' '.' ',' '<' '>'
   '/' '?' '|' '\\' '`' '~' ]* '\"' as lxm { STRLIT(lxm) }

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
  "\n"  { token lexbuf   }
| _     { linecomment lexbuf }
