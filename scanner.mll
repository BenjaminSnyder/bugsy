(* Ocamllex scanner for bugsy *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { singcomment lexbuf }       (* single line comments *)
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
| "char"   { CHAR   }
| "True" |"true"   { TRUE  }
| "False"|"false"  { FALSE }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' '_' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singcomment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
