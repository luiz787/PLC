(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Name to token *)

fun selectToken (s, lpos, rpos) = 
	case s of
		  "Bool" => BOOLTYPE(lpos, rpos)
		| "else" => ELSE(lpos, rpos)
		| "end" => END(lpos, rpos)
		| "false" => FALSE(lpos, rpos)
		| "fn" => FN(lpos, rpos)
		| "fun" => FUN(lpos, rpos)
		| "hd" => HD(lpos, rpos)
		| "if" => IF(lpos, rpos)
		| "Int" => INTTYPE(lpos, rpos)
		| "ise" => ISE(lpos, rpos)
		| "Nil" => NILTYPE(lpos, rpos)
		| "print" => PRINT(lpos, rpos)
		| "rec" => REC(lpos, rpos)
		| "then" => THEN(lpos, rpos)
		| "tl" => TL(lpos, rpos)
		| "true" => TRUE(lpos, rpos)
		| "var" => VAR(lpos, rpos)
		| "with" => WITH(lpos, rpos)
		| "_" => UNDERSCORE(lpos, rpos)
		|  _  => NAME(s, lpos, rpos)




(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

%%

digit=[0-9];
ws = [\ \t];
name = [A-z_][A-z_0-9]*

%%
\n       => (lineNumber := !lineNumber + 1; lex());
{name}   => (selectToken (yytex, yypos, yypos))
{ws}+    => (lex());
{digit}+ => (CINT(valOf (Int.fromString yytext), yypos, yypos));
"="      => (EQ(yypos,yypos));
"!="     => (NEQ(yypos,yypos));
"<"      => (LT(yypos,yypos));
"<="     => (LTE(yypos,yypos));
","      => (COMMA(yypos,yypos));
":"      => (COLON(yypos,yypos));
";"      => (SEMICOLON(yypos,yypos));
"::"     => (DOUBLECOLON(yypos,yypos));
"&&"	 => (AND(yypos, yypos));
"!"      => (EXCL(yypos, yypos));
"+"      => (PLUS(yypos,yypos));
"*"      => (TIMES(yypos,yypos));
"-"      => (MINUS(yypos,yypos));
"/"      => (DIV(yypos,yypos));
"("      => (LPAREN(yypos,yypos));
")"      => (RPAREN(yypos,yypos));
"["      => (LSQBRACKET(yypos,yypos));
"]"      => (RSQBRACKET(yypos,yypos));
"{"      => (LBRACE(yypos,yypos));
"}"      => (RBRACE(yypos,yypos));
"|"      => (PIPE(yypos, yypos));
"->"     => (ARROW(yypos, yypos));
"=>"     => (FATARROW(yypos, yypos));
"."      => (error("\n***Lexer error: invalid character ***\n"); 
raise Fail("Lexer error: invalid character " ^ yytex));
