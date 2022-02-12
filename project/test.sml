(* Infrastructure to run the Plc interpreter *)
(* TODO: chamar run ao invés de teval/eval  *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";


use "testPlcCases.sml";

fun testCases (sourceCode:string, expected:string) : string = 
  let
    val expr = fromString(sourceCode);
    val observed = run(expr)
  in
    if (observed = expected) then "V" else sourceCode
  end

val results = map (fn (s,e) => testCases(s, e)) cases;

fun isAllTestsPassed [] = true
  | isAllTestsPassed (h::t) = (h="V") andalso (isAllTestsPassed t);

val isPlcCorrect = isAllTestsPassed(results);

(* val abs = fromFile "tests/t23.plc"; *)
(* val venv = [];
val v = teval abs venv *)

(* TODO deletar o código abaixo. Pode ser útil para debugar então deixei comentado por enquanto. *)

(* val venv = [];
val abs = fromFile "tests/t26.plc"
val omg =
let
    val k = teval abs venv
in
    abs
end
handle Debug e => e; *)
