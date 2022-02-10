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

val abs = fromFile "tests/t20.plc";
(* val venv = [];
val v = teval abs venv *)

run abs

(* TODO deletar o código abaixo. Pode ser útil para debugar então deixei comentado por enquanto. *)

(*
val omg =
let
    val k = teval abs []
in
    abs
end
handle Debug e => e;
*)
