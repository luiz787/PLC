(* Plc interpreter main file

TODO: descomentar linhas 8 e 10 para produzir a saída esperada
TODO: descomentar as linhas 32-36 para as exceções do interpretador (e remover ponto e vírgula da linha 28)
*)

fun run(e: expr) =
    let
        val eType = teval e []
        (* val eValue = eval e [] *)
    in
        (* (val2string eValue) ^ " : " ^ (type2string eType) *)
        type2string eType
    end
    handle EmptySeq => "Plc Checker: Empty sequence should have a sequence type."
        | UnknownType => "Plc Checker: Unknown operator, or type error."
        | NotEqTypes => "Plc Checker: Types in comparison are different or not comparable."
        | WrongRetType => "Plc Checker: declared function return type differs from actual return type."
        | CallTypeMisM => "Plc Checker: Type mismatch in function call."
        | NotFunc => "teval Call: not a function."
        | IfCondNotBool => "Plc Checker: 'if' condition not Boolean."
        | DiffBrTypes => "Plc Checker: 'if' branches types differ."
        | NoMatchResults => "Plc Checker: no Match results."
        | MatchCondTypesDiff => "Plc Checker: at least one 'match' arm condition type is different from matching expressions's type."
        | MatchResTypeDiff => "Plc Checker: 'match' result types differ."
        | ListOutOfRange => "Plc Checker: List index out of range."
        | OpNonList => "Plc Checker: Selection with operator # applied to non-list."
        | SymbolNotFound => "Symbol not found.";
        
        
        
        (*| Impossible => "Plc Interp: Impossible to evaluate expression."
        | HDEmptySeq => "Plc Interp: 'hd' empty sequence argument."
        | TLEmptySeq => "Plc Interp: 'tl' empty sequence argument."
        | ValueNotFoundInMatch => "Plc Interp: value not found in match."
        | NotAFunc => "Plc Interp: eval Call: not a function."; *)
