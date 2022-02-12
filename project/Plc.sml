(* Plc interpreter main file *)

fun run(e: expr) =
    let
        val eType = teval e []
        val eValue = eval e []
    in
        (val2string eValue) ^ " : " ^ (type2string eType)
    end
    handle EmptySeq => "Plc Checker: Sequence operation is not possible on an empty sequence."
        | UnknownType => "Plc Checker: Unknown operator or type error."
        | NotEqTypes => "Plc Checker: Types in comparison are different or not comparable."
        | WrongRetType => "Plc Checker: declared function return type differs from actual return type."
        | CallTypeMisM => "Plc Checker: Type mismatch in function call."
        | NotFunc => "Plc Checker: Trying to call an expression that is not a function."
        | IfCondNotBool => "Plc Checker: expression in 'if' condition is not boolean."
        | DiffBrTypes => "Plc Checker: 'if' branches types differ."
        | NoMatchResults => "Plc Checker: no Match results."
        | MatchCondTypesDiff => "Plc Checker: at least one 'match' arm condition type is different from matching expressions's type."
        | MatchResTypeDiff => "Plc Checker: 'match' result types differ."
        | ListOutOfRange => "Plc Checker: List index out of range."
        | OpNonList => "Plc Checker: Selection with operator [] applied to non-list."
        | SymbolNotFound => "Symbol not found."
        | Impossible => "Plc Interp: Impossible to evaluate expression."
        | HDEmptySeq => "Plc Interp: 'hd' empty sequence argument."
        | TLEmptySeq => "Plc Interp: 'tl' empty sequence argument."
        | ValueNotFoundInMatch => "Plc Interp: value not found in match.";
