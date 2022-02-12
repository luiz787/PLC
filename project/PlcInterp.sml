exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (expression : expr) (env : plcVal env) : plcVal = 
case expression of
    ConI n => IntV n
  | ConB b => BoolV b
  | Var x => lookup env x
  | List a => (case a of
                    [] => ListV []
                  | l => ListV (List.map (fn x => eval x env) l))
  | Prim1 (op1, exp) => 
          (let 
              val exp' = eval exp env 
           in
              case (op1, exp') of 
                    ("-", IntV n) => IntV (~n)
                  | ("!", BoolV b) => BoolV (not b)
                  | ("hd", l) => (case l of 
                                       SeqV [] => raise HDEmptySeq 
                                     | SeqV (e::_) => e)
                  | ("tl", l) => (case l of 
                                       SeqV [] => raise TLEmptySeq
                                     | SeqV (_::ts) => SeqV ts)
                  | ("ise", SeqV l) => if l = [] then BoolV true else BoolV false
                  | ("print", _) => 
                        (let
                           val s = val2string exp'
                         in
                           print(s^"\n"); ListV []
                        end)
              end)
  | Prim2 (op2, exp1, exp2) => 
          (let 
              val exp1' = eval exp1 env
              val exp2' = eval exp2 env
          in
              case (op2, exp1', exp2') of 
                    ("+", IntV e1, IntV e2) => IntV (e1 + e2)
                  | ("-", IntV e1, IntV e2) => IntV (e1 - e2)
                  | ("*", IntV e1, IntV e2) => IntV (e1 * e2)
                  | ("/", IntV e1, IntV e2) => IntV (e1 div e2)
                  | ("<", IntV e1, IntV e2) => BoolV (e1 < e2)
                  | ("<=", IntV e1, IntV e2) => BoolV (e1 <= e2)
                  | ("&&", BoolV e1, BoolV e2) => BoolV (e1 andalso e2)
                  | ("=", _, _) => BoolV (exp1' = exp2')
                  | ("!=", _, _) => BoolV (exp1' <> exp2')
                  | ("::", _, _) => (case exp2' of 
                                           SeqV l => SeqV (exp1'::l)
                                         | _ => raise Impossible)
                  | (";", _, _) => exp2'
          end)
  | ESeq (SeqT ty) => SeqV []
  | Item (n, exp) => 
        (let 
            val arr = eval exp env
         in
            case arr of 
                      (ListV l) => List.nth(l, n - 1)
                    | _ => raise SymbolNotFound
         end)
  | If (b, exp1, exp2) => 
        (let
            val b' = eval b env 
         in
              case b' of 
                  BoolV e => if e then eval exp1 env else eval exp2 env 
         end)
  | Match (exp1, ls) => 
              (case ls of 
                   [] => raise ValueNotFoundInMatch
                 | ((expO, exp2)::l) =>
                        (let 
                            val exp1' = eval exp1 env
                            val exp2' = eval exp2 env
                            val newMatch = Match (exp1, l)
                         in
                            case expO of
                                 (SOME (exp3)) => if exp1' = (eval exp3 env) then exp2' else eval newMatch env
                               | (NONE) => exp2'
                        end))
  | Anon (_, name, exp) => Clos ("", name, exp, env)
  | Let (name, exp1, exp2) => 
        (let 
            val exp1' = eval exp1 env
            val env' = (name, exp1')::env
         in
            eval exp2 env'
         end)
  | Letrec (name, _, paramName, _, exp1, exp2) => eval exp2 ((name, Clos("", paramName, exp1, env))::env)
  | Call (v, e) => 
        (let
            val v' = eval v env
        in
            case (v, v') of 
                 (Var f, Clos (_, paramName, exp, env2)) =>
                                (let 
                                    val ev = eval e env
                                    val env' = (paramName, ev) :: (f, v') :: env2
                                 in
                                    eval exp env'
                                 end)
                | (Call (_, _), Clos(_, paramName, exp, env2)) =>
                                (let 
                                    val ev = eval e env
                                    val env' = (paramName, ev) :: env2
                                 in
                                    eval exp env'
                                 end)
                | _ => raise NotAFunc
         end)
  | _ => raise Impossible
