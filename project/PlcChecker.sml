(* PlcChecker *)

(* TODO: lembrar de remover exceção Debug *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList
exception Debug of expr

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		 Var x => lookup env x
		| ConI _ => IntT
		| ConB _ => BoolT
		| ESeq _ => ListT []
		| List(explist) =>
			let
				fun listType (explist: expr list) (env: plcType env): plcType list =
					case explist of
						[] => []
						| h::[] => [teval h env]
						| h::t =>
							let
								val t1 = teval h env
								val rest = listType t env
							in
								t1::rest
							end
			in
				let
					val exptypes = listType explist env
				in
					ListT exptypes
				end
			end
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
						 ("print", _) => ListT []
						| _ => raise UnknownType
				end
		| Prim2(opr, e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					case (opr, t1, t2) of
					 ("*" , IntT, IntT) => IntT
					| ("/" , IntT, IntT) => IntT
					| ("+" , IntT, IntT) => IntT
					| ("-" , IntT, IntT) => IntT
					| (";" , _ , _)    => t2
					| _   =>  raise UnknownType
				end
		| Let(x, e1, e2) =>
				let
					val t = teval e1 env
					val env' = (x,t)::env
				in
					teval e2 env'
				end
		| _ => 
				let
					val t = 2
				in
					print(" failure \n");
					raise Debug e
				end
