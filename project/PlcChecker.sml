(* PlcChecker *)

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

fun isEqualityType (t: plcType): bool =
	case t of
		BoolT => true
		| IntT => true
		| ListT([]) => true (* Nil *)
		| ListT(types) => foldl(fn (e, acc) => acc andalso isEqualityType(e)) true types
		| SeqT(st) => isEqualityType(st)
		| _ => false

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		 Var x => lookup env x
		| ConI _ => IntT
		| ConB _ => BoolT
		| ESeq (SeqT st) => SeqT st
		| List(explist) => ListT (map (fn e => teval e env) explist)
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr) of
						("print") => ListT []
						| ("!") => if t1 = BoolT then BoolT else raise UnknownType
						| ("-") => if t1 = IntT then IntT else raise UnknownType
						| ("hd") =>
							let
							in
								case e1 of
									ESeq(_) => raise EmptySeq
									| _ => 
										let
										in
											case t1 of
												SeqT(st) => st
												| _ => raise UnknownType
										end
							end
						| ("tl") =>
							let
							in
								case e1 of
									ESeq(_) => raise EmptySeq
									| _ => 
										let
										in
											case t1 of
												SeqT(st) => SeqT(st)
												| _ => raise UnknownType
										end
							end
						| ("ise") => 
							let
							in
								case t1 of
									SeqT(et) => BoolT
									| _ => raise UnknownType
							end
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
					| ("<", IntT, IntT) => BoolT
					| ("<=", IntT, IntT) => BoolT
					| (";" , _ , _)    => t2
					| ("&&", BoolT, BoolT) => BoolT
					| ("::", _, SeqT(et)) =>
						if t1 = et
						then
							t2
						else
							raise UnknownType
					| ("=", _, _) =>
						let
							val t1IsEqType = isEqualityType t1
							val t2IsEqType = isEqualityType t2
						in
							if t1IsEqType andalso t2IsEqType andalso t1 = t2
							then
								BoolT
							else
								raise NotEqTypes
						end
					| ("!=", _, _) =>
						let
							val t1IsEqType = isEqualityType t1
							val t2IsEqType = isEqualityType t2
						in
							if t1IsEqType andalso t2IsEqType andalso t1 = t2
							then
								BoolT
							else
								raise NotEqTypes
						end
					| _   =>  raise UnknownType
				end
		| Let(x, e1, e2) =>
			let
				val t = teval e1 env
				val env' = (x,t)::env
			in
				teval e2 env'
			end
		| Anon(s, x, body) =>
				let
					val env' = (x, s)::env
				in
					let
						val btype = teval body env'
					in
						FunT (s, btype)
					end
				end
		| Letrec(fname, argtype, argname, ftype, fbody, e2) =>
			let
				val providedFunType = FunT (argtype, ftype)
			in
				let
					val envWithFun = (fname, providedFunType)::env
					val envInsideFun = (fname, providedFunType)::(argname, argtype)::env
				in
					let
						val calcfbodytype = teval fbody envInsideFun
					in
						if calcfbodytype <> ftype
						then
							raise WrongRetType
						else
							teval e2 envWithFun
					end
				end
			end
		| Call(e1, e2) =>
			let
				val calleeType = teval e1 env
				val argType = teval e2 env
			in
				case calleeType of
				FunT (calleeArgType, fnRetType) => 
					let
						val equal = argType = calleeArgType
					in
						if equal
						then
							fnRetType
						else
							raise CallTypeMisM
					end
				| _ => raise NotFunc
			end
		| If(cond, thn, els) =>
			let
				val condType = teval cond env
				val thenType = teval thn env
				val elseType = teval els env 
			in
				if condType <> BoolT
				then
					raise IfCondNotBool
				else if thenType <> elseType
				then
					raise DiffBrTypes
				else
					thenType
			end
		| Match(matchexpr, []) => raise NoMatchResults
		| Match(matchexpr, arms) =>
			let
				val matchExprType = teval matchexpr env
			in
				let
					fun matchArmExpTypeIsEqualToMatchedExprType matchArm =
						case matchArm of
							(SOME(mExp), mBody) =>
								let
									val mArmExprType = teval mExp env
								in
									if mArmExprType = matchExprType
									then
										true
									else
										false
								end
							 | (NONE, mBody) => true
					
					fun matchBodyExpressionMapper matchArm =
						let
							val (_, mArmBody) = matchArm
						in
							teval mArmBody env
						end
					
					fun remove(x, l) =
						if l = []
						then
							[]
						else if x = hd(l)
						then
							remove(x, tl(l))
						else
							hd(l)::remove(x, tl(l))

					fun uniqueItems l =
						if l = []
						then
							[]
						else
							hd(l)::remove(hd(l), uniqueItems(tl(l)))

				in
					let
						val allEq = foldl(fn(elem, acc) => acc andalso matchArmExpTypeIsEqualToMatchedExprType(elem)) true arms
						val matchBodyTypes = map matchBodyExpressionMapper arms
					in
						let
							val uniqueMatchBodyTypes = uniqueItems matchBodyTypes
						in
							if not(allEq)
							then
								raise MatchCondTypesDiff
							else if length(uniqueMatchBodyTypes) > 1
							then
								raise MatchResTypeDiff
							else
								hd matchBodyTypes
						end
					end
				end
			end
		| Item(pos, expr) =>
			let
				val tt = teval expr env
			in
				case tt of
					ListT (exprlist) =>
						let
							val atpos = List.nth (exprlist, (pos - 1))
							handle Subscript => raise ListOutOfRange
						in
							atpos
						end
					| _ => raise OpNonList 
			end
