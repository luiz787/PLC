%%

%name PlcParser

%pos int

%term VAR
    | FUN
    | REC
    | EQ
    | COLON
    | SEMICOLON
    | IF
    | THEN
    | ELSE
    | MATCH
    | WITH
    | UNDERSCORE
    | EXCL
    | HD
    | TL
    | ISE
    | PRINT
    | AND
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | NEQ
    | LT
    | LTE
    | DOUBLECOLON
    | LSQBRACKET
    | RSQBRACKET
    | LPAREN
    | RPAREN
    | FN
    | FATARROW
    | END
    | TRUE
    | FALSE
    | LBRACE
    | RBRACE
    | CINT of int
    | NAME of string
    | PIPE
    | COMMA
    | ARROW
    | NILTYPE
    | BOOLTYPE
    | INTTYPE
    | EOF

%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | AtomicExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of (expr option)
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomicType of plcType
    | Types of plcType list

%right SEMICOLON ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NEQ
%left LT LTE
%right DOUBLECOLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc EXCL HD TL ISE PRINT NAME
%left LSQBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR NAME EQ Expr SEMICOLON Prog(Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr SEMICOLON Prog(Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args COLON Type EQ Expr SEMICOLON Prog(makeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomicExpr (AtomicExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | EXCL Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr TIMES Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
    | Expr DOUBLECOLON Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))
    | Expr LSQBRACKET CINT RSQBRACKET (Item(CINT, Expr))

AtomicExpr : Const (Const)
    | NAME (Var NAME)
    | LBRACE Prog RBRACE (Prog)
    | LPAREN Expr RPAREN (Expr)
    | LPAREN Comps RPAREN (List Comps)
    | FN Args FATARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2))
    | AppExpr AtomicExpr (Call(AppExpr, AtomicExpr))

Const : TRUE (ConB(true))
    | FALSE (ConB(false))
    | CINT (ConI(CINT))
    | LPAREN RPAREN (List([]))
    | LPAREN Type LSQBRACKET RSQBRACKET RPAREN (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME Expr)
    | UNDERSCORE (NONE)

Args : LPAREN RPAREN ([])
    | LPAREN Params RPAREN (Params)

Params : TypedVar (makeArgs(TypedVar, []))
    | TypedVar COMMA Params (makeArgs(TypedVar, Params))

TypedVar : Type NAME ((Type, NAME))

Type : AtomicType (AtomicType)
    | LPAREN Types RPAREN (ListT(Types))
    | LSQBRACKET Type RSQBRACKET (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

AtomicType : NILTYPE (ListT [])
    | BOOLTYPE (BoolT)
    | INTTYPE (IntT)
    | LPAREN Type RPAREN (Type)

Types : Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)
