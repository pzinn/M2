--		Copyright 1993-2002 by Daniel R. Grayson
-- rewritten by P. Zinn-Justin 2018-2020

protect symbol operator
protect symbol EmptyName

Constant = new Type of BasicList
globalAssignment Constant

precedence = method(Dispatch => Thing)
rightPrecedence = method(Dispatch => Thing)
lprec = prec = x -> (getParsing x)#0
rprec = strength2 = x -> (getParsing x)#1
uprec = strength1 = x -> (getParsing x)#2

-- local variables
unit := symbol unit
letters := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"
digits := set characters "0123456789"
endsWithIdentifier := s -> (
     c := "";
     n := # s - 1;
     while ( 
	  c = s#n;
	  n > 0 and digits#?c
	  ) do n = n - 1;
     letters#?c)

-----------------------------------------------------------------------------
bigParenthesize = n -> (
     h := height n;
     d := depth n;
     if (h === 2 or h === 1) and (d === 0 or d === 1) then return "(" | n | ")";
     if h+d <= 1 then return "("^-d | n | ")"^-d;
     (stack("/",h+d-2:"|","\\"))^(h-1) | n | (stack("\\",h+d-2:"|","/"))^(h-1)
     )
-----------------------------------------------------------------------------

HeaderType = new Type of Type
HeaderType.synonym = "header type"
HeaderType List := (T,z) -> new T from z
HeaderType Sequence := (T,z) -> new T from z

WrapperType = new Type of Type
WrapperType.synonym = "wrapper type"
WrapperType List := (T,z) -> new T from z
WrapperType Sequence := (T,z) -> new T from z
WrapperType Thing := (T,z) -> new T from {z}

-----------------------------------------------------------------------------

Expression = new Type of BasicList
Expression.synonym = "expression"
expression = method(Dispatch => Thing, TypicalValue => Expression)
expression Expression := identity
Expression.operator = ""

expressionValue = method(Dispatch => Thing)
expressionValue VisibleList := x -> apply(x,expressionValue)
expressionValue Thing := identity

-- with the following line we have no way to distinguish between "hold symbol x" and "hold x" when x has a value:
-- but without it, we have no way to recover a polynomial from its expression, without introducing Holder2 or something like it
expressionValue Symbol := value

value Expression := expressionValue

--Holder2 = new WrapperType of Expression			    -- Holder{ printable form, value form }
--Holder2.synonym = "holder"
--Holder = new WrapperType of Holder2			    -- Holder{ printable form, value form }, with printable form === value form
Holder = new WrapperType of Expression
Holder.synonym = "holder"

Describe = new WrapperType of Holder
Describe.synonym = "description"
describe = method(Dispatch => Thing)
describe Thing := x -> new Describe from { unhold expression x }
Describe#{Standard,AfterPrint} = identity -- all this to suppress "o##: class" thing

-- new Holder2 from VisibleList := (H,x) -> (
--      assert( #x === 2 );
--      if instance(x#0,Holder) then {x#0#0,x#1} else {x#0,x#1})
--new Holder from VisibleList := (H,x) -> (
--     assert( #x === 1 );
--     {x#0,x#0})

hold = method(Dispatch => Thing, TypicalValue => Expression)
hold Thing := x -> new Holder from {x}
hold Expression := identity

unhold = method()
unhold Holder := first
-- unhold Holder2 := first
unhold Expression := identity

AssociativeExpression = new Type of Expression
AssociativeExpression.synonym = "associative expression"
--new AssociativeExpression from Sequence := 
--new AssociativeExpression from List := (type,v) -> (
--     toList splice apply(v, 
--	  term -> if class term === type then toSequence term else term
--	  )
--     )

lookupi := x -> (
     r := lookup x;
     if r === null then error "encountered null or missing value";
     r)

toString' = method()
toString Thing := v -> toString'(toString,v)
toString' (Function, Thing) := (toString,x) -> ( y := expression x;
    -- we need to avoid loops: objects whose expression is a Holder and whose net is undefined
    if instance(y,Holder) and class y#0 === class x then simpleToString x -- if all else fails...
    else toString y )

toExternalFormat = method(Dispatch=>Thing)
toExternalFormat Thing := toExternalString
toExternalFormat Expression := v -> toString'(toExternalFormat,v)
toExternalFormat Symbol := toExternalFormat Sequence := toString

toString'(Function, Expression) := (fmt,v) -> (
     op := class v;
     p := precedence v;
     names := apply(toList v,term -> (
	       if precedence term <= p
	       then "(" | fmt term | ")"
	       else fmt term
	       )
	  );
     if # v === 0 then op.EmptyName
     else demark(op.operator,names)
     )

--texMath Holder2 := v -> "{" | texMath v#0 | "}"
--html Holder2 := v -> html v#0
--net Holder2 := v -> net v#0

html Holder := v -> html v#0
net Holder := v -> net v#0
toString Holder := v -> toString v#0

--toString'(Function, Holder2) := (fmt,v) -> fmt v#0
toString'(Function, Holder) := (fmt,v) -> fmt v#0

remove(Sequence,expression)

Minus = new WrapperType of Expression		  -- unary minus
Minus.synonym = "minus expression"

Minus.operator = "-"
expressionValue Minus := v -> minus apply(toSequence v,expressionValue)
toString'(Function, Minus) := (fmt,v) -> (
     term := v#0;
     if precedence term > precedence v or class term === Product
     then "-" | fmt term
     else "-(" | fmt term | ")"
     )

Equation = new HeaderType of AssociativeExpression
Equation.synonym = "equation expression"
Equation.operator = "=="
expressionValue Equation := (v) -> (
     v = apply(toSequence v,expressionValue);
     if # v === 2
     then v#0 == v#1
     else if # v <= 1
     then true
     else (
     	  x := v#0;
     	  w := drop(v,1);
     	  all(w,y->x==y)
     	  )
     )
net Equation := v -> (
     n := # v;
     if n === 0 then "Equation{}"
     else if n === 1 then "Equation{" | net v#0 | "}"
     else (
	  p := precedence v;
	  horizontalJoin toList between(" == ", 
	       apply(toList v, e -> if precedence e <= p then bigParenthesize net e else net e))))
toString'(Function, Equation) := (fmt,v) -> (
     n := # v;
     if n === 0 then "Equation{}"
     else if n === 1 then "Equation{" | fmt v#0 | "}"
     else (
	  p := precedence v;
	  demark(" == ", 
	       apply(toList v, e -> if precedence e <= p then ("(", fmt e, ")") else fmt e))))
-----------------------------------------------------------------------------
ZeroExpression = new Type of Holder
ZeroExpression.synonym = "zero expression"
ZERO = new ZeroExpression from {0}
unhold ZeroExpression := identity
-----------------------------------------------------------------------------
OneExpression = new Type of Holder
OneExpression.synonym = "one expression"
ONE = new OneExpression from {1}
unhold OneExpression := identity
-----------------------------------------------------------------------------
Parenthesize = new WrapperType of Expression
Parenthesize.synonym = "possibly parenthesized expression"
net Parenthesize := net @@ first
toString'(Function, Parenthesize) := (fmt,v) -> fmt v#0
expressionValue Parenthesize := first
-----------------------------------------------------------------------------
Sum = new WrapperType of AssociativeExpression
Sum.synonym = "sum expression"

Sum#unit = ZERO
Sum.EmptyName = "0"
Sum.operator = "+"
expressionValue Sum := v -> plus apply(toSequence v,expressionValue)

toString'(Function, Sum) := (fmt,v) -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"+"));
	  seps#0 = seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus 
		    then ( seps#i = "-"; v#i#0 )
		    else v#i ));
	  names := apply(n, i -> (
		    if precedence v#i <= p 
		    then "(" | fmt v#i | ")"
		    else fmt v#i ));
	  concatenate mingle ( seps, names )))

Product = new WrapperType of AssociativeExpression
Product.synonym = "product expression"

Product#unit = ONE
Product.EmptyName = "1"
Product.operator = "*"
expressionValue Product := v -> times apply(toSequence v,expressionValue)

toString'(Function, Product) := (fmt,v) -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"*"));
	  seps#0 = seps#n = "";
     	  names := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then "(" | fmt term | ")"
	       	    else fmt term));
	  concatenate mingle ( seps, names )
	  )
     )

NonAssociativeProduct = new WrapperType of Expression
NonAssociativeProduct.synonym = "nonassociative product expression"

NonAssociativeProduct#unit = ONE
NonAssociativeProduct.EmptyName = "1"
NonAssociativeProduct.operator = "**"
expressionValue NonAssociativeProduct := v -> times apply(toSequence v,expressionValue)

toString'(Function, NonAssociativeProduct) := (fmt,v) -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"**"));
	  seps#0 = seps#n = "";
     	  names := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then "(" | fmt term | ")"
	       	    else fmt term
	       	    )
	       );
	  scan(# v - 1,
	       i -> (
		    if seps#(i+1)!=""
		    and names#(i+1)#?0
		    and letters#?(names#(i+1)#0)
		    and not endsWithIdentifier names#i 
		    then seps#(i+1)=""
		    )
	       );
	  concatenate mingle ( seps, names )
	  )
     )

Divide = new HeaderType of Expression
Divide.synonym = "divide expression"
Divide.operator = "/"
expressionValue Divide := (x) -> (expressionValue x#0) / (expressionValue x#1)
numerator Divide := x -> x#0
denominator Divide := x -> x#1

Power = new HeaderType of Expression
Power.synonym = "power expression"
Power.operator = "^"
expressionValue Power := (x) -> (expressionValue x#0) ^ (expressionValue x#1)

Subscript = new HeaderType of Expression
Subscript.synonym = "subscript expression"
Subscript.operator = "_"
expressionValue Subscript := (x) -> (expressionValue x#0)_(expressionValue x#1)

Superscript = new HeaderType of Expression
Superscript.synonym = "superscript expression"
Superscript.operator = "^"
expressionValue Superscript := (x) -> (expressionValue x#0)^(expressionValue x#1)

toString'(Function, Subscript) := toString'(Function, Superscript) := (fmt,v) -> (
     x := fmt v#0;
     y := fmt v#1;
     p := precedence v;
     if precedence v#0 <  p then x = "(" | x | ")";
     if precedence v#1 <= p then y = "(" | y | ")";
     concatenate(x,(class v).operator,y))

toString'(Function, Power) := (fmt,v) -> (
     x := v#0;
     y := v#1;
     if y === 1 then fmt x 
     else (
	  x = fmt x;
	  y = fmt y;
	  if precedence v#0 <  prec symbol ^  then x = "(" | x | ")";
	  if precedence v#1 <= prec symbol ^  then y = "(" | y | ")";
	  concatenate(x,(class v).operator,y)))

-----------------------------------------------------------------------------
RowExpression = new HeaderType of Expression
RowExpression.synonym = "row expression"
net RowExpression := w -> horizontalJoin apply(toList w,net)
html RowExpression := x -> concatenate("<span style=\"display:inline-flex;flex-direction:row\">", apply(toList x, html), "</span>")
toString'(Function, RowExpression) := (fmt,w) -> concatenate apply(w,fmt)
Expression | Expression := (a,b) -> new RowExpression from {a,b}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
ColumnExpression = new HeaderType of Expression
ColumnExpression.synonym = "row expression"
net ColumnExpression := w -> stack apply(toList w,net)
toString ColumnExpression := toString @@ net
html ColumnExpression := x -> concatenate("<span style=\"display:inline-flex;flex-direction:column\">", apply(toList x, html), "</span>")
toString'(Function, ColumnExpression) := (fmt,w) -> demark("\n", apply(w,fmt))
Expression || Expression := (a,b) -> new ColumnExpression from {a,b}
-----------------------------------------------------------------------------
Adjacent = new HeaderType of Expression
Adjacent.synonym = "adjacent expression"
expressionValue Adjacent := x -> (expressionValue x#0) (expressionValue x#1)
-----------------------------------------------------------------------------
prepend0 := (e,x) -> prepend(e#0, x)
append0 := (x,e) -> append(x, e#0)
Equation == Equation        := join
Equation == Expression      := append
Equation == Holder          := append0
Expression == Equation      := prepend
Holder     == Equation      := prepend0
Expression == Expression    := Equation => (x,y) -> new Equation from {x,y}
Holder     == Holder        := (x,y) -> new Equation from {x#0,y#0}
Expression == Thing         := (x,y) -> x == expression y
Thing == Expression         := (x,y) -> expression x == y
ZeroExpression + Expression := (x,y) -> y
Sum + ZeroExpression     :=
Holder + ZeroExpression     :=
Expression + ZeroExpression := (x,y) -> x
Sum + Sum                   := join
Sum + Expression            := append
Sum + Holder                := append0
Expression + Sum            := prepend
Holder     + Sum            := prepend0
Expression + Expression     := Sum => (x,y) -> new Sum from {x,y}
       - ZeroExpression     := identity
	   - Minus          := x -> expression x#0
           - Expression     := x -> new Minus from {x}
           - Holder         := x -> new Minus from {x#0}
Expression - Expression     := Sum => (x,y) -> x + Minus y
Thing - Minus               := Sum => (x,y) -> expression x + y#0
Product    * OneExpression  :=
Expression * OneExpression  :=
Holder     * OneExpression  := (x,y) -> x
OneExpression * Expression  := (x,y) -> y
Holder     * ZeroExpression :=
Product    * ZeroExpression :=
Expression * ZeroExpression := (x,y) -> y
ZeroExpression * Holder     :=
ZeroExpression * Expression := (x,y) -> x
Product * Product           := join
Product * Expression        := append
Product * Holder            := append0
Expression * Product        := prepend
Holder     * Product        := prepend0
Expression * Expression := Product => (x,y) -> new Product from {x,y}
Expression * Minus := (x,y) -> -(x * y#0)
Minus * Expression := (x,y) -> -(x#0 * y)
Minus * Minus := (x,y) -> expression x#0 * expression y#0
Expression * Thing      := (x,y) -> x * (expression y)
     Thing * Expression := (x,y) -> (expression x) * y
Holder     ** OneExpression :=
Expression ** OneExpression := (x,y) -> x
OneExpression ** Holder     :=
OneExpression ** Expression := (x,y) -> y
NonAssociativeProduct ** NonAssociativeProduct := join
NonAssociativeProduct ** Expression := append
NonAssociativeProduct ** Holder     := append0
Expression Expression := Adjacent => (x,y) -> new Adjacent from {x,y}
     -- are lists expressions, too???
Expression Thing      := (x,y) -> x (expression y)
     Thing Expression := (x,y) -> (expression x) y
Expression ** NonAssociativeProduct := prepend
Holder     ** NonAssociativeProduct := prepend0
Expression ** Expression := NonAssociativeProduct => (x,y) -> new NonAssociativeProduct from {x,y}
Holder     / OneExpression :=
Expression / OneExpression := (x,y) -> x
Expression / Expression := Divide => (x,y) -> new Divide from {x,y}
not Equation := e -> if #e == 2 then BinaryOperation { symbol !=, e#0, e#1 } else -* UnaryOperation{symbol not, e} *- error ("negation of an equation with ", toString (#e), " parts")
expression ZZ := i -> (
     if i === 0 then ZERO
     else if i === 1 then ONE
     else if i === -1 then new Minus from { ONE }
     else if i < 0 then new Minus from { -i }
     else hold i
     )
Holder     ^ OneExpression :=
Expression ^ OneExpression := (x,y) -> x
Holder     ^ ZeroExpression :=
Expression ^ ZeroExpression := (x,y) -> ONE -- potentially dangerous (modules?)
ZeroExpression ^ Holder     :=
ZeroExpression ^ Expression := (x,y) -> ZERO
ZeroExpression ^ ZeroExpression := (x,y) -> ONE
Expression ^ Expression := Power => (x,y) -> Power{x,y}
Expression _ Expression := Subscript => (x,y) -> Subscript{x,y}

InfiniteNumber .. InfiniteNumber :=
InfiniteNumber .. ZZ             :=
ZZ             .. InfiniteNumber := (x,y) -> if x < y then (
     error "infinite range requested";
     -- BinaryOperation{symbol ..,x,y}
     ) else ()

InfiniteNumber ..< InfiniteNumber :=
InfiniteNumber ..< ZZ             :=
ZZ             ..< InfiniteNumber := (x,y) -> if x < y then (
     error "infinite range requested";
     -- BinaryOperation{symbol ..<,x,y}
     ) else ()

binaryOperatorFunctions := new HashTable from {
     symbol * => ((x,y) -> x*y),
     symbol + => ((x,y) -> x+y),
     symbol - => ((x,y) -> x-y),
     symbol / => ((x,y) -> x/y),
     symbol // => ((x,y) -> x//y),
     symbol ^ => ((x,y) -> x^y),
     symbol == => ((x,y) -> x==y),
     symbol .. => ((x,y) -> x..y),
     symbol ..< => ((x,y) -> x..<y),
     symbol % => ((x,y) -> x%y),
     symbol @ => ((x,y) -> x@y),
     symbol ==> => ((x,y) -> x==>y),
     symbol ===> => ((x,y) -> x===>y),
     symbol <== => ((x,y) -> x<==y),
     symbol <=== => ((x,y) -> x<===y),
     symbol <==> => ((x,y) -> x<==>y),
     symbol |- => ((x,y) -> x|-y),
     symbol \ => ((x,y) -> x\y),
     symbol @@ => ((x,y) -> x@@y),
     symbol & => ((x,y) -> x&y),
     symbol ? => ((x,y) -> x?y),
     symbol | => ((x,y) -> x|y),
     symbol => => ((x,y) -> x=>y),
     symbol || => ((x,y) -> x||y),
     symbol << => ((x,y) -> x<<y),
     symbol >> => ((x,y) -> x>>y),
     symbol : => ((x,y) -> x:y),
     symbol ++ => ((x,y) -> x++y),
     symbol ** => ((x,y) -> x**y),
     symbol _ => ((x,y) -> x_y),
     symbol SPACE => ((x,y) -> x y),
     symbol != => ((x,y) -> x != y),
     symbol and => ((x,y) -> x and y),
     symbol or => ((x,y) -> x or y),
     symbol ^** => ((x,y) -> x^**y)
     }

expressionBinaryOperators = -- excludes built-in functions, cf typicalvalues.m2, and a few more such as functional
{symbol and, symbol <==, symbol ^**, symbol ^, symbol ==>, symbol _,
    symbol ==, symbol ++, symbol <===, symbol <==>, symbol or,
    symbol %, symbol SPACE, symbol &, symbol *, symbol +,
    symbol -, symbol |-, symbol :, symbol !=, symbol |, symbol ..<,
    symbol @@, symbol @, symbol **, symbol .., symbol ^^,
    symbol ||, symbol ===>, symbol /}

scan(expressionBinaryOperators, op -> (
    f := try Expression#(op,Expression,Expression) else installMethod(op,Expression,Expression,(x,y) -> BinaryOperation{op,x,y});
    installMethod(op,Expression,Holder,(x,y) -> f(x,y#0)); -- or we could just use unhold...
    installMethod(op,Holder,Expression,(x,y) -> f(x#0,y));
    installMethod(op,Holder,Holder,(x,y) -> f(x#0,y#0));
    g := try binaryOperatorFunctions#op else f; -- subtly different
    installMethod(op,Expression,Thing,(x,y) ->  g(x,expression y));
    installMethod(op,Thing,Expression,(x,y) ->  g(expression x,y));
    ))

-----------------------------------------------------------------------------
--expressionValue Holder2 := x -> x#1
expressionValue Holder := x -> expressionValue x#0
expressionValue OneExpression := v -> 1
expressionValue ZeroExpression := v -> 0
-----------------------------------------------------------------------------
SparseVectorExpression = new HeaderType of Expression
SparseVectorExpression.synonym = "sparse vector expression"
expressionValue SparseVectorExpression := x -> notImplemented()
toString'(Function, SparseVectorExpression) := (fmt,v) -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=fmt x);
     w = toList w;
     concatenate("{",between(",",w),"}")
     )
-----------------------------------------------------------------------------
SparseMonomialVectorExpression = new HeaderType of Expression
SparseMonomialVectorExpression.synonym = "sparse monomial vector expression"
-- in these, the basis vectors are treated as variables for printing purposes
expressionValue SparseMonomialVectorExpression := x -> notImplemented()
toString'(Function, SparseMonomialVectorExpression) := (fmt,v) -> toString (
     sum(v#1,(i,m,a) -> 
	  expression a * 
	  expression m * 
	  hold concatenate("<",fmt i,">"))
     )
-----------------------------------------------------------------------------
MatrixExpression = new HeaderType of Expression
MatrixExpression.synonym = "matrix expression"
expressionValue MatrixExpression := x -> matrix applyTable(toList x,expressionValue)
toString'(Function,MatrixExpression) := (fmt,m) -> concatenate(
     "matrix {",
     between(", ",apply(toList m,row->("{", between(", ",apply(row,fmt)), "}"))),
     "}" )
MatrixDegreeExpression = new HeaderType of Expression
MatrixDegreeExpression.synonym = "matrix with degrees expression"
expressionValue MatrixDegreeExpression := x -> (
    m := expressionValue MatrixExpression x#0;
    R := ring m;
    n := degreeLength R;
    if all(x#1|x#2, y->(class y === List and #y===n) or (class y === ZZ and n===1))
    then map(R^(-x#1),R^(-x#2),entries m)
    else m
    )
toString'(Function,MatrixDegreeExpression) := (fmt,x) -> toString'(fmt,MatrixExpression x#0)
-----------------------------------------------------------------------------
VectorExpression = new HeaderType of Expression
VectorExpression.synonym = "vector expression"
expressionValue VectorExpression := x -> vector apply(toList x,expressionValue)
toString'(Function,VectorExpression) := (fmt,v) -> concatenate(
     "vector {",
     between(", ",apply(toList v,fmt)),
     "}" )
-----------------------------------------------------------------------------
Table = new HeaderType of Expression
Table.synonym = "table expression"
expressionValue Table := x -> applyTable(toList x,expressionValue)
toString'(Function, Table) := (fmt,m) -> concatenate(
     "Table {",
     between(", ",apply(toList m,row->("{", between(", ",apply(row,fmt)), "}"))),
     "}" )
-----------------------------------------------------------------------------

spacedOps = set { symbol =>, symbol and, symbol or, symbol ++, symbol |- } -- more?

BinaryOperation = new HeaderType of Expression -- {op,left,right}
BinaryOperation.synonym = "binary operation expression"
expressionValue BinaryOperation := (m) -> (
     if binaryOperatorFunctions#?(m#0) then binaryOperatorFunctions#(m#0) (expressionValue m#1,expressionValue m#2) else m
     )
net BinaryOperation := m -> (
     x := net m#1;
     y := net m#2;
     if rightPrecedence m#1 < lprec m#0 then x = bigParenthesize x;
     if precedence m#2 <= rprec m#0 then y = bigParenthesize y;
     if spacedOps#?(m#0) then horizontalJoin( x, " ", toString m#0, " ", y ) else horizontalJoin( x, toString m#0, y )
     )

toString'(Function, BinaryOperation) := (fmt,m) -> (
     x := fmt m#1;
     y := fmt m#2;
     if rightPrecedence m#1 < lprec m#0 then x = ("(",x,")");
     if precedence m#2 <= rprec m#0 then y = ("(",y,")");
     if spacedOps#?(m#0) then concatenate( x, " ", toString m#0, " ", y ) else concatenate( x, toString m#0, y )
     )

-----------------------------------------------------------------------------
FunctionApplication = new HeaderType of Expression -- {fun,args}
FunctionApplication.synonym = "function application expression"
expressionValue FunctionApplication := (m) -> (expressionValue m#0) (expressionValue m#1)
toString'(Function, Adjacent) := toString'(Function, FunctionApplication) := (fmt,m) -> (
     p := precedence m;
     fun := m#0;
     args := m#1;
     if class args === Sequence
     then if #args === 1
     then concatenate(fmt fun, "(", fmt args#0, ")")  -- f (1:x)
     else concatenate(fmt fun, fmt args)       -- f(x,y) or f(), ...
     else if precedence args > p
     then if precedence fun > p
     then concatenate(fmt fun, if not instance(args,Array) then " ", fmt args)
     else concatenate("(", fmt fun, ")", fmt args)
     else if precedence fun > p
     then concatenate(fmt fun, "(", fmt args, ")")
     else concatenate("(", fmt fun, ")(", fmt args, ")")
     )
net Adjacent := net FunctionApplication := m -> (
     p := precedence m;
     fun := m#0;
     args := m#1;
     netfun := net fun;
     netargs := net args;
     div := instance(fun,Divide);
     pfun := if div then strength1 symbol symbol else precedence fun;
     if precedence args > p
     then if pfun >= p
     then (
	  if instance(args,Array) or div or class netfun === Net and netfun#?0 and width netfun > width netfun#0
	  then horizontalJoin (netfun, netargs)
	  else horizontalJoin (netfun, " ", netargs)
	  )
     else horizontalJoin (bigParenthesize netfun, netargs)
     else if pfun >= p
     then horizontalJoin (netfun, bigParenthesize netargs)
     else horizontalJoin (bigParenthesize netfun, bigParenthesize netargs)
     )

NewFromExpression = new WrapperType of Expression
NewFromExpression.synonym = "New ... From expression"
expressionValue NewFromExpression := x -> new (expressionValue x#0) from (expressionValue x#1)
-- temp: spacing needs improving
net NewFromExpression := lookup(net,Adjacent)
toString' (Function, NewFromExpression) := (fmt, e) -> "new " | fmt e#0 | " from " | fmt toList e#1


-----------------------------------------------------------------------------

returns = t -> x -> t

	      precedence Sequence := x -> if #x === 1 then prec symbol : else strength1 symbol symbol
     	  precedence Parenthesize := returns 0
	      precedence Equation := returns prec symbol ==
	     precedence HashTable := returns 0		    -- some things might print out as symbols though...
		 precedence Thing := returns 0
		   precedence Sum := returns prec symbol +
	       precedence Product := returns prec symbol *
 precedence NonAssociativeProduct := returns prec symbol **
		 precedence Minus := returns strength1 symbol -
   precedence FunctionApplication :=
     precedence NewFromExpression :=
              precedence Adjacent := returns prec symbol SPACE
		precedence Divide := returns prec symbol /
	     precedence Subscript := returns prec symbol _
	   precedence Superscript := returns prec symbol ^
		 precedence Power := x -> if x#1 === 1 then precedence x#0 else prec symbol ^
		    precedence ZZ := x -> if x>=0 then strength1 symbol symbol else prec symbol -
		    precedence RR :=
	      precedence Function :=
	          precedence Type :=
	       precedence Boolean :=
		  precedence List :=
		 precedence Array :=
	      precedence Constant :=
		precedence Symbol :=
		   precedence Net :=
		precedence String :=
	    precedence Expression := returns strength1 symbol symbol
	        precedence Holder := x -> precedence x#0
	      precedence Describe := x -> precedence x#0
--	       precedence Holder2 := x -> precedence x#0
       precedence BinaryOperation := x -> lprec x#0
  rightPrecedence BinaryOperation := x -> rprec x#0
            rightPrecedence Thing := precedence
-----------------------------------------------------------------------------
-- printing two dimensional ascii output

nobracket := identity

padto := (s,n) -> (
     k := n - stringlen s;
     if k === 0 then s else (s, k))

-- document { stringlen,
--      TT "stringlen s", "returns the length of the string s.  The argument
--      may also be a sequence or list of strings and symbols, and so
--      on, recursively, in which case the lengths of the various elements
--      are summed.  Additionally, an integer may be used to represent a
--      number of spaces.",
--      SeeAlso => {"String", "concatenate", "#" }
--      }

nopar := x -> (
     -- this is like net Sequence except we omit the parentheses.
     horizontalJoin deepSplice (
	  if #x === 0 then "()"
	  else if #x === 1 then ("1 : (", net x#0, ")") -- ugly
	  else (toSequence between(",",apply(x,net)))))

nopars := x -> if class x === Sequence then nopar x else net x

net Subscript := x -> (
     n := nopars x#1;
     if precedence x#0 < prec symbol ^
     then horizontalJoin( bigParenthesize net x#0, n^-(height n) )
     else net x#0 | n^-(height n)
     )

net Superscript := x -> if x#1 === moduleZERO then "0" else (
     n := net x#1;
     if precedence x#0 < prec symbol ^
     then horizontalJoin( bigParenthesize net x#0, n^(1+depth n))
     else net x#0 | n^(1+depth n)
     )

expectExponent = n -> if height n < 2 then n = (stack( 2 - height n : "", n))^1 else n

net Power := v -> (
     x := v#0;
     y := v#1;
     if y === 1 or y === ONE then net x
     else (
     	  nety := net y;
	  nety = nety ^ (1 + depth nety);
	  if class x === Subscript then (
	       t := stack(nety,"",nopars x#1);
	       horizontalJoin (
		    if precedence x < prec symbol ^
		    then ( bigParenthesize expectExponent net x#0, t)
		    else (                 net x#0, t)
		    )
	       )
	  else (
	       horizontalJoin (
		    if precedence x <= prec symbol ^
		    then ( bigParenthesize expectExponent net x, nety)
		    else (            	   net x, nety)
		    )
	       )
	  )
     )
net Sum := v -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->" + "));
	  seps#0 = seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus
		    then (
			 seps#i = if i == 0 then "- " else " - ";
			 v#i#0)
		    else v#i));
	  horizontalJoin splice mingle(seps,
	       apply(n, i ->
		    if precedence v#i <= p
		    then bigParenthesize net v#i
		    else      	   	 net v#i))))

isNumber = method(TypicalValue => Boolean)
isNumber Thing := i -> false
isNumber RR :=
isNumber QQ :=
isNumber Divide := -- QQ never appears in an expression, so we take care of it this way
isNumber ZZ := i -> true
isNumber Holder := i -> isNumber i#0

startsWithSymbol = method(TypicalValue => Boolean)
startsWithSymbol Thing := i -> false
startsWithSymbol Symbol := i -> true
startsWithSymbol Product :=
startsWithSymbol Subscript :=
startsWithSymbol Power :=

startsWithSymbol Holder := i -> startsWithSymbol i#0
-- startsWithSymbol Holder2 := i -> startsWithSymbol i#0

net Product := v -> (
     n := # v;
     if n === 0 then "1"
     else if n === 1 then net v#0
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, splice {"", n-1 : "*", ""});
	  if n>1 and isNumber v#0 and startsWithSymbol v#1 then seps#1 = "";
     	  boxes := apply(#v,
	       i -> (
		    term := v#i;
		    nterm := net term;
	       	    if precedence term <= p and class term =!= Divide then (
			 seps#i = seps#(i+1) = "";
			 nterm = bigParenthesize nterm;
			 );
		    if class term === Power
		    and not (term#1 === 1 or term#1 === ONE)
		    or class term === Subscript then (
			 seps#(i+1) = "";
			 );
	       	    nterm));
	  horizontalJoin splice mingle (seps, boxes)
	  )
     )
net NonAssociativeProduct := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i -> "**"));
	  seps#0 = seps#n = "";
     	  boxes := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then bigParenthesize net term
	       	    else net term
	       	    )
	       );
	  horizontalJoin splice mingle (seps, boxes)
	  )
     )

net Minus := x -> (
     term := x#0;
     horizontalJoin if precedence term <= precedence x	    -- note that precedence(Minus{...}) and precedence(Sum{...}) are equal
     then ("-", bigParenthesize net term)
     else (
	  term = net term;
	  h := height term - 1;
	  (if term#?h and term#h#?0 and term#h#0 === "-" then "- " else "-", term)))

net Divide := x -> (
     top := net x#0;
     bot := net x#1;
     wtop := width top;
     wbot := width bot;
     w := max(wtop,wbot);
     if instance(x#0,Divide) or instance(x#1,Divide) then w = w+2;
     itop := (w-wtop+1)//2;
     if itop != 0 then top = spaces itop | top;
     ibot := (w-wbot+1)//2;
     if ibot != 0 then bot = spaces ibot | bot;
     top = top || dashes w;
     top = top ^ (depth top);
     top || bot)
net SparseVectorExpression := v -> (
     if # v === 0
     then "0"
     else net sum(v#1,(i,r) -> (
	       expression r *
	       hold concatenate("<",toString i,">")
	       )
	  )
     )
net SparseMonomialVectorExpression := v -> (
     if # v === 0
     then "0"
     else (
	  net sum(v#1,(i,m,a) ->
	       expression a *
	       expression m *
	       hold concatenate("<",toString i,">"))
	  )
     )

net Table := x -> netList (toList x, HorizontalSpace=>2, VerticalSpace => 1, BaseRow => 0, Boxes => false, Alignment => Center)

compactMatrixForm=true; -- governs net MatrixExpression
matrixDisplayOptions := hashTable { true => new OptionTable from { HorizontalSpace => 1, VerticalSpace => 0, BaseRow => 0, Alignment => Left },
                                   false => new OptionTable from { HorizontalSpace => 2, VerticalSpace => 1, BaseRow => 0, Alignment => Center } }

-- modified to work with factorized expressions as well
toCompactString := method(Dispatch => Thing)
toCompactParen = x -> if precedence x < prec symbol * then "(" | toCompactString x | ")" else toCompactString x
toCompactString RingElement := x -> toString raw x
toCompactString Thing := toString
toCompactString Product := x -> if #x === 0 then "1" else concatenate apply(toList x,toCompactParen)
toCompactString Sum := x -> if #x === 0 then "0" else concatenate apply(#x,i->
    if i===0 or class x#i === Minus then toCompactString x#i else { "+", toCompactString x#i })
toCompactString Minus := x -> "-" | toCompactParen x#0
toCompactString Power := x -> if x#1 === 1 or x#1 === ONE then toCompactString x#0 else (
    a:=toCompactParen x#0;
    b:=toCompactString x#1;
    if #a =!= 1 then a|"^"|b else a|b
    )
toCompactParen1 = x -> if precedence x < prec symbol * and class x =!= Minus then "(" | toCompactString x | ")" else toCompactString x
toCompactString Divide := x -> toCompactParen1 x#0 | "/" | toCompactParen1 x#1

net MatrixExpression := x -> (
    if all(x,r->all(r,i->class i===ZeroExpression)) then "0"
    else (
	x=applyTable(toList x,if compactMatrixForm then toCompactString else net);
	netList(x,Boxes=>{false,{0,#x#0}},matrixDisplayOptions#compactMatrixForm)
     ))
html MatrixExpression := x -> html TABLE toList x

net MatrixDegreeExpression := x -> (
    if all(x#0,r->all(r,i->class i===ZeroExpression)) then "0"
    else (
	x=apply(#x#0,i->apply(prepend(x#1#i,x#0#i),if compactMatrixForm then toCompactString else net));
	netList(x,Boxes=>{false,{1,#x#0}},matrixDisplayOptions#compactMatrixForm)
     ))

net VectorExpression := x -> (
    if all(x,i->class i===ZeroExpression) then "0"
     else (
	 x=apply(toList x,y->{(if compactMatrixForm then toCompactString else net)y});
	netList(x,Boxes=>{false,{0,1}},HorizontalSpace=>1,VerticalSpace=>if compactMatrixForm then 0 else 1,BaseRow=>0,Alignment=>Center)
     ))
html VectorExpression := x -> html TABLE apply(toList x,y->{y})

-----------------------------------------------------------------------------
-- tex stuff


html Thing := toString

html Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(toList v,term -> (
	       if precedence term <= p
	       then ("(", html term, ")")
	       else html term));
     if # v === 0
     then (
	  if op.?EmptyName then op.EmptyName
	  else error("no method for html ", op)
	  )
     else (
	  if op.?operator then demark(op.operator,names)
	  else error("no method for html ", op)
	  )
     )

html Minus := v -> (
     term := v#0;
     if precedence term < precedence v
     then "-(" | html term | ")"
     else "-" | html term
     )

-*
texMath Divide := x -> (
     if precedence x#0 < precedence x
     then "(" | texMath x#0 | ")"
     else texMath x#0
     ) | "/" | (
     if precedence x#1 < precedence x
     then "(" | texMath x#1 | ")"
     else texMath x#1
     )
*-
html Divide := x -> (
     p := precedence x;
     a := html x#0;
     b := html x#1;
     if precedence x#0 <= p then a = "(" | a | ")";
     if precedence x#1 <= p then b = "(" | b | ")";
     a | " / " | b)

html Sum := v -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"+"));
	  seps#0 = seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus
		    then ( seps#i = "-"; v#i#0 )
		    else v#i ));
	  names := apply(n, i -> (
		    if precedence v#i <= p
		    then "(" | html v#i | ")"
		    else html v#i ));
	  concatenate (
	       mingle(seps, names)
	       )))

html Product := v -> (
     n := # v;
     if n === 0 then "1"
     else if n === 1 then html v#0
     else (
     	  p := precedence v;
     	  concatenate apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p
		    then "(" | html term | ")"
	       	    else html term
	       	    )
	       )
	  )
     )

html Superscript := v -> (
     p := precedence v;
     x := html v#0;
     y := html v#1;
     if precedence v#0 <  p then x = "(" | x | ")";
     concatenate(x,"<sup>",y,"</sup>"))

html Power := v -> (
     if v#1 === 1 then html v#0
     else (
	  p := precedence v;
	  x := html v#0;
	  y := html v#1;
	  if precedence v#0 <  p then x = "(" | x | ")";
	  concatenate(x,"<sup>",y,"</sup>")))

html Subscript := v -> (
     p := precedence v;
     x := html v#0;
     y := html v#1;
     if precedence v#0 <  p then x = "(" | x | ")";
     concatenate(x,"<sub>",y,"</sub>"))


ctr := 0
showTex = method()

showTex Thing := x -> (
     dir := temporaryFileName();
     makeDirectory dir;
     f := dir | "/show";
     f | ".tex"
     << ///\documentclass{article}
\usepackage{amsmath}
\usepackage{amssymb}
\begin{document}
///
     << tex x <<
///
\end{document}
///
     << close;
     if 0 =!= chkrun("set -x ; cd "|dir|"; latex " | f)
     then error ("latex failed on input file "|f|".tex");
     if 0 =!= chkrun("(xdvi "|f|".dvi && rm -f "|f|".tex "|f|".dvi "|f|".log "|f|".aux)&")
     then error ("xdvi failed on input file "|f|".tex");
     )
show TEX := showTex

-----------------------------------------------------------------------------
print = x -> (<< net x << endl;)
-----------------------------------------------------------------------------

File << Thing := File => (o,x) -> printString(o,net x)
List << Thing := List => (files,x) -> apply(files, o -> o << x)

o := () -> concatenate(interpreterDepth:"o")

Thing#{Standard,AfterPrint} = x -> (
     << endl;				  -- double space
     << o() << lineNumber;
     y := class x;
     << " : " << y;
     << endl;
     )

-* TODO: add an option to re-enable these two
Type#{Standard,AfterPrint} = x -> (
     << endl;				  -- double space
     << o() << lineNumber;
     y := class x;
     << " : " << y << ", with ancestors: ";
     << concatenate between_" < " drop(toString \ ancestors y, 1);
     << endl;
     )

Function#{Standard,AfterPrint} = x -> (
     Thing#{Standard,AfterPrint} x;
     briefDocumentation x;
     )
*-

Expression#{Standard,AfterPrint} = x -> (
     << endl;				  -- double space
     << o() << lineNumber << " : " << Expression << " of class " << class x << endl;
     )

-----------------------------------------------------------------------------

expression VisibleList := v -> new Holder from { apply(v, expression) }
expression Thing :=
expression Symbol :=
expression Function :=
expression Boolean := x -> new Holder from { x }

-----------------------------------------------------------------------------

Nothing#{Standard,AfterPrint} = identity
ZZ#{Standard,AfterPrint} = identity
Boolean#{Standard,AfterPrint} = identity

FilePosition = new Type of BasicList
FilePosition.synonym = "file position"
toString'(Function, FilePosition) := (fmt,i) -> concatenate(i#0,":",toString i#1,":",toString i#2)
net FilePosition := i -> concatenate(i#0,":",toString i#1,":",toString i#2)

-- extra stuff
expression Option := z -> BinaryOperation { symbol =>, unhold expression z#0, unhold expression z#1 }

SheafExpression = new WrapperType of Expression;
toString'(Function, SheafExpression) := (fmt,x) -> toString'(fmt,new FunctionApplication from { sheaf, x#0 })
net SheafExpression := x -> net x#0
expressionValue SheafExpression := x -> sheaf expressionValue x#0

moduleZERO = new ZeroExpression from { 0, Module }

-- only used by webapp.m2 at the moment. note that one can't have a symbol <---
MapExpression = new HeaderType of Expression;
toString'(Function, MapExpression) := (fmt,x) -> toString'(fmt,new FunctionApplication from { map, toSequence x })
lineOnTop := (s) -> concatenate(width s : "-") || s
net MapExpression := x-> if #x>2 then horizontalJoin(net x#0, " <--",
		    lineOnTop net x#2,
		    "-- ", net x#1) else net x#0 | " <--- " | net x#1
expressionValue MapExpression := x -> map toSequence apply(x,expressionValue)

-- moved from set.m2 because of loadsequence order
expression Set := x -> Adjacent {set, expression (sortByName keys x)}

expression HashTable := x -> (
         if hasAttribute(x,ReverseDictionary) then return new Holder from { getAttribute(x,ReverseDictionary), class x };
	 NewFromExpression { class x,
	 apply(sortByName pairs x, (k,v) -> expression k => expression v )
	 }
	 )
--expressionValue HashTable := x -> applyPairs(x, (k,v) -> (expressionValue k, expressionValue v))

expression BasicList := s -> NewFromExpression { expression class s, apply(toList s, expression) }

-- shouldn't look inside these 2
expression MutableHashTable := hold
expression MutableList := hold

-- .. but these are OK (what a mess)
expression Ring := lookup(expression,HashTable)
expression RingFamily := lookup(expression,HashTable)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
