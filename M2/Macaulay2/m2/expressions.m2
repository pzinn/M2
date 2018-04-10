--		Copyright 1993-2002 by Daniel R. Grayson
-- rewritten by P. Zinn-Justin 2018

Constant = new Type of BasicList
globalAssignment Constant

precedence = method(Dispatch => Thing)
rightPrecedence = method(Dispatch => Thing)
lprec = prec = x -> (getParsing x)#0
rprec = strength2 = x -> (getParsing x)#1
uprec = strength1 = x -> (getParsing x)#2

-- local variables
EmptyName := symbol EmptyName
unit := symbol unit
operator := symbol operator
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
Expression#operator = ""

value' = method(Dispatch => Thing)
value' BasicList := x -> apply(x,value')
value' Thing := identity

-- with the following line we have no way to distinguish between "hold symbol x" and "hold x" when x has a value:
-- but without it, we have no way to recover a polynomial from its expression, without introducing Holder2 or something like it
value' Symbol := value

value Expression := value'

--Holder2 = new WrapperType of Expression			    -- Holder{ printable form, value form }
--Holder2.synonym = "holder"
--Holder = new WrapperType of Holder2			    -- Holder{ printable form, value form }, with printable form === value form
Holder = new WrapperType of Expression
Holder.synonym = "holder"

Describe = new WrapperType of Expression
Describe.synonym = "description"
describe = method()
describe Thing := x -> Describe expression x
net Describe := x -> net x#0
toString Describe := x -> toString x#0
value' Describe := x -> value' x#0
texMath Describe := x -> texMath x#0
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
toString'(Function,Thing) := (toString,x) -> toString x
toString Expression := v -> toString'(toString,v)

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
     if # v === 0 then op#EmptyName
     else demark(op#operator,names)
     )

--texMath Holder2 := v -> "{" | texMath v#0 | "}"
--html Holder2 := v -> html v#0
--net Holder2 := v -> net v#0

texMath Holder := v -> texMath v#0
html Holder := v -> html v#0
net Holder := v -> net v#0

--toString'(Function, Holder2) := (fmt,v) -> fmt v#0
toString'(Function, Holder) := (fmt,v) -> fmt v#0

remove(Sequence,expression)

Minus = new WrapperType of Expression		  -- unary minus
Minus.synonym = "minus expression"

Minus#operator = "-"
value' Minus := v -> minus apply(toSequence v,value')
toString'(Function, Minus) := (fmt,v) -> (
     term := v#0;
     if precedence term > precedence v or class term === Product
     then "-" | fmt term
     else "-(" | fmt term | ")"
     )

Equation = new HeaderType of AssociativeExpression
Equation.synonym = "equation expression"
Equation#operator = "=="
value' Equation := (v) -> (
     v = apply(toSequence v,value');
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
-----------------------------------------------------------------------------
OneExpression = new Type of Holder
OneExpression.synonym = "one expression"
ONE = new OneExpression from {1}
-----------------------------------------------------------------------------
Parenthesize = new WrapperType of Expression
Parenthesize.synonym = "possibly parenthesized expression"
net Parenthesize := net @@ first
toString'(Function, Parenthesize) := (fmt,v) -> fmt v#0
value' Parenthesize := first
texMath Parenthesize := texMath @@ first
-----------------------------------------------------------------------------
Sum = new WrapperType of AssociativeExpression
Sum.synonym = "sum expression"

Sum#unit = ZERO
Sum#EmptyName = "0"
Sum#operator = "+"
value' Sum := v -> plus apply(toSequence v,value')

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
Product#EmptyName = "1"
Product#operator = "*"
value' Product := v -> times apply(toSequence v,value')

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
NonAssociativeProduct#EmptyName = "1"
NonAssociativeProduct#operator = "**"
value' NonAssociativeProduct := v -> times apply(toSequence v,value')

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
Divide#operator = "/"
value' Divide := (x) -> (value' x#0) / (value' x#1)
numerator Divide := x -> x#0
denominator Divide := x -> x#1

Power = new HeaderType of Expression
Power.synonym = "power expression"
Power#operator = "^"
value' Power := (x) -> (value' x#0) ^ (value' x#1)

Subscript = new HeaderType of Expression
Subscript.synonym = "subscript expression"
Subscript#operator = "_"
value' Subscript := (x) -> (value' x#0)_(value' x#1)

Superscript = new HeaderType of Expression
Superscript.synonym = "superscript expression"
Superscript#operator = "^"
value' Superscript := (x) -> (value' x#0)^(value' x#1)

toString'(Function, Subscript) := toString'(Function, Superscript) := (fmt,v) -> (
     x := fmt v#0;
     y := fmt v#1;
     p := precedence v;
     if precedence v#0 <  p then x = "(" | x | ")";
     if precedence v#1 <= p then y = "(" | y | ")";
     concatenate(x,(class v)#operator,y))

toString'(Function, Power) := (fmt,v) -> (
     x := v#0;
     y := v#1;
     if y === 1 then fmt x 
     else (
	  x = fmt x;
	  y = fmt y;
	  if precedence v#0 <  prec symbol ^  then x = "(" | x | ")";
	  if precedence v#1 <= prec symbol ^  then y = "(" | y | ")";
	  concatenate(x,(class v)#operator,y)))

-----------------------------------------------------------------------------
RowExpression = new HeaderType of Expression
RowExpression.synonym = "row expression"
net RowExpression := w -> horizontalJoin apply(toList w,net)
html RowExpression := w -> concatenate apply(w,html)
texMath RowExpression := w -> concatenate apply(w,texMath)
toString'(Function, RowExpression) := (fmt,w) -> concatenate apply(w,fmt)
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
ColumnExpression = new HeaderType of Expression
ColumnExpression.synonym = "row expression"
net ColumnExpression := w -> stack apply(toList w,net)
--html ColumnExpression := w -> concatenate apply(w,html)
--texMath ColumnExpression := w -> concatenate apply(w,texMath)
--toString'(Function, ColumnExpression) := (fmt,w) -> concatenate apply(w,fmt)
-----------------------------------------------------------------------------
Adjacent = new HeaderType of Expression
Adjacent.synonym = "adjacent expression"
value' Adjacent := x -> (value' x#0) (value' x#1)
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
Expression + Holder         := (x,y) -> new Sum from {x,y#0}
Holder     + Expression     := (x,y) -> new Sum from {x#0,y}
Holder     + Holder         := (x,y) -> new Sum from {x#0,y#0}
Expression + Thing          := (x,y) -> x + expression y
     Thing + Expression     := (x,y) -> expression x + y
       - ZeroExpression     := identity
	   - Minus          := x -> expression x#0
           - Expression     := x -> new Minus from {x}
           - Holder         := x -> new Minus from {x#0}
Expression - Expression     := Sum => (x,y) -> x + -y
Expression - Thing          := (x,y) -> x - expression y
     Thing - Expression     := (x,y) -> expression x - y
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
Holder     * Expression := (x,y) -> new Product from {x#0,y}
Expression * Holder     := (x,y) -> new Product from {x,y#0}
Holder     * Holder     := (x,y) -> new Product from {x#0,y#0}
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
Holder     Expression := (x,y) -> new Adjacent from {x#0,y}
Expression Holder     := (x,y) -> new Adjacent from {x,y#0}
Holder     Holder     := (x,y) -> new Adjacent from {x#0,y#0}
     -- are lists expressions, too???
Expression Thing      := (x,y) -> x (expression y)
     Thing Expression := (x,y) -> (expression x) y
Expression ** NonAssociativeProduct := prepend
Holder     ** NonAssociativeProduct := prepend0
Expression ** Expression := NonAssociativeProduct => (x,y) -> new NonAssociativeProduct from {x,y}
Holder     ** Expression := (x,y) -> new NonAssociativeProduct from {x#0,y}
Expression ** Holder     := (x,y) -> new NonAssociativeProduct from {x,y#0}
Holder     ** Holder     := (x,y) -> new NonAssociativeProduct from {x#0,y#0}
Expression ** Thing      := (x,y) -> x ** (expression y)
     Thing ** Expression := (x,y) -> (expression x) ** y
Holder     / OneExpression :=
Expression / OneExpression := (x,y) -> x
Expression / Expression := Divide => (x,y) -> new Divide from {x,y}
Holder     / Expression := (x,y) -> new Divide from {x#0,y}
Expression / Holder     := (x,y) -> new Divide from {x,y#0}
Holder     / Holder     := (x,y) -> new Divide from {x#0,y#0}
Expression / Thing      := (x,y) -> x / (expression y)
     Thing / Expression := (x,y) -> (expression x) / y
not Equation := e -> if #e == 2 then BinaryOperation { symbol !=, e#0, e#1 } else -* UnaryOperation{symbol not, e} *- error ("negation of an equation with ", toString (#e), " parts")
-- not Expression := e -> BinaryOperation{symbol not, e}
Expression and Expression := (e,f) -> BinaryOperation{symbol and,e,f}
Expression or Expression := (e,f) -> BinaryOperation{symbol or,e,f}
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
Expression ^ ZeroExpression := (x,y) -> ONE
ZeroExpression ^ Holder     :=
ZeroExpression ^ Expression := (x,y) -> ZERO
ZeroExpression ^ ZeroExpression := (x,y) -> ONE
Expression ^ Expression := Power => (x,y) -> Power{x,y}
Holder     ^ Expression := (x,y) -> Power{x#0,y}
Expression ^ Holder     := (x,y) -> Power{x,y#0}
Holder     ^ Holder     := (x,y) -> Power{x#0,y#0}
Expression ^ Thing      := (x,y) -> x ^ (expression y)
     Thing ^ Expression := (x,y) -> (expression x) ^ y
Expression _ Expression := Subscript => (x,y) -> Subscript{x,y}
Holder     _ Expression := (x,y) -> Subscript{x#0,y}
Expression _ Holder     := (x,y) -> Subscript{x,y#0}
Holder     _ Holder     := (x,y) -> Subscript{x#0,y#0}
Expression _ Thing      := (x,y) -> x _ (expression y)
     Thing _ Expression := (x,y) -> (expression x) _ y

Expression : Expression := (x,y) -> BinaryOperation{symbol :,x,y}
Holder     : Expression := (x,y) -> BinaryOperation{symbol :,x#0,y}
Expression     : Holder := (x,y) -> BinaryOperation{symbol :,x,y#0}
Holder         : Holder := (x,y) -> BinaryOperation{symbol :,x#0,y#0}
Thing      : Expression := (x,y) -> BinaryOperation{symbol :,x,y}
Expression     :  Thing := (x,y) -> BinaryOperation{symbol :,x,y}

Holder     .. Expression := (x,y) -> BinaryOperation{symbol ..,x#0,y}
Expression     .. Holder := (x,y) -> BinaryOperation{symbol ..,x,y#0}
Holder         .. Holder := (x,y) -> BinaryOperation{symbol ..,x#0,y#0}
InfiniteNumber .. InfiniteNumber :=
InfiniteNumber .. ZZ             :=
ZZ             .. InfiniteNumber := (x,y) -> if x < y then (
     error "infinite range requested";
     -- BinaryOperation{symbol ..,x,y}
     ) else ()
Expression     .. Expression     :=
Thing          .. Expression     :=
Expression     .. Thing          := (x,y) -> BinaryOperation{symbol ..,x,y}

Holder     ..< Expression := (x,y) -> BinaryOperation{symbol ..<,x#0,y}
Expression     ..< Holder := (x,y) -> BinaryOperation{symbol ..<,x,y#0}
Holder         ..< Holder := (x,y) -> BinaryOperation{symbol ..<,x#0,y#0}
InfiniteNumber ..< InfiniteNumber :=
InfiniteNumber ..< ZZ             :=
ZZ             ..< InfiniteNumber := (x,y) -> if x < y then (
     error "infinite range requested";
     -- BinaryOperation{symbol ..<,x,y}
     ) else ()
Expression     ..< Expression     :=
Thing          ..< Expression     :=
Expression     ..< Thing          := (x,y) -> BinaryOperation{symbol ..<,x,y}

-----------------------------------------------------------------------------
--value' Holder2 := x -> x#1
--value' Holder := x -> x#1
value' Holder := x -> value' x#0 -- !!!
value' OneExpression := v -> 1
value' ZeroExpression := v -> 0
-----------------------------------------------------------------------------
SparseVectorExpression = new HeaderType of Expression
SparseVectorExpression.synonym = "sparse vector expression"
value' SparseVectorExpression := x -> notImplemented()
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
value' SparseMonomialVectorExpression := x -> notImplemented()
toString'(Function, SparseMonomialVectorExpression) := (fmt,v) -> toString (
     sum(v#1,(i,m,a) -> 
	  expression a * 
	  expression m * 
	  hold concatenate("<",fmt i,">"))
     )
-----------------------------------------------------------------------------
MatrixExpression = new HeaderType of Expression
MatrixExpression.synonym = "matrix expression"
value' MatrixExpression := x -> matrix applyTable(toList x,value')
toString'(Function,MatrixExpression) := (fmt,m) -> concatenate(
     "MatrixExpression {",		  -- ????
     between(",",apply(toList m,row->("{", between(",",apply(row,fmt)), "}"))),
     "}" )
-----------------------------------------------------------------------------
Table = new HeaderType of Expression
Table.synonym = "table expression"
value' Table := x -> applyTable(toList x,value')
toString'(Function, Table) := (fmt,m) -> concatenate(
     "Table {",
     between(",",apply(toList m,row->("{", between(",",apply(row,fmt)), "}"))),
     "}" )
-----------------------------------------------------------------------------

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
     symbol not => ((x,y) -> x not y),
     symbol or => ((x,y) -> x or y)
     }

keywordTexMath := new HashTable from { -- both unary and binary keywords
    -*
    symbol # => "\\# ",
    symbol ^ => "{^\\wedge}",
    symbol % => "\\% ",
    symbol & => "\\& ",
    symbol ^^ => "{^{\\wedge\\wedge}}"
    symbol == => "=", -- ??
    *-
    symbol |- => "\\vdash ",
    symbol .. => "\\,{.}{.}\\, ",
    symbol ..< => "\\,{.}{.}{<}\\, ",
    symbol => => "\\Rightarrow ",
    symbol <= => "\\Leftarrow ",
    symbol ==> => "\\Longrightarrow ",
    symbol <== => "\\Longleftarrow ",
    symbol <==> => "\\Longleftrightarrow ",
    symbol ** => "\\otimes ",
    symbol ++ => "\\oplus ",
    symbol != => "\\ne ",
    symbol = => "=",
    symbol -> => "\\rightarrow ",
    symbol <- => "\\leftarrow ",
    symbol ===> => "{\\large\\Longrightarrow}",
    symbol <=== => "{\\large\\Longleftarrow}",
    symbol << => "\\ll ",
    symbol >> => "\\gg ",
    symbol ~ => "\\sim ",
    symbol ^** => "^{\\otimes}",
    symbol _ => "\\_ ",
    symbol | => "|",
    symbol || => "||",
    symbol * => "*",
    symbol + => "+",
    symbol - => "-",
    symbol / => "/",
    symbol // => "//",
    symbol { => "\\{ ",
    symbol { => "\\} ",
    symbol \ => "\\backslash ",
    symbol \\ => "\\backslash\\backslash ",
    symbol : => ":",
    symbol ; => ";"
    }

texMath Keyword := x -> if keywordTexMath#?x then keywordTexMath#x else texMath toString x

BinaryOperation = new HeaderType of Expression -- {op,left,right}
BinaryOperation.synonym = "binary operation expression"
value' BinaryOperation := (m) -> (
     if binaryOperatorFunctions#?(m#0) then binaryOperatorFunctions#(m#0) (value' m#1,value' m#2) else m
     )
net BinaryOperation := m -> (
     x := net m#1;
     y := net m#2;
     if rightPrecedence m#1 < lprec m#0 then x = bigParenthesize x;
     if precedence m#2 <= rprec m#0 then y = bigParenthesize y;
     if m#?3 then horizontalJoin( x, m#3, toString m#0, m#3, y ) else horizontalJoin( x, toString m#0, y ) -- allow for optional separator
     )

texMath BinaryOperation := m -> (
     x := texMath m#1;
     y := texMath m#2;
     if rightPrecedence m#1 < lprec m#0 then x = "\\left(" | x | "\\right)";
     if precedence m#2 <= rprec m#0 then y = "\\left(" | y | "\\right)";
     if m#?3 then concatenate( x, replace(" ","\\,",m#3), texMath m#0, replace(" ","\\,",m#3), y ) else concatenate( x, texMath m#0, y )
     )

toString'(Function, BinaryOperation) := (fmt,m) -> (
     x := fmt m#1;
     y := fmt m#2;
     if rightPrecedence m#1 < lprec m#0 then x = ("(",x,")");
     if precedence m#2 <= rprec m#0 then y = ("(",y,")");
     if m#?3 then concatenate( x, m#3, toString m#0, m#3, y ) else concatenate( x, toString m#0, y ) -- allow for optional separator
     )

-----------------------------------------------------------------------------
FunctionApplication = new HeaderType of Expression -- {fun,args}
FunctionApplication.synonym = "function application expression"
value' FunctionApplication := (m) -> (value' m#0) (value' m#1)
toString'(Function, Adjacent) := toString'(Function, FunctionApplication) := (fmt,m) -> (
     p := precedence m;
     fun := m#0;
     args := m#1;
     if class args === Sequence
     then if #args === 1
     then concatenate(fmt fun, "(", fmt args#0, ")")  -- f (1:x)
     else concatenate(fmt fun, fmt args)       -- f(x,y) or f(), ...
     else if precedence args >= p
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
     netfun := net fun;
     div := instance(fun,Divide);
     pfun := if div then strength1 symbol symbol else precedence fun;
     args := m#1;
     if instance(args,Array) or (class args === Holder and instance(args#0,Array)) then (p = p-1; div = true; );
     -- sometimes Lists are wrapped, sometimes they aren't
     netargs := net args;
     if precedence args >= p
     then if pfun > p
     then (
	  if div or class netfun === Net and netfun#?0 and width netfun > width netfun#0
	  then horizontalJoin (netfun, netargs)
	  else horizontalJoin (netfun, " ", netargs)
	  )
     else horizontalJoin (bigParenthesize netfun, netargs)
     else if pfun > p
     then horizontalJoin (netfun, bigParenthesize netargs)
     else horizontalJoin (bigParenthesize netfun, bigParenthesize netargs)
     )
texMath Adjacent := texMath FunctionApplication := m -> (
     p := precedence m;
     fun := m#0;
     div := instance(fun,Divide);
     pfun := if div then strength1 symbol symbol else precedence fun;
     args := m#1;
     -- we can finesse further the amount of space than in net
     sep := "\\ ";
     if instance(args,Array) or (class args === Holder and instance(args#0,Array)) then (p = p-1; div = true; )
     else if instance(args,VisibleList) or (class args === Holder and instance(args#0,VisibleList)) then sep="\\,";
     -- sometimes Lists are wrapped, sometimes they aren't
     if precedence args >= p
     then if pfun > p then (
	 if div
	 then concatenate (texMath fun, texMath args)
	 else concatenate (texMath fun, sep, texMath args)
	 )
     else concatenate ("\\left(", texMath fun, "\\right)", texMath args)
     else if pfun > p
     then concatenate (texMath fun, "\\left(", texMath args, "\\right)")
     else concatenate ("\\left(",texMath fun,"\\right)\\left(", texMath args, "\\right)")
     )
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
   precedence FunctionApplication := returns prec symbol SPACE
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
                precedence Ring := returns prec symbol ^ -- sort of temp. basically takes care of ZZ, QQ...
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
--	  else if #x === 1 then ("1 : (", net x#0, ")") -- ugly
	  else (toSequence between(",",apply(x,net)))))

nopars := x -> if class x === Sequence then nopar x else net x

net Subscript := x -> (
     n := nopars x#1;
     if precedence x#0 < prec symbol ^
     then horizontalJoin( bigParenthesize net x#0, n^-(height n) )
     else net x#0 | n^-(height n)
     )

net Superscript := x -> (
     n := net x#1;
     if precedence x#0 < prec symbol ^
     then horizontalJoin( bigParenthesize net x#0, n^(1+depth n))
     else net x#0 | n^(1+depth n)
     )

expectExponent = n -> if height n < 2 then n = (stack( 2 - height n : "", n))^1 else n

net Power := v -> (
     x := v#0;
     y := v#1;
     if y === 1 then net x
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

net MatrixExpression := x -> (
     if # x === 0 or # (x#0) === 0 then "|  |"
     else (
	  m := net Table toList x;
	  side := "|" ^ (height m, depth m);
	  horizontalJoin(side," ",m," ",side)))
html MatrixExpression := x -> html TABLE toList x

-----------------------------------------------------------------------------
-- tex stuff

texMath Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(toList v,term -> (
	       if precedence term <= p
	       then ("{\\left(", texMath term, "\\right)}")
	       else ("{", texMath term, "}") ) );
     if # v === 0 then (
	  if op#?EmptyName then op#EmptyName
	  else error("no method for texMath ", op)
	  )
     else (
	  if op#?operator then demark(op#operator,names)
	  else error("no method for texMath ", op)
	  )
     )

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
	  if op#?EmptyName then op#EmptyName
	  else error("no method for html ", op)
	  )
     else (
	  if op#?operator then demark(op#operator,names)
	  else error("no method for html ", op)
	  )
     )

texMath Minus := v -> (
     term := v#0;
     if precedence term < precedence v
     then "{-(" | texMath term | ")}"
     else "{-" | texMath term | "}"
     )

html Minus := v -> (
     term := v#0;
     if precedence term < precedence v
     then "-(" | html term | ")"
     else "-" | html term
     )

texMath Divide := x -> "\\frac{" | texMath x#0 | "}{" | texMath x#1 | "}"

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

html OneExpression := html ZeroExpression :=
texMath OneExpression := texMath ZeroExpression := toString

texMath Sum := v -> (
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
		    then "(" | texMath v#i | ")"
		    else texMath v#i ));
	  concatenate mingle ( seps, names )))

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

texMath Product := v -> (
     n := # v;
     if n === 0 then "1"
     else if n === 1 then texMath v#0
     else (
     	  p := precedence v;
	  nums := apply(v, x -> isNumber x or (class x === Power and isNumber x#0 and (x#1 === 1 or x#1 === ONE)));
	  seps := apply (n-1, i-> if nums#i and nums#(i+1) then "\\cdot " else "\\,");
     	  boxes := apply(v,
		    term -> (
			 if precedence term <= p and class expression term =!= Divide
			 then "\\left(" | texMath term | "\\right)"
			 else texMath term
			 )
		    );
	  concatenate splice mingle (boxes,seps)
	  )
      )

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

texMath Power := v -> (
     if v#1 === 1 then texMath v#0
     else (
	  p := precedence v;
	  x := texMath v#0;
	  y := texMath v#1;
	  if precedence v#0 <  p then x = "\\left({" | x | "}\\right)";
	  concatenate("{",x,"}",(class v)#operator,"{",y,"}")))

texMath Subscript := texMath Superscript := v -> ( -- there is a precedence issue, compare with net Superscript
--     p := precedence v;
     x := texMath v#0;
     if class v#1 === Sequence then y:=demark(",", apply(v#1,texMath)) else y = texMath v#1;
--     if precedence v#0 <  p then x = "\\left(" | x | "\\right)";
     if precedence v#0 <  prec symbol ^ then x = "\\left(" | x | "\\right)";
     concatenate("{",x,"}",(class v)#operator,"{",y,"}"))

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

texMath SparseVectorExpression := v -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=texMath x);
     concatenate("\\begin{pmatrix}", between("\\\\ ",w),"\\end{pmatrix}")
     )

texMath SparseMonomialVectorExpression := v -> (
     texMath sum(v#1,(i,m,a) ->
	  expression a *
	  expression m *
	  hold concatenate("<",toString i,">"))
     )

texMath VerticalList := s -> concatenate(
    "\\left\\{\\begin{array}{l}",
    between("\\\\",apply(toList s,texMath))
    ,"\\end{array}\\right\\}"
    )

texMath Table := m -> (
    if m#?0 then concatenate(
	"{\\begin{array}{", #m#0: "c", "}", newline,
	apply(m, row -> (between("&",apply(row,texMath)), ///\\///|newline)),
	"\\end{array}}")
)
	
texMath MatrixExpression := m -> (
     if m#?0 then if #m#0>10 then "{\\left(" | texMath(new Table from toList m) | "\\right)}" -- the extra {} is to discourage line breaks
     else concatenate(
      	      "\\begin{pmatrix}" | newline,
     	      between(///\\/// | newline, apply(toList m, row -> concatenate between("&",apply(row,texMath)))),
	      newline | "\\end{pmatrix}" -- notice the absence of final \\ -- so lame
	      )
	  )

ctr := 0
showTex = method(
     Options => {
	  Format => "dvi", -- or "pdf", not implemented yet
	  }
     )

showTex Thing := o -> x -> (
     f := temporaryFileName();
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
     if 0 === chkrun("cd /tmp; latex " | f)
     then chkrun("(xdvi "|f|".dvi; rm -f "|f|".tex "|f|".dvi "|f|".log "|f|".aux)&")
     else error ("latex failed on input file " | f | ".tex")
     )

-----------------------------------------------------------------------------
print = x -> (<< net x << endl;)
-----------------------------------------------------------------------------
texMath RR := toString
texMath ZZ := toString
tex Thing := x -> concatenate("$",texMath x,"$")
texMath Thing := texMath @@ net -- if we're desperate (in particular, for raw objects)
--texMath Symbol := toString -- the simplest version
-- next version is a horrible hack
--texMath Symbol := x -> ( xx := value x; if instance(xx,HashTable) and xx.?texMath then xx.texMath else toString x)
bbletters := set characters "kABCDEFGHIJKLMNOPQRSTUVWXYZ"
greekletters := set {"alpha","beta","gamma","delta","epsilon","varepsilon","zeta","eta","theta","vartheta","iota","kappa","lambda","mu","nu","xi","pi","varpi","rho","varrho","sigma","varsigma","tau","upsilon","phi","varphi","chi","psi","omega","Gamma","Delta","Theta","Lambda","Xi","Pi","Sigma","Upsilon"}
texVariable := x -> (
    if #x === 2 and x#0 === x#1 and bbletters#?(x#0) then return "{\\mathbb "|x#0|"}"; -- effectively, makes ZZ.texMath obsolete
    if last x === "'" then return texVariable substring(x,0,#x-1) | "'";
    if #x > 3 and substring(x,-3) === "bar" then return "\\bar{"|texVariable substring(x,0,#x-3)|"}";
    if greekletters#?x then return "{\\"|x|"}";
    if #x === 1 then x else "\\textit{"|x|"}"
    )
texMath Symbol := x -> texVariable toString x;


File << Thing := File => (o,x) -> printString(o,net x)
List << Thing := List => (files,x) -> apply(files, o -> o << x)

o := () -> concatenate(interpreterDepth:"o")

symbol briefDocumentation <- identity			    -- temporary assignment

Thing#{Standard,AfterPrint} = x -> (
     << endl;				  -- double space
     << o() << lineNumber;
     y := class x;
     << " : " << y;
     << endl;
     )

-- Type#{Standard,AfterPrint} = x -> (
--      << endl;				  -- double space
--      << o() << lineNumber;
--      y := class x;
--      << " : " << y << ", with ancestors:";
--      while ( y = parent y; << " " << y; y =!= Thing ) do ();
--      << endl;
--      )

-*
Function#{Standard,AfterPrint} = x -> (
     Thing#{Standard,AfterPrint} x;
     -- briefDocumentation x; -- from now on, type "?foo" to get brief documentation on foo
     )
*-
Expression#{Standard,AfterPrint} = x -> (
     << endl;				  -- double space
     << o() << lineNumber << " : " << Expression << " of class " << class x << endl;
     )



-----------------------------------------------------------------------------

expression VisibleList := v -> new Holder from {apply(v,expression)}
expression Thing := x -> new Holder from { if hasAttribute(x,ReverseDictionary) then getAttribute(x,ReverseDictionary) else x }
expression Symbol := x -> new Holder from { x }
expression Function := x -> new Holder from { x }
expression Boolean := x -> new Holder from { x }

-----------------------------------------------------------------------------

? Function := x -> (briefDocumentation x;)

Nothing#{Standard,AfterPrint} = identity
ZZ#{Standard,AfterPrint} = identity
Boolean#{Standard,AfterPrint} = identity

FilePosition = new Type of BasicList
FilePosition.synonym = "file position"
toString'(Function, FilePosition) := (fmt,i) -> concatenate(i#0,":",toString i#1,":",toString i#2)
net FilePosition := i -> concatenate(i#0,":",toString i#1,":",toString i#2)

-- extra stuff
expression Option := z -> BinaryOperation { symbol =>, expression z#0, expression z#1, " " }
net Option := net @@ expression
texMath Option := texMath @@ expression
toString Option := toString @@ expression

-- needed because can't really have a symbol <---
MapArrow = new HeaderType of Expression;
toString MapArrow := x-> toString(x#0) | " <--- " | toString(x#1)
net MapArrow := x-> net(x#0) | " <--- " | net(x#1)
texMath MapArrow := x -> texMath(x#0) | "\\,\\longleftarrow\\," | texMath(x#1)

-- moved from set.m2 because of loadsequence order
expression Set := x -> Adjacent {set, expression (sortByName keys x)}
toString Set := toString @@ expression
net Set := net @@ expression
texMath Set := x -> if x.?texMath then x.texMath else texMath expression x

-*
-- useless -- nobody uses expression HashTable at the moment because it's not semantically correct :(
-- plus creates all kinds of complications with subclasses
expression HashTable := x -> (
         if hasAttribute(x,ReverseDictionary) then return expression getAttribute(x,ReverseDictionary);
	 new Holder from { applyPairs(x, (k,v) -> (expression k, expression v) ) }
	 )
value' HashTable := x -> applyPairs(x, (k,v) -> (value' k, value' v))
expression Type := x -> new Holder from { x }
*-

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
