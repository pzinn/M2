newPackage(
    "Factor",
    Version => "0.1",
    Date =>  "Jul 21, 2021", -- "Nov 29, 2020",
    Authors => {{Name => "Paul Zinn-Justin",
            Email => "pzinn@unimelb.edu.au",
            HomePage => "http://http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
    Headline => "Proper factor",
    Keywords => {"Miscellaneous"},
    DebuggingMode => true,
    AuxiliaryFiles => false,
    Configuration => { "DegreesRings" => false, "OldFactor" => true }
    -- "OldFactor" => false would be nicer but for now too many compatibility problems
    )

export {"FactorPolynomialRing","FactorLeadMonomial"}

OldFactor := (options Factor).Configuration#"OldFactor";
--FactorLeadMonomial := (options Factor).Configuration#"FactorLeadMonomial";

debug Core
factorOpts := new OptionTable from {FactorLeadMonomial=>false}; -- new options for factor
(frame factor)#0 = factorOpts;

commonPairs := (a,b,f) -> fusePairs(a,b, (x,y) -> if x === null or y === null then continue else f(x,y));
-- commonPairs should probably be defined in d for optimization purposes
subPairs := (a,b) -> fusePairs(a,b, (x,y)-> if y===null then continue else if x===null then y else if y>x then y-x else continue);
-- mergePairs could be defined similarly as
-- mergePairs := (a,b,f) -> fusePairs(a,b, (x,y) -> if x === null then y else if y === null then x else f(x,y));

FactorPolynomialRing = new Type of PolynomialRing;
FactorPolynomialRing.synonym = "factorized polynomial ring";
coefficientRing FactorPolynomialRing := R -> coefficientRing last R.baseRings;
factor FactorPolynomialRing := opts -> identity;
--expression FactorPolynomialRing := R -> if hasAttribute(R,ReverseDictionary) then expression getAttribute(R,ReverseDictionary) else (expression factor) (expression last R.baseRings)
expression FactorPolynomialRing := R -> if hasAttribute(R,ReverseDictionary) then expression getAttribute(R,ReverseDictionary) else (expression last R.baseRings)_factor
--describe FactorPolynomialRing := R -> Describe (expression factor) (describe last R.baseRings)
describe FactorPolynomialRing := R -> Describe (describe last R.baseRings)_factor
factor FractionField := opts -> F -> frac(factor(opts,last F.baseRings))  -- simpler to do it in this order -- though needs more checking (see also below)

leadCoeff := x -> ( -- iterated leadCoefficient
    R := ring x;
    if class R === PolynomialRing then leadCoeff leadCoefficient x else
    if class R === QuotientRing or class R === GaloisField then leadCoeff lift(x,ambient R) else
    x);

factor PolynomialRing := opts -> R -> (
    if R.?factor then return R.factor;
    Rf:=new FactorPolynomialRing of RingElement from R; -- not of R from R for subtle reasons: each such R gets its own addition law etc, cf enginering.m2
    R.factor=Rf; -- careful that this is symbol factor, not method factor
    Rf.baseRings=append(R.baseRings,R);
    commonEngineRingInitializations Rf;
    if Rf.?frac then remove(Rf,global frac);   -- simpler to do it in this order -- though needs more checking (see also above)
    expression Rf := a -> (expression a#0)* product apply(a#1,(f,e)->(expression f)^e);
    oldFactor := R#factor;
    if OldFactor then (
    	factor Rf := opts1 -> a -> Product apply(if a#0 == 1 then a#1 else append(a#1,(a#0,1)),u->Power u); -- emulation of old factor
	-- factor R is untouched
	) else (
    	factor Rf := opts1 -> identity;
    	factor R := a -> new Rf from a;
	);
    value Rf := a->(a#0)*product(a#1,u->(u#0)^(u#1));
    raw Rf := a-> (raw a#0)*product(a#1,u->(raw u#0)^(u#1)); -- !!!
    if (options R).Inverses then (
        denominator Rf := a -> new Rf from { (denominator a#0)*product(a#1,(f,e)->(denominator f)^e), {} };
        numerator Rf := a -> new Rf from { numerator a#0, apply(a#1,(f,e)->(numerator f,e)) };
        )
    else
    (
        denominator Rf := a -> new Rf from { denominator a#0, {} };
        numerator Rf := a -> new Rf from { numerator a#0, a#1 };
        );
    new Rf from R := (A,a) -> (
        if (options R).Inverses then (
            -- a bit of a hack if a==0, but works
            minexps:=min\transpose apply(toList (rawPairs(raw R.basering,raw a))#1,m->exponents(R.numallvars,m)); -- sadly, exponents doesn't take an optional Variables like coefficients... might wanna change that
            a=a*R_(-minexps); -- get rid of monomial in factor if a Laurent polynomial.
            c:=R_minexps;
            )
        else c = 1_R;
	-*
   	       R1 := R.basering;	
	       isSimpleNumberField := F -> isField F and instance(baseRing F, QuotientRing) and coefficientRing baseRing F === QQ and numgens baseRing F == 1 and numgens ideal baseRing F == 1; 
	       fe := if isSimpleNumberField R1 then (
		   (R', toR') := flattenRing(R, CoefficientRing=>QQ);
		   minp := (ideal R')_0;
		   ((fs,es) -> (for f in fs list raw (map(R, R', generators R|{R_0})) new R' from f, es)) (
		       rawFactor(raw toR' a, raw minp)) -- apply rawFactor, but the factors need to be converted back to R
	       ) else if instance(R1, FractionField) then (
        	   denom := lcm \\ (t -> denominator t_1) \ listForm a;
        	   baseR := (baseRing R1)(R.monoid);
		   a = (map(baseR, R, generators baseR)) (denom * a);
		   ((fs,es) -> (for i in (0..<#fs) list raw (((map(R, baseR, generators R)) new baseR from fs_i) * if i==0 then 1/denom else 1), es)) (
		       rawFactor raw a) -- similar: convert back to R, and put denom back into the leadCoefficient
	       ) else rawFactor raw a;	-- example value: ((11, x+1, x-1, 2x+3), (1, 1, 1, 1)); constant term is first, if there is one
	*-
	fe := rawFactor raw a;
        fe = toList apply append(fe,(f,e)->(
                ff:=new R from f;
                if (options R).Inverses and opts.FactorLeadMonomial and ff!=0 then (c=c*(leadMonomial ff)^e; ff=ff*(leadMonomial ff)^(-1)); -- should only be used with Inverses=>true
                if leadCoeff ff >= 0 then ff else (if odd e then c=-c; -ff),e)
            );
        if liftable(fe#0#0,R.basering) then (
            -- factory returns the possible constant factor in front
            assert(fe#0#1 == 1);
            c = c*(fe#0#0);
            fe=drop(fe,1);
            );
        { c, -- constant term
            sort fe }  -- technically the sort should be on f, not on fe -- but should be the same. warning, do not change/remove sorting, needed by mergePairs
        );
    new Rf from RawRingElement := (A,a) -> new Rf from (new R from a); -- promote uses this
    -- various redefinitions (there might be a more clever way to automate this?)
    Rf.generators=apply(generators R,a->new Rf from a);
    Rf.indexSymbols=applyValues(R.indexSymbols,x->new Rf from x);
    Rf.indexStrings=applyValues(R.indexStrings,x->new Rf from x);
    Rf#0=new Rf from { 0_R, {} };
    Rf#1=new Rf from { 1_R, {} };
    -- then operations!
    Rf * Rf := (a,b) -> if a#0===0_R or b#0===0_R then 0_Rf else new Rf from { a#0*b#0, mergePairs(a#1,b#1,plus) }; -- ha!
    Rf ^ ZZ := (a,n) -> (
        if n>0 then new Rf from { a#0^n, apply(a#1,(f,e)->(f,e*n)) } else if n===0 then 1_Rf else if a#1 =!= {} then error "division is not defined in this ring" else new Rf from {(a#0)^n,{}} -- negative value of n can only occur for constant/monomial
        );
    - Rf := a -> new Rf from { -a#0, a#1 };
    -- to avoid #321
    --    gcd (Rf, Rf) := (a,b) -> new Rf from { gcd(a#0,b#0), if a#0==0 then b#1 else if b#0==0 then a#1 else commonPairs(a#1,b#1,min) }; -- commonPairs only adds keys in both
    --    lcm (Rf, Rf) := (a,b) -> new Rf from { lcm(a#0,b#0), mergePairs(a#1,b#1,max) }; -- ha!
    gcd (Rf, Rf) := (a,b) -> new Rf from { new R from rawGCD(raw a#0,raw b#0), if a#0===0_R then b#1 else if b#0===0_R then a#1 else commonPairs(a#1,b#1,min) }; -- commonPairs only adds keys in both
    lcm (Rf, Rf) := (a,b) -> a*(b//gcd(a,b)); -- yuck (there's no rawLCM)
    Rf // Rf := (a,b) -> (
        if a#0===0_R then return 0_Rf;
        mn:=subPairs(a#1,b#1);
        mp:=subPairs(b#1,a#1);
        if mn==={} and (a#0)%(b#0)===0_R then new Rf from { (a#0)//(b#0), mp } else new Rf from ((value new Rf from {a#0,mp})//(value new Rf from {b#0,mn}))
        );
    Rf + Rf := (a,b) ->  ( c:=gcd(a,b); c*(new Rf from (value(a//c)+value(b//c))) );
    Rf - Rf := (a,b) ->  ( c:=gcd(a,b); c*(new Rf from (value(a//c)-value(b//c))) );
    Rf == Rf := (a,b) -> ( c:=gcd(a,b); value(a//c) == value(b//c) ); -- this is almost, but not quite the same as asking for equality of every factor (!)
    -- ... and map (only really useful when target ring is also factorized, or map considerably reduces complexity of polynomial)
    RingMap Rf := (p,x) -> (
        R := source p;
        S := target p;
        local pp;
        if R === ring x then pp = a -> promote(rawRingMapEval(raw p,raw a),S) else pp = a -> promote(rawRingMapEval(raw p,raw promote(a,R)),S);
        -- should perhaps test if promote is possible, else error "ring element not in source of ring map, and not promotable to it";
        (pp(x#0))*product(x#1,u->(pp(u#0))^(u#1))
        );
    -- experimental
    -*	lowestPart(ZZ,Rf) := (d,x) -> lowestPart x; -- no checking performed
    lowestPart Rf := x -> (new Rf from {x#0,{}}) * product(x#1,(f,e) -> (new Rf from lowestPart f)^e); *-
    remove(Rf,symbol vars); -- in case R had already cached its vars
    Rf
    )

frac FactorPolynomialRing := R -> if R.?frac then R.frac else (
    F := (lookup(frac,EngineRing)) R;
    -- a bunch of things need to be redefined
    new F from R := (A,a) -> fraction(a,1_R);
    new F from RawRingElement := (A,a) -> fraction(new R from rawNumerator a, new R from rawDenominator a);
    promote(R,F) := (x,F) -> new F from x;
    lift(F,R) := opts -> (f,R) -> if denominator f === 1_R then numerator f else error "cannot lift given ring element";
    numerator F := a -> a#0;
    denominator F := a -> a#1;
    value F := a-> value numerator a / value denominator a;
    raw F := a -> rawFraction(F.RawRing,raw numerator a, raw denominator a);
    fraction(R,R) := (r,s) -> (
        if s == 0 then error "division by 0";
        g:=gcd(r,s);
        if isField coefficientRing R then g=g*s#0 -- no constant in the denominator
        else if coefficientRing R === ZZ and lift(s#0,ZZ)<0 then g=-g; -- no sign in the denominator
        new F from {r//g, s//g}
        );
    fraction(F,F) := F / F := F // F := (x,y) -> fraction(numerator x*denominator y,denominator x*numerator y);
    F * F := (x,y) -> fraction(numerator x*numerator y,denominator x*denominator y);
    F + F := (x,y) -> fraction(numerator x*denominator y+numerator y*denominator x,denominator x*denominator y);
    F - F := (x,y) -> fraction(numerator x*denominator y-numerator y*denominator x,denominator x*denominator y);
    - F := x -> fraction(-numerator x, denominator x);
    F ^ ZZ := (x,n) -> if n>=0 then fraction( (numerator x)^n, (denominator x)^n ) else fraction( (denominator x)^-n, (numerator x)^-n );
    -- F == F := (x,y) -> numerator x == numerator y and denominator x == denominator y; -- only if really unique which is hopefully the case
    F == F := (x,y) -> numerator x * denominator y == numerator y * denominator x; -- safer
    F#0 = new F from { 0_R, 1_R };
    F#1 = new F from { 1_R, 1_R };
    if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
    if R.?indexSymbols then F.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,F));
    if R.?indexStrings then F.indexStrings = applyValues(R.indexStrings, r -> promote(r,F));
    if (last R.baseRings).?frac then promote((last R.baseRings).frac,F) := (x,F) -> new F from {factor numerator x,factor denominator x};
    if not OldFactor then factor F := opts1 -> identity;
    F
    )

frac PolynomialRing := R -> (
    F := (lookup(frac,EngineRing)) R;
    if R.?factor and R.factor.?frac then (
        Ff := R.factor.frac;
        F.factor = Ff;
        promote(F,Ff) := (x,Ff) -> new Ff from {factor numerator x,factor denominator x};
        );
    F
    )

newRing FactorPolynomialRing := opts -> (R) -> factor(newRing(last R.baseRings,opts)) -- TODO missing factor options

FactorPolynomialRing / Ideal := QuotientRing => (F,I) -> I.cache.QuotientRing = (last F.baseRings)/((map(last F.baseRings,F))I);
--- possibly temp: for now, we lose factoring when taking quotients


-- this is an optimization: the product would take forever
FactorPolynomialRing _ List := (R,v) -> (
    R0 := last R.baseRings;
    if (options R).Inverses then new R from { R0_v, {} }
    else new R from { 1_R, sort apply(#v, i-> (R0_i,v#i)) }
    );

-- force the use of the new factor
if not OldFactor then (
    Ring Array :=
    Ring List := (R,variables) -> (
    	RM := R monoid if instance(variables,List) then (variables,Local=>true) else variables;
	factor RM := opts -> a -> (factor (opts,RM); factor a);
    	use RM
    	)
    )

-- alternate syntax (looks prettier)
PolynomialRing _ Function := (R,f) -> if f===factor then factor R else "syntax error"

-- some functions need old factor
-- ugly hack for now, we'll see later
-*
factorOpts1 = new OptionTable from {Inverses=>false,OldFactor=>true}
-- version 1
scan({(MinimalPrimes#"private dictionary"#"factors",RingElement,value),(Core#"private dictionary"#"decompose",Ideal,identity)},
    (f,T,ff)->(
	f=value f;
	g := T#f;
	f T := F -> (
	    (frame factor)#0 = factorOpts1;
	    first(g ff F,
		(frame factor)#0 = factorOpts)
	    );
	)
    )
-- version 2
f := value MinimalPrimes#"private dictionary"#"factors"
g := RingElement#f
f RingElement := (F) -> (
(frame factor)#0 = factorOpts1;
first(g value F,
(frame factor)#0 = factorOpts)
)
*-

FactorPolynomialRing#{Standard,AfterPrint}=Thing#{Standard,AfterPrint}

if ((options Factor).Configuration#"DegreesRings") then (
    -- degrees rings
    dR0 := degreesRing {};
    degreesRing List := PolynomialRing => memoize(
     	hft -> if #hft === 0 then dR0 else factor (ZZ degreesMonoid hft));
    degreesRing ZZ := PolynomialRing => memoize( n -> if n == 0 then dR0 else factor(ZZ degreesMonoid n));
)

end

beginDocumentation()
multidoc ///
 Node
  Key
   Factor
  Headline
   A package to work with factorized expressions
  Description
   Text
    @EM Factor@ is a package that defines a new type of polynomial rings, @TO FactorPolynomialRing@. You can mostly use them
    as ordinary polynomial rings (with some caveats to be described below); the difference is that elements are displayed in
    a factorized form. In fact, they are also stored internally in a factorized form.
    [description of the package options]
  Caveat
    Some functions, most notably @TO decompose@, do not work on factorized polynomial rings,
    and require the traditional form produced by factor.
    If you need them you should leave the option "OldFactor" to its default false.
 Node
  Key
   FactorPolynomialRing
  Headline
   A polynomial ring with factorized entries
  Description
   Text
    Factorized polynomial rings are simply produced by the command @TO factor@
   Example
    R = factor ( QQ[x,y] );
    x^2-y^2   
///
