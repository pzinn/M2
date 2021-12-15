newPackage(
    "CotangentSchubert",
    AuxiliaryFiles => true,
    Version => "0.1", 
    Date => "1 Aug 2021", -- "22 Mar 2021",
    Authors => {{Name => "Paul Zinn-Justin",
            Email => "pzinn@unimelb.edu.au",
            HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
    Headline => "Cotangent Schubert calculus",
    Keywords => {"Intersection Theory"},
    PackageImports => {"VectorGraphics"},
    AuxiliaryFiles => true,
    DebuggingMode => true,
    Configuration => { "Factor" => false }
    )

if (options CotangentSchubert).Configuration#"Factor" then needsPackage "Factor" else factor PolynomialRing := opts -> identity;

opts = new OptionTable from {Ktheory => false, Equivariant => true, Generic => true} -- common options
export {"Ktheory", "Equivariant" };

load "CotangentSchubert/cotangent.m2";
load "CotangentSchubert/puzzles.m2";

beginDocumentation()
multidoc ///
 Node
  Key
   CotangentSchubert
  Headline
   A package for contangent Schubert calculus
  Description
   Text
    @BOLD "CotangentSchubert"@ is a package for calculations in cotangent Schubert calculus.
    Spectifically, it allows to compute Chern and Segre motivic classes (as well as their limits in ordinary
    Schubert calculus, namely Schubert classes), and to independently compute the expansion of their products
    using puzzles.
 Node
  Key
   setupCotangent
  Headline
   Set up cotangent Schubert calculus rings
 Node
  Key
   segreClass
  Headline
   Compute a Segre motivic class
 Node
  Key
   schubertClass
  Headline
   Compute a Schubert class
 Node
  Key
   tautClass
  Headline
   Compute the class of a tautological bundle
 Node
  Key
   puzzle
  Headline
   Compute puzzles with given boundaries
 Node
  Key
   pushforwardToPoint
  Headline
   Push forward classes to a point
 Node
  Key
   pushforwardToPointFromCotangent
  Headline
   Push forward classes to a point from the cotangent bundle
///
end

(FF,I)=setupCotangent(1,2,Ktheory=>true)
segreCls=segreClasses()
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(segreClass i * segreClass j))
L=puzzle("01","01",Generic=>true,Equivariant=>true)
fugacityVector(L,Ktheory=>true)

(AA,BB,f,I)=setupCotangent(2,4,Presentation=>Borel,Equivariant=>false,Ktheory=>true)
segreCls=segreClasses();
L=puzzle("0101","0101",Generic=>true,Equivariant=>false)
fugacityVector(L,Ktheory=>true)
(segreCls*oo)_0
segreCls_(0,1)^2

-- d=2
(FF,I)=setupCotangent(1,2,4)
segreCls=segreClasses();
i1=I#1;i2=I#2;
a=(segreClass i1)*(segreClass i2);
P=puzzle(i1,i2,Generic=>true,Equivariant=>true)
b=segreCls*(fugacityVector P);
a==b
Table table(I,I,(i1,i2)->(segreClass i1)*(segreClass i2)==segreClasses*(fugacityVector puzzle(i1,i2,Generic=>true,Equivariant=>true)))

-- note that for large examples, no need to compute the inverse
(FF,I)=setupCotangent(1,2,3,4)
segreCls=segreClasses();
i1="3021"; i2="2130"; -- interesting because separated "3 2 " * " 1 0"
a=(segreClass i1)*(segreClass i2);
P=puzzle(i1,i2,Generic=>true,Equivariant=>true)
fugacityTally P
b=segreCls*(fugacityVector P);
a==b

Q=puzzle("3 2 "," 1 0",Generic=>true,Equivariant=>true,Separated=>true)
fugacityTally Q -- not the same as P!
b=segreCls*(fugacityVector Q);
a=(segreClass "3021" - segreClass "3120")*(segreClass "2130" - segreClass "3120");
a==b

i1="2103"; i2="0321"; i3="2301"; -- interesting because shows compensations
P=puzzle(i1,i2,i3,Generic=>true,Equivariant=>true)
fug=fugacity\P;
R=QQ[ϵ,x_1..x_4]; f=map(frac R,ring first fug,{1/ϵ,x_1..x_4})
fug2=f\fug;
load "series.m2"
apply(fug2,p->p+O(ϵ^2)) -- takes too long

puzzle("5 4 3 ","210   ",Separated=>true)

