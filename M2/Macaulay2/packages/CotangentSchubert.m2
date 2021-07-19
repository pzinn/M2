newPackage(
    "CotangentSchubert",
    AuxiliaryFiles => true,
    Version => "0.1", 
    Date => "22 Mar 2021",
    Authors => {{Name => "Paul Zinn-Justin",
            Email => "pzinn@unimelb.edu.au",
            HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
    Headline => "Cotangent Schubert calculus",
    Keywords => {"Schubert","motivic","CSM"},
    PackageImports => {"Factor", "VectorGraphics"},
    AuxiliaryFiles => true,
    DebuggingMode => true
    )

opts = new OptionTable from {Kth => false, Equivariant => true, Generic => true} -- common options
export {"Kth", "Equivariant" };

load "CotangentSchubert/cotangent.m2";
load "CotangentSchubert/puzzles.m2";

beginDocumentation()
multidoc ///
///
end

(FF,I)=setupCotangent(1,2,Kth=>true)
segreCls=segreClasses()
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(segreClass i @ segreClass j))
L=puzzle("01","01",Generic=>true,Equivariant=>true)
fugacityVector(L,Kth=>true)

(AA,BB,f,I)=setupCotangent(2,4,Presentation=>Borel,Equivariant=>false,Kth=>true)
segreCls=segreClasses();
L=puzzle("0101","0101",Generic=>true,Equivariant=>false)
fugacityVector(L,Kth=>true)
(segreCls*oo)_0
segreCls_(0,1)^2

-- d=2
(FF,I)=setupCotangent(1,2,4)
segreCls=segreClasses();
i1=I#1;i2=I#2;
a=(segreClass i1)@(segreClass i2);
P=puzzle(i1,i2,Generic=>true,Equivariant=>true)
b=segreCls*(fugacityVector P);
a==b
Table table(I,I,(i1,i2)->(segreClass i1)@(segreClass i2)==segreClasses*(fugacityVector puzzle(i1,i2,Generic=>true,Equivariant=>true)))

-- note that for large examples, no need to compute the inverse
(FF,I)=setupCotangent(1,2,3,4)
segreCls=segreClasses();
i1="3021"; i2="2130"; -- interesting because separated "3 2 " * " 1 0"
a=(segreClass i1)@(segreClass i2);
P=puzzle(i1,i2,Generic=>true,Equivariant=>true)
fugacityTally P
b=segreCls*(fugacityVector P);
a==b

Q=puzzle("3 2 "," 1 0",Generic=>true,Equivariant=>true,Separated=>true)
fugacityTally Q -- not the same as P!
b=segreCls*(fugacityVector Q);
a=(segreClass "3021" - segreClass "3120")@(segreClass "2130" - segreClass "3120");
a==b

i1="2103"; i2="0321"; i3="2301"; -- interesting because shows compensations
P=puzzle(i1,i2,i3,Generic=>true,Equivariant=>true)
fug=fugacity\P;
R=QQ[ϵ,x_1..x_4]; f=map(frac R,ring first fug,{1/ϵ,x_1..x_4})
fug2=f\fug;
load "series.m2"
apply(fug2,p->p+O(ϵ^2)) -- takes too long

puzzle("5 4 3 ","210   ",Separated=>true)

