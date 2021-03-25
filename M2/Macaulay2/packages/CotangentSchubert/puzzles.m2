dir := currentFileDirectory;

Puzzle = new Type of HashTable;

myload := x -> value get (CotangentSchubert#"source directory"|"CotangentSchubert/Puzzles/"|x) -- ewww

-- lots of global variables, not thread-safe!
myload "states.m2"

basicTriangles := apply(5, d-> if d==4 then {} else ( -- the procedure below only works for d<=3
        l := new MutableList;
        -- single digit triangles
        scan(0..d, i -> l#(#l) = { toString i, toString i, toString i });
        -- combos
        table(states#d,states#d, (a,b) -> (
                c := (if #b == 1 then b else "(" | b | ")") |  (if #a == 1 then a else "(" | a | ")");
                if member(c,states#d) then (
                    l#(#l) = {a,b,c};
                    l#(#l) = {c,a,b};
                    l#(#l) = {b,c,a};
                )));
        new List from l
        ));

-- TODO: move somewhere else. create on the fly?
koganStates1 = apply(10, d -> {" "} | apply(toList(0..d), toString));
koganStates2 = apply(10, d -> apply(toList(0..d), toString) | flatten apply(toList(0..d), j->apply(toList(0..j-1),i->toString i | toString j)));
koganTriangles = apply(10, d -> apply(toList(0..d),i->{" ",toString i,toString i}) | apply(toList(0..d),i->{toString i," ",toString i})
    | flatten apply(toList(0..d), j->apply(toList(0..j-1),i->{toString i,toString j,toString i | toString j})));

opts := {Steps => false, Kth => false, Kinv => false, Equivariant => false, Generic => false, Separated => false, Labels => true, Paths => false};
export {"Steps", "Kth", "Kinv", "Equivariant", "Separated", "Size", "Labels", "Paths"};
KUp=KDown=allTriangles=allRhombi=equivRhombi=upTriangles=downTriangles=rhombi={};
apply({rhombusStyle,downStyle,upStyle},protect);

prevopts := opts;

tiles := o -> (
    if o === prevopts then return;
    if debugLevel>0 then << "rebuilding tiles" << newline;
    prevopts = o;
    d := o.Steps;
    if o#Separated then (
        if not koganTriangles#?d then error "this value of d not implemented";
        upTriangles = downTriangles = koganTriangles#d;
        rhombi = if o#Equivariant then if o#Generic then apply(koganStates1#d,i->{i,i,i,i}) else {{" "," "," "," ","fill"=>"gray"}} else {}; -- actually, Generic makes no diff!!!
	if o#Generic then upTriangles = downTriangles = select(flatten table(koganStates1#d,koganStates1#d, (a,b) -> {a,b,if a==" " then b else if b==" " then a else concatenate sort {a,b}}), tri -> member(tri#2,koganStates2#d)) else (
            if o#Kth =!= false then upTriangles = upTriangles | toList splice table(0..o#Kth,o#Kth+1..d,(i,j)->{toString j,toString i,toString i|toString j,"fill"=>"yellow"});
            if o#Kinv =!= false then downTriangles = downTriangles | toList splice table(0..o#Kinv,o#Kinv+1..d,(i,j)->{toString j,toString i,toString i|toString j,"fill"=>"yellow"});
	    -- what's going on here?? what's the value of o#Kth for? shouldn't it be detected automatically?
	    );
        return;
        ); 
    if not basicTriangles#?d then error "this value of d not implemented";
    upTriangles = downTriangles = basicTriangles#d;
    rhombi = {};
    if o#Kth then (
        if #KUp === 0 or #KDown === 0 then (KUp,KDown) = myload "K.m2";
        if not KUp#?d or not KDown#?d then error "K-tiles not implemented" else (
        upTriangles = upTriangles | apply(KUp#d, x -> append(x,"fill"=>"yellow"));
        downTriangles = downTriangles | apply(KDown#d, x -> append(x,"fill"=>"yellow"));
        ));
    if o#Kinv then (
        if #KUp === 0 or #KDown === 0 then (KUp,KDown) = myload "K.m2";
        if not KUp#?d or not KDown#?d then error "K-tiles not implemented" else (
        upTriangles = upTriangles | apply(KDown#d, x -> append(x,"fill"=>"yellow"));
        downTriangles = downTriangles | apply(KUp#d, x -> append(x,"fill"=>"yellow"));
        ));
    if o#Equivariant and not o#Generic then (
        if #equivRhombi === 0 then equivRhombi = myload "equiv.m2";
        if not equivRhombi#?d then error "equivariant tiles not implemented" else
        rhombi=rhombi | apply(equivRhombi#d, x -> append(x,"fill"=>"gray"));
        );
    if o#Generic then (
        if #allTriangles === 0 then allTriangles = myload "generic.m2";
        if not allTriangles#?d then error "generic tiles not implemented" else
        upTriangles = downTriangles = allTriangles#d;
        if o#Equivariant then (
            if #allRhombi === 0 then allRhombi = myload "generic-equiv.m2";
            if not allRhombi#?d then error "equivariant generic tiles not implemented" else
            rhombi = allRhombi#d;
            );
        );
)

new List from Puzzle := (T,p) -> apply(p.Size,i->apply(p.Size-i,j->apply(3,k->p#(i,j,k))));

net Puzzle := p -> netList(applyTable(new List from p, a -> netList({a#0,{a#1,a#2}},HorizontalSpace=>1,Boxes=>false)),HorizontalSpace=>2,VerticalSpace=>1,Boxes=>false)

vgTextOpts := s -> { "dominant-baseline" => "middle",  "text-anchor" => "middle", FontSize => 1.7/(4.+#s), "stroke" => "none", "fill" => "black", "font-family" => "helvetica" };
vgOpts := k -> { SizeX => k*5, TransformMatrix => matrix{{-.5,.5,0,0},{-.5*sqrt 3,-.5*sqrt 3,0,0},{0,0,1,0},{0,0,0,1}}, "stroke-width" => 0.02, "fill" => "white" }

html Puzzle := p -> html vg p

cols:={"red","green","blue","yellow","magenta","cyan"};
strk:=0.07;

vg = p -> gList (
    n:=p.Size;
    flatten apply(n, i -> flatten apply(n-i, j -> (
                a := p#(i,j,0);
                b := p#(i,j,1);
                deepSplice {
                    if p#(i,j,2) != "" then (
                        c := p#(i,j,2);
                        Polygon splice {{(i+1,j),(i,j),(i,j+1)},if p#?(i,j,upStyle) then p#(i,j,upStyle)},
                        if (i+j<n-1) then Polygon splice {{(i+1,j),(i,j+1),(i+1,j+1)},if p#?(i,j,downStyle) then p#(i,j,downStyle)},
			if p#Paths then (
			    if i+j<n-1 then (
				aa := p#(i+1,j,0);
			    	bb := p#(i,j+1,1);
				);
			    apply(0..p#Steps, k -> (
				    kk:=toString k;
			    	    (
				    	if match(kk,a) and match(kk,b) and match(kk,c) then (
				    	    Line{(i,j+.5),(i+.333,j+.333),"stroke"=>cols#k,"stroke-width"=>strk},
				    	    Line{(i+.5,j),(i+.333,j+.333),"stroke"=>cols#k,"stroke-width"=>strk},
				    	    Line{(i+.5,j+.5),(i+.333,j+.333),"stroke"=>cols#k,"stroke-width"=>strk}
				    	    )
				    	else if match(kk,a) and match(kk,b) then Line{(i,j+.5),(i+.5,j),"stroke"=>cols#k,"stroke-width"=>strk}
				    	else if match(kk,a) and match(kk,c) then Line{(i,j+.5),(i+.5,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
				    	else if match(kk,c) and match(kk,b) then Line{(i+.5,j+.5),(i+.5,j),"stroke"=>cols#k,"stroke-width"=>strk},
    				    	if i+j<n-1 then
					if match(kk,aa) and match(kk,bb) and match(kk,c) then (
				    	    Line{(i+1,j+.5),(i+.666,j+.666),"stroke"=>cols#k,"stroke-width"=>strk},
				    	    Line{(i+.5,j+1),(i+.666,j+.666),"stroke"=>cols#k,"stroke-width"=>strk},
				    	    Line{(i+.5,j+.5),(i+.666,j+.666),"stroke"=>cols#k,"stroke-width"=>strk}
				    	    )
					else if match(kk,aa) and match(kk,bb) then Line{(i+1,j+.5),(i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk}
					else if match(kk,aa) and match(kk,c) then Line{(i+1,j+.5),(i+.5,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
					else if match(kk,c) and match(kk,bb) then Line{(i+.5,j+.5),(i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk}
					)
				    )
				)
			    ),
			if p#Labels then (
			    GraphicsText ({(i,j+.5),a} | vgTextOpts a),
                    	    GraphicsText ({(i+.5,j),b} | vgTextOpts b),
			    GraphicsText ({(i+.5,j+.5),c} | vgTextOpts c)
			    )
		    ) else (
			Polygon splice {{(i+1,j),(i,j),(i,j+1),(i+1,j+1)},if p#?(i,j,rhombusStyle) then p#(i,j,rhombusStyle)},
                    	if p#Paths then (
			    aa = p#(i+1,j,0);
			    bb = p#(i,j+1,1);
			    apply(0..p#Steps, k -> (
				    kk:=toString k;
				    (
					if match(kk,a) and match(kk,aa) then Line{(i,j+.5),(i+1,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
					else if match(kk,a) and match(kk,bb) then Line {(i,j+.5),(i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk},
					if match(kk,b) and match(kk,bb) then Line{(i+.5,j),(i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk}
					else if match(kk,b) and match(kk,aa) then Line {(i+.5,j),(i+1,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
					)
				    ))),
			if p#Labels then (
			    GraphicsText ({(i,j+.5),a} | vgTextOpts a),
                    	    GraphicsText ({(i+.5,j),b} | vgTextOpts b)
			    )
			)
                    }
                    ))) | vgOpts n )


digit := s -> #s==1 and first ascii s >=48 and first ascii s <= 48+9;
valid := (x,y) -> x === y or (x === "#" and digit y) or x === "*" or x === "**";

Puzzle ++ List := (opts1, opts2) -> merge(opts1,new class opts1 from opts2,last) -- cf similar method for OptionTable

initPuzzle = opts >> o -> args -> (
    if debugLevel>0 then << "initializing puzzle" << newline;
    args = apply(args, a -> if instance(a,String) then characters a else apply(a,toString));
    if length unique apply(args,length) != 1 then error "inputs should have the same length";
    n := #(args#0);
    new Puzzle from pairs o | { Size=>n, if o.Steps === false then Steps => max flatten apply(join args,ascii) - 48 } | flatten flatten apply(n, i ->
            apply(n-i, j -> {
                    (i,j,0) => if i==0 then args#1#j else "**",
                    (i,j,1) => if j==0 then args#0#(n-1-i) else "**",
                    (i,j,2) => if i+j==n-1 then if #args == 3 then args#2#j else "*" else "**" -- ** means even nothing (rhombus diagonal)
                    }
                )
            )
        )
    
puzzle = opts >> o -> args -> (
    if not instance(args,Puzzle) and (not instance(args,Sequence) or #args<2 or #args>3) then error "wrong number of arguments";
    puz0 := if instance(args,Sequence) then initPuzzle(args,o) else args ++ pairs o;
    n := puz0.Size;
    d := puz0.Steps;
    tiles puz0;
    lst := new MutableList;
    
    recurse := (i,j,o,p) -> ( -- o=0/1: up/down triangle needs filling
        if i == n then (
            lst#(#lst)=p;
            ) else if o==0 then (
            -- up triangles
            scan(upTriangles, x -> if valid(p#(i,j,0),x#0) and valid(p#(i,j,1),x#1) and valid(p#(i,j,2),x#2) then (
                    recurse append(if i+j==n-1 then (i+1,0,0) else (i,j,1),p++{(i,j,0)=>x#0,(i,j,1)=>x#1,(i,j,2)=>x#2,if #x>3 then (i,j,upStyle)=>x#3});
                    ));
            -- rhombi
            if p#(i,j,2) === "**" then (
                scan(rhombi, x -> if valid(p#(i,j,0),x#0) and valid(p#(i,j,1),x#1)
                    and valid(p#(i+1,j,0),x#2) and valid(p#(i,j+1,1),x#3) then (
                        recurse(i,j+1,0,p++{(i,j,0)=>x#0,(i,j,1)=>x#1,(i,j,2)=>"",(i+1,j,0)=>x#2,(i,j+1,1)=>x#3,if #x>4 then (i,j,rhombusStyle)=>x#4});
                )));
            ) else (
            -- down triangles
            scan(downTriangles, x -> if valid(p#(i+1,j,0),x#0) and valid(p#(i,j+1,1),x#1) and valid(p#(i,j,2),x#2) then (
                    recurse(i,j+1,0,p++{(i,j,2)=>x#2,(i+1,j,0)=>x#0,(i,j+1,1)=>x#1,if #x>3 then (i,j,downStyle)=>x#3});
            )));
        );

    if debugLevel>0 then << "computing puzzles" << newline;
    recurse(0,0,0, puz0);
    new List from lst
    )

-- computation of (d<=3) equivariant fugacities

-- input table of scalar products
scalar = myload "scalar.m2"
--ℏ:=getSymbol "ℏ";
--Rfug_0 = frac(factor(ZZ[ℏ])); -- init index table
--xbar:=getSymbol "xbar";
--Rfug1 = frac(factor(ZZ[ℏ,xbar]));
--ℏ=Rfug1_0; xbar=Rfug1_1;
ℏ:=FH1_0; xbar:=FH1_1;
-- H_T fugacity in terms of scalar products
fug = matrix { { 1,0,0 }, 
    {ℏ/(ℏ-xbar),xbar/(ℏ-xbar),0}, 
    {4*ℏ^2/(ℏ-xbar)/(4*ℏ-xbar),ℏ*xbar/(ℏ-xbar)/(4*ℏ-xbar),-xbar*(3*ℏ-xbar)/(ℏ-xbar)/(4*ℏ-xbar)}
    };
ind := x -> position(states#3,y->y===x);
fugacityH = p -> (
        n:=p.Size; x:=getSymbol "x"; ℏ:=getSymbol "ℏ";
        if not FH#?n then FH_n = frac(factor(ZZ[ℏ,x_1..x_n]));
	FF = FH_n;
        product(n-1, i -> product(n-1-i, j -> (
		    X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0);
                    if not p#Separated then (
                    	X = ind X; W = ind W; U = ind U;
			s := scalar_(U,X);
			t := scalar_(W,X); -- print(i,j,X,W,U,s,t);
			) else (
			if X == W then ( s=1; t=1; ) else if X == U then (  s=1; t=0; ) else ( s=0; t=0; ); -- A_n scalar products ~ A_1 scalar products
			);
                    (map(FF,FH1,{FF_0,FF_(n-i)-FF_(j+1)})) fug_(s,t)
                    ))))
-- K_T fugacity
q:=FK1_0; zbar:=FK1_1;
fugK = 1/(1-q^2*zbar) * matrix {{1 - q^2*zbar, 0, 0, 0, 0, 0, 0, -((-1 + q)*(1 + q)*zbar), 0}, {0, 0, 0, 1 - q^2*zbar, 0, 0, 0, 0, 
  ((-1 + q)*(1 + q)*zbar)/q}, {0, 0, 0, 0, 0, 0, -(q*(-1 + zbar)), 0, 0}, {0, -(q*(-1 + zbar)), 0, 0, 0, 0, 0, 0, 0}, 
 {0, 0, -((-1 + q)*(1 + q)*zbar), 0, 1 - q^2*zbar, 0, 0, 0, 0}, {-((-1 + q)*(1 + q)), 0, 0, 0, 0, 0, 0, 1 - q^2*zbar, 0}, 
 {0, 0, 1 - q^2*zbar, 0, -((-1 + q)*(1 + q)), 0, 0, 0, 0}, {0, 0, 0, 0, 0, -(q*(-1 + zbar)), 0, 0, 0}, 
 {0, 0, 0, (-1 + q)*q*(1 + q), 0, 0, 0, 0, 1 - q^2*zbar}};
ind1 := x -> position(states#1,y->y===x);
fugacityK = p -> (  -- for now, only d=1
        n:=p.Size; z:=getSymbol "z"; q:=getSymbol "z";
        if not FK#?n then FK_n = frac(factor(ZZ[q,z_1..z_n]));
	FF = FK_n;
        product(n-1, i -> product(n-1-i, j -> (
		    X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0); V := p#(i,j+1,1);
		    X = ind1 X; W = ind1 W; U = ind1 U; V = ind1 V;
                    (map(FF,FK1,{FF_0,FF_(n-i)/FF_(j+1)})) fugK_(U*3+V,X*3+W)
		    )))
    )
fugacity = p -> if class FF === FractionField and toString FF_0 === "q" then fugacityK p else fugacityH p -- primitive

bottom = p -> (
    L := apply(p.Size,i->p#(p.Size-1-i,i,2));
    new AryString from apply(L, x -> if #x === 1 then value x else x)
    )

--tallyFugacities = L -> applyKeys(hashTable apply(L,p->p=>fugacity p),bottom,plus)
fugacityTally = L -> sum(L,p->new VirtualTally from {bottom p=>fugacity p})

fugacityVector = L -> (
    if #L === 0 then error "can't determine puzzle size";
    I := unique permutations sort bottom(first L);
    t := fugacityTally L;
    vector apply(I,i->t_i)
    )

export { "puzzle", "bottom", "fugacity", "fugacityTally", "fugacityVector", "Puzzle" }
end

-- ex of use 

puzzle({0,2,0,1},{0,1,0,2},Equivariant=>true)

puzzle("103213","103213","323011",Kth=>true)

puzzle("0123","3210",Generic=>true,Equivariant=>true)

#puzzle("#######","0101212","#######") -- # is any single-digit, * is anything

puzzle("5 4 3 ","210   ",Separated=>true)

-- a complicated example: app C1 d=3

p=puzzle("2103","0321","2301",Generic=>true,Equivariant=>true);
f=fugacity\p;
sum f_{0..4}
sum f_{10..14}
sum f_{15..19} -- lots of cancellations
sum f
