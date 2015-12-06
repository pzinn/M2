------------------------------------------------------
-- additional NumericalAG types  
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
-- (common types are in ../NAGtypes.m2 
------------------------------------------------------

export{ "GateHomotopy", "GateParameterHomotopy", "gateHomotopy" }

debug SLPexpressions

----------------------------------
-- GateHomotopy

GateHomotopy := new Type of Homotopy    
GateParameterHomotopy := new Type of ParameterHomotopy

-- !!! DUMMY engine function
makeRawHomotopy = S -> null

gateHomotopy = method(Options=>{Parameters=>null,Software=>null})
gateHomotopy (GateMatrix, GateMatrix, InputGate) := o->(H,X,T) -> (
    para := o.Parameters=!=null;
    soft := if o.Software=!=null then o.Software else DEFAULT.Software;
    GH := new GateHomotopy;
    GH#"X" = X;
    if para then GH#"X" = o.Parameters | GH#"X";
    GH#"T" = T;    
    GH#"H" = H;
    GH#"Hx" = diff(X,H);
    GH#"Ht" = diff(T,H);
    GH.Software = soft;
    if soft === M2 then (
	)
    else if soft === M2engine then (
	varMat := X | matrix{{T}};
	if para then varMat = o.Parameters | varMat;
    	GH#"EH" = makeEvaluator(H,varMat);
    	GH#"EHx" = makeEvaluator(GH#"Hx",varMat);
    	GH#"EHt" = makeEvaluator(GH#"Ht",varMat);
	GH#"EHxt" = makeEvaluator(GH#"Hx"|GH#"Ht",varMat);
	GH#"EHxH" = makeEvaluator(GH#"Hx"|GH#"H",varMat);
	GH#"RawHomotopy" = (GH#"EHx",GH#"EHt",GH#"EHxH") / rawSLEvaluatorK // makeRawHomotopy;
	)
    else error "uknown Software option value";
    if para then (
	GPH := new GateParameterHomotopy;
	GPH.GateHomotopy = GH;
	GPH
	) 
    else GH
    ) 
    
evaluateH (GateHomotopy,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"H", 
    valueHashTable(flatten entries H#"X" | {H#"T"}, flatten entries x | {t}) 
    ) else if H.Software===M2engine then (
    K := ring x;
    r := if H#?("retH",K) then H#("retH",K) else H#("retH",K) = mutableMatrix(K, 1, numcols H#"H"*numrows H#"H");
    evaluate(H#"EH", mutableMatrix(transpose x | matrix{{t}}), r);
    matrix(matrix r, numrows H#"H", numcols H#"H")    
    )
evaluateHt (GateHomotopy,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"Ht", 
    valueHashTable(flatten entries H#"X" | {H#"T"}, flatten entries x | {t}) 
    ) else if H.Software===M2engine then (
    K := ring x;
    r := if H#?("retHt",K) then H#("retHt",K) else H#("retHt",K) = mutableMatrix(K, 1, numcols H#"Ht"*numrows H#"Ht");
    evaluate(H#"EHt", mutableMatrix(transpose x | matrix{{t}}), r);
    matrix(matrix r, numrows H#"Ht", numcols H#"Ht")
    )
evaluateHx (GateHomotopy,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"Hx", 
    valueHashTable(flatten entries H#"X" | {H#"T"}, flatten entries x | {t}) 
    ) else if H.Software===M2engine then (
    K := ring x;
    r := if H#?("retHx",K) then H#("retHx",K) else H#("retHx",K) = mutableMatrix(K, 1, numcols H#"Hx"*numrows H#"Hx");
    evaluate(H#"EHx", mutableMatrix(transpose x | matrix{{t}}), r);
    matrix(matrix r, numrows H#"Hx", numcols H#"Hx")
    )
evaluateH (GateParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateH(H.GateHomotopy,parameters||x,t)
evaluateHt (GateParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateHt(H.GateHomotopy,parameters||x,t)
evaluateHx (GateParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateHx(H.GateHomotopy,parameters||x,t)

-- !!! DUMMY engine function
specializeRawHomotopy = (H,M) -> H 

specialize (GateParameterHomotopy,MutableMatrix) := (PH, M) -> specialize(PH, mutableMatrix M)
specialize (GateParameterHomotopy,MutableMatrix) := (PH, M) -> (                                                                                                         
    SPH := new SpecializedParameterHomotopy;                                                                                                                  
    SPH.ParameterHomotopy = PH;                                                                                                                               
    SPH.Parameters = M;                                                                                                                                       
    if PH#?"RawHomotopy" then SPH#"RawHomotopy" = specializeRawHomotopy(PH#"RawHomotopy",M);
    SPH                                                                                                                                                       
    ) 

-------------------------------------------------------
-- trackHomotopy tests
TEST /// 
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T

K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G

debug SLPexpressions
debug NumericalAlgebraicGeometry
-- preSLP way
preH = toPreSLP({X,Y,T},H)
evaluatePreSLP(preH, {1,1,0})
preHx = transposePreSLP jacobianPreSLP(preH,toList(0..1));
evaluatePreSLP(preHx, {1,1,0})
s = first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2)
peek s
s = first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2enginePrecookedSLPs)
peek s
assert (norm evaluatePreSLP(preH, coordinates s|{1}) < 1e-6)
///

TEST ///-- Homotopy
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G
Rvars = valueHashTable({X,Y,T},{x,y,t})
gV = matrix{{X,Y}}
gH = transpose matrix {H}
gHx = diff(gV,gH)
gHt = diff(T,gH)
value(gH, Rvars)
value(gHt, Rvars)
value(gHx, Rvars)

debug NumericalAlgebraicGeometry
HS = gateHomotopy(gH,gV,T)
x0 = matrix{{1_CC},{1}}
s = first trackHomotopy(HS,{x0},Software=>M2)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
value(HS#"H", Rvars)
evaluateH(HS,x0,0.1_CC)
value(HS#"Ht", Rvars)
evaluateHt(HS,x0,0.1_CC)
evaluateHx(HS,x0,0.1_CC)

F = {X*X-1, Y*Y*Y-1}
G = {X*X+Y*Y-1, X*X*X+Y*Y*Y-1}
H = (1 - T) * F + T * G
gV = matrix{{X,Y}}
gH = transpose matrix {H}
HS = gateHomotopy(gH,gV,T)
s = first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2)
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-4)
///

TEST /// -- ParameterHomotopy
needsPackage "SLPexpressions"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
P = inputGate symbol P
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-P, -X*X+Y}
H = (1 - T) * F + T * G
gV = matrix{{X,Y}}
gH = transpose matrix {H}
gP = matrix{{P}}

debug NumericalAlgebraicGeometry
PHS = gateHomotopy(gH,gV,T,Parameters=>gP)
HS = specialize(PHS,matrix{{1_CC}})
x0 = matrix{{1_CC},{1}}
s = first trackHomotopy(HS,{x0},Software=>M2)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
///
