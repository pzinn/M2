-- -*- coding: utf-8 -*-
newPackage(
        "Gfx",
        Version => "0.2",
        Date => "May 18, 2018",
        Authors => {{Name => "Paul Zinn-Justin", 
                  Email => "pzinn@unimelb.edu.au", 
                  HomePage => "http://http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "A package to produce SVG graphics",
        DebuggingMode => false,
	AuxiliaryFiles => true,
	PackageImports => {"Text"},
	PackageExports => {"Text"}
        )

export{"GfxType", "GfxObject", "GfxPoly",
    "GfxList", "GfxCircle", "GfxLight", "GfxEllipse", "GfxPath", "GfxPolygon", "GfxPolyline", "GfxText", "GfxLine", "GfxHtml",
    "gfx", "gfxRange", "gfxIs3d", "gfxDistance", "gfxRotation", "gfxTranslation", "gfxLinearGradient", "gfxRadialGradient", "gfxArrow", "gfxPlot",
    "GfxContents", "GfxOneSided", "GfxRadiusX", "GfxRadiusY", "GfxSpecular", "GfxPoint1", "GfxPoint2", "GfxPoint", "GfxRange", "GfxWidth",
    "GfxPerspective", "GfxFontSize", "GfxCenter", "GfxHeight", "GfxAutoMatrix", "GfxMatrix", "GfxPoints", "GfxRadius",
    "GfxBlur", "GfxStatic", "GfxString", "GfxPathList", "GfxAxes", "GfxMargin", "GfxMesh",
    "SVG", "SVGElement"
    }

protect GfxFilter
protect GfxDistance
protect GfxIs3d
protect GfxAuto
protect GfxCurrentMatrix
protect GfxScaledRadius
protect GfxScaledRadiusX
protect GfxScaledRadiusY
protect GfxTag

coreStuff := {
     "hasAttribute", "getAttribute", "ReverseDictionary",    -- for global assignment
--     "Hypertext", "MarkUpType",
     "nonnull", "qname", "withOptions", "withQname", "htmlAttr" } -- hypertext

scan(coreStuff, s -> value s <- value Core#"private dictionary"#s) -- not the correct way, use PackageImports? (cf debug Core w or w/o debug Gfx)
-*
exportFrom_Core coreStuff
*-

-- for now data-* need entering manually
htmlData={ "data-matrix","data-dmatrix","data-pmatrix","data-center","data-r","data-rx","data-ry","data-coords","data-onesided","data-origin","data-point","data-point1","data-point2","data-fontsize"}
svgAttr= htmlAttr | htmlData | { "transform", "filter" } -- what else ?

GfxObject = new Type of HashTable -- ancestor type

new GfxObject from List := (T,l) -> hashTable append(l,symbol cache => new CacheTable); -- every Gfx object should have a cache
new GfxObject := T -> new T from {};
new GfxObject from OptionTable := (T,o) -> o ++ {symbol cache => new CacheTable};
GfxObject ++ List := (opts1, opts2) -> merge(opts1,new class opts1 from opts2,last) -- cf similar method for OptionTable

-- a bunch of options are scattered throughout the code:
-- * all dimensions are redefined as dimensionless quantities: GfxRadius, GfxFontSize, etc
-- * GfxMatrix for static transformation
-- * GfxAutoMatrix for animation transformation
-- * GfxOneSided for 3d paths, polygons
-- * GfxStatic for objects that can't be rotated 
--   i.e, they or their contents can rotate/autorotate, but the rotations of their ancestors won't affect them
--   useful for lights
-- * GfxBlur (amount of blurriness relative to the size of the object)
-- GLOBAL options (only work if in outermost object)
-- * GfxHeight / GfxWidth for picture sizes
-- * GfxRange for manual range of viewing window
-- * GfxPerspective for 3d: can be a number or a whole 4d matrix (ideally, there'd be a function to translate...)
--   the matrix should be such that after transformation, the coordinates are (x,y,z,z/p) where the viewer is at (0,0,0) and the screen at z=-p
-- * GfxMargin (leave blank around picture)
-- * GfxAxes (draw axes)

-- 3d: turns on lights, enables sorting, axes are diff

GfxType = new Type of Type -- all usable Gfx objects are ~ self-initialized

gfxParseFlag := false;
gfxParse := method(Dispatch=>Thing)
gfxParse Sequence := x -> gfxParse vector(toList x)
gfxParse VisibleList := x -> apply(x,gfxParse)
gfxParse HashTable := x -> applyValues(x,gfxParse)
gfxParse CacheTable := identity
gfxParse Option := x -> x#0 => gfxParse x#1
gfxParse Thing := identity
gfxParse Matrix := x -> (
    if rank source x =!= rank target x or rank source x < 2 or rank source x > 4 then error "wrong matrix";
    if rank source x == 2 then x++1++1 else if rank source x == 3 then x++1 else x
    )
gfxParse Vector := x -> (
    if rank class x < 2 or rank class x > 4 then error "wrong coordinates";
    if rank class x === 2 then x || vector {0,1.} else (
	 gfxParseFlag=true; if rank class x === 3 then x || vector {1.} else if rank class x === 4 then x)
     )

GfxType List := (T,opts) -> (
    opts0 := T.Options;
    -- scan the first few arguments in case we skipped the keys for standard arguments. also, parse
    gfxParseFlag = false;
    temp := gfxParse(opts0 | apply(#opts, i -> if i < #opts0 and class opts#i =!= Option then opts0#i#0 => opts#i else opts#i));
    new T from append(temp,symbol GfxIs3d => gfxParseFlag)
)

gfxPerspective = g -> (
    persp := if g.?GfxPerspective then g.GfxPerspective else 1000.; -- some arbitrary number
    if instance(persp,Matrix) then persp else matrix {{1,0,0,0},{0,-1,0,0},{0,0,-1,persp},{0,0,-1/persp,1}} -- useful to have output {x,y,z+p,1+z/p}
)

gfxRange = g -> (
    if not g.cache.?GfxRange then svg g; -- need to be rendered
    g.cache.GfxRange
    )
gfxRange1 := method() -- returns [xmin,ymin],[xmax,ymax]
gfxRange1 GfxObject := x -> null

-- GfxIs3d=false has two effects:
-- * the data-* stuff is lightened (can be recreated from the normal parameters)
-- * the event listeners for 3d rotating the object with the mouse are deactivated
-- * lighting is deactivated
gfxIs3d = x -> if x.?GfxIs3d then x.GfxIs3d else true; -- the else clause should never happen

gfxDistance = g -> (
    if not g.cache.?GfxDistance then svg g; -- need to be rendered
    g.cache.GfxDistance
    )
gfxDistance1 := method()
gfxDistance1 GfxObject := x -> 0_RR

updateGfxCache := g -> (
    g.cache.GfxRange = gfxRange1 g; -- update the range
    g.cache.GfxDistance = gfxDistance1 g; -- update the distance
    if g.?GfxOneSided and g.GfxOneSided then gfxDetermineSide g;
    -- bit of a hack: 2d objects GfxCircle, GfxEllipse get scaled in a 3d context
    if instance(g,GfxCircle) then (
	scale := 1/(g.cache.GfxCurrentMatrix*g.GfxCenter)_3;
	g.cache.GfxScaledRadius=max(0,g.GfxRadius*scale);
	) else if instance(g,GfxEllipse) then (
	scale = 1/(g.cache.GfxCurrentMatrix*g.GfxCenter)_3;
	g.cache.GfxScaledRadiusX=max(0,g.GfxRadiusX*scale);
	g.cache.GfxScaledRadiusY=max(0,g.GfxRadiusY*scale);
	) else if instance(g,GfxText) then ( -- same for GfxText
	-- choose font size
	f := if g.?GfxFontSize then g.GfxFontSize else 14.;
	scale = 1/(g.cache.GfxCurrentMatrix*g.GfxPoint)_3;
	f = max(0,f*scale);
	g.cache#"font-size"= toString f|"px";
	if instance(g,GfxHtml) then ( -- hack
	    g.cache#"overflow"="visible"; -- makes width/height irrelevant
	    g.cache#"width"=g.cache#"height"="100%"; -- but still needed otherwise webkit won't render
	    );
	);
    )

project2d := x -> vector {x_0/x_3,x_1/x_3}

new GfxType of GfxObject from VisibleList := (T,T2,x) -> (
    g:=new MutableHashTable;
    g.Options=x#1; -- TODO: should it be an actual table? then have to suppress the BS syntax
    g.SVGElement = withQname_(x#0) withOptions_(svgAttr | if #x>=3 then x#2 else {}) new MarkUpType of Hypertext;
    g)

    

GfxCircle = new GfxType of GfxObject from ( "circle",
    { symbol GfxCenter => vector {0.,0.}, symbol GfxRadius => 50. },
    { "r", "cx", "cy" }
    )
gfxRange1 GfxCircle := g -> (
    p := g.cache.GfxCurrentMatrix * g.GfxCenter;
    r:=g.GfxRadius/p_3;
    p=project2d p;
    r = vector {r,r};
    { p - r, p + r }
    )
gfxDistance1 GfxCircle := g -> (
    y := g.cache.GfxCurrentMatrix * g.GfxCenter;
    y_2
    )

GfxEllipse = new GfxType of GfxObject from ( "ellipse",
    { symbol GfxCenter => vector {0.,0.}, symbol GfxRadiusX => 50., symbol GfxRadiusY => 50. },
    { "rx", "ry", "cx", "cy" }
    )
gfxRange1 GfxEllipse := g -> (
    p := g.cache.GfxCurrentMatrix * g.GfxCenter;
    rx:=g.GfxRadiusX/p_3; ry:=g.GfxRadiusY/p_3;
    p=project2d p;
    r := vector {rx,ry};
    { p - r, p + r }
    )
gfxDistance1 GfxEllipse := g -> (
    y := g.cache.GfxCurrentMatrix * g.GfxCenter;
    y_2
    )

GfxText = new GfxType of GfxObject from ( "text",
    { GfxPoint => vector {0.,0.}, GfxString => "" },
    { "x", "y" }
    )
gfxRange1 GfxText := g -> (
    f := if g.?GfxFontSize then g.GfxFontSize else 14.;
    p := g.cache.GfxCurrentMatrix * g.GfxPoint;
    f=f/p_3;
    p=project2d p;
    { p - vector {0,f}, p + vector{f*0.6*length g.GfxString,0} } -- very approximate TODO properly
    )

GfxLine = new GfxType of GfxObject from ( "line",
    { GfxPoint1 => vector {0.,0.}, GfxPoint2 => vector {50.,50.}},
    { "x1", "y1", "x2", "y2" }
    )
gfxRange1 GfxLine := g -> (
    p1 := project2d(g.cache.GfxCurrentMatrix * g.GfxPoint1);
    p2 := project2d(g.cache.GfxCurrentMatrix * g.GfxPoint2);
    p := transpose{entries p1,entries p2};
    { vector(min\p), vector(max\p) }
    )
gfxDistance1 GfxLine := g -> (
    p1 := g.cache.GfxCurrentMatrix * g.GfxPoint1;
    p2 := g.cache.GfxCurrentMatrix * g.GfxPoint1;
    0.5*(p1_2+p2_2)
    )

GfxPoly = new Type of GfxObject;

GfxPolyline = new GfxType of GfxPoly from ( "polyline", { symbol GfxPoints => {} }, { "points" } )
GfxPolygon = new GfxType of GfxPoly from ( "polygon", { symbol GfxPoints => {} }, { "points" } )
GfxPath = new GfxType of GfxPoly from ( "path", { symbol GfxPathList => {} }, { "d" } )
gfxRange1 GfxPoly := g -> ( -- relative coordinates *not* supported, screw this
    if instance(g,GfxPath) then s := select(g.GfxPathList, x -> instance(x,Vector)) else s = g.GfxPoints;
    s = transpose apply(s, x -> entries project2d (g.cache.GfxCurrentMatrix*x));
    {vector(min\s), vector(max\s)}
    )

-- to make lists of them
GfxList = new GfxType of GfxObject from ( "g", { symbol GfxContents => {} } )
-- slightly simpler syntax: gfx (a,b,c, opt=>xxx) rather than GfxList { {a,b,c}, opt=>xxx }
gfx = x -> (
    x=flatten toList sequence x;
    x1 := select(x, y -> instance(y,GfxObject));
    x2 := select(x, y -> instance(y,Option));
    GfxList append(x2,symbol GfxContents => x1)
    )
gfxRange1 GfxList := x -> (
    s := nonnull apply(x.GfxContents, y->y.cache.GfxRange);
    if #s===0 then null else (
	s = transpose s;
    	mn := transpose (entries \ s#0);
    	mx := transpose (entries \ s#1);
	{vector (min\mn), vector(max\mx)}
    )
)

-- the scaling is incorrect. no known solution. disabling.
-- the positioning is problematic: the only way to position correctly (as well as set width, height)
-- is to shift by half its size, which can only be obtained by getBoundingClientRect(), which one can only do after rendering
GfxHtml = new GfxType of GfxText from ( "foreignObject",
    { GfxPoint => vector {0.,0.}, GfxString => "" },
    { "x", "y" }
    )
gfxRange1 GfxHtml := g -> (
    p := project2d (g.cache.GfxCurrentMatrix * g.GfxPoint);
    { p, p } -- TODO properly
    )

--
gfxAuto := method()
gfxAuto GfxObject := x -> x.?GfxAutoMatrix
gfxAuto GfxList := x -> (
    if not x.cache.?GfxAuto then x.cache.GfxAuto = x.?GfxAutoMatrix or any(x.GfxContents,gfxAuto);
    x.cache.GfxAuto
    )

SVG = withOptions_{
    svgAttr,
    "height","preserveAspectRatio","viewBox","width","x","xmlns"=>"http://www.w3.org/2000/svg","y","zoomAndPan"
    } new MarkUpType of Hypertext

--
stableSort = x -> if #x <= 1 then x else (
xx := transpose {x,toList(0..#x-1)};
(transpose sort xx)#0
)

-- for javascript stuff
jsString := method(Dispatch=>Thing)
jsString Thing := toString
jsString String := x -> "'" | x |"'"
jsString Matrix := x -> "matrix(" | jsString entries x | ")"
jsString Vector := x -> "vector(" | jsString entries x | ")"
jsString VisibleList := x -> "[" | demark(",",jsString\x) | "]"
--jsString HashTable := x -> "{" | demark(",",apply(pairs x, (key,val) -> jsString key | ":" | jsString val)) | "}"
jsString Option := x -> "times(" | jsString x#0 | "," | jsString x#1 | ")"

updateGfxMatrix := (g,m,p) -> ( -- (object,matrix,persepective matrix)
    g.cache.GfxCurrentMatrix = if g.?GfxStatic and g.GfxStatic then p else m; -- if static reset to perspective matrix
    if g.?GfxMatrix then g.cache.GfxCurrentMatrix = g.cache.GfxCurrentMatrix*g.GfxMatrix;
    )

-*
LiteralString := new WrapperType of Holder -- to make sure the text inside GfxText doesn't get html'ified
htmlWithTex LiteralString := x -> htmlLiteral x#0
*-

svgLookup := hashTable { -- should be more systematic
    symbol GfxMatrix => (x,m) -> "data-matrix" => jsString x,
    symbol GfxAutoMatrix => (x,m) -> "data-dmatrix" => jsString x,
    symbol GfxCenter => (x,m) -> (
	x = project2d (m*x);
	"cx" => toString x_0,
	"cy" => toString x_1
	),
    symbol GfxScaledRadius => (x,m) ->  "r" => toString x,
    symbol GfxScaledRadiusX => (x,m) ->  "rx" => toString x,
    symbol GfxScaledRadiusY => (x,m) ->  "ry" => toString x,
    symbol GfxPathList => (x,m) -> "d" => demark(" ", flatten apply(x, y -> if instance(y,Vector) then apply(entries project2d(m*y),toString) else y)),
    symbol GfxPoints => (x,m) -> "points" => demark(" ", flatten apply(x, y -> apply(entries project2d(m*y),toString))),
    symbol GfxPoint => (x,m) -> (
	x = project2d (m*x);
	"x" => toString x_0,
	"y" => toString x_1
	),
    symbol GfxPoint1 => (x,m) -> (
	x = project2d (m*x);
	"x1" => toString x_0,
	"y1" => toString x_1
	),
    symbol GfxPoint2 => (x,m) -> (
	x = project2d (m*x);
	"x2" => toString x_0,
	"y2" => toString x_1
	),
    symbol GfxStatic => (x,m) -> if x then "data-pmatrix" => jsString m,
    symbol GfxTag => (x,m) -> "id" => x,
    symbol GfxFilter => (x,m) -> "filter" => toString x,
    symbol GfxContents => (x,m) -> (
	x = toSequence stableSort x;
	apply(x, y -> y.cache.SVGElement)
	),
    symbol GfxString => (x,m) -> x
    }

svg3dLookup := hashTable { -- should be more systematic
    symbol GfxCenter => x -> "data-center" => jsString x,
    symbol GfxRadius => x -> "data-r" => jsString x,
    symbol GfxRadiusX => x -> "data-rx" => jsString x,
    symbol GfxRadiusY => x -> "data-ry" => jsString x,
    symbol GfxPathList => x -> "data-coords" => jsString x,
    symbol GfxPoints => x -> "data-coords" => jsString x,
    symbol GfxPoint => x -> "data-point" => jsString x,
    symbol GfxPoint1 => x -> "data-point1" => jsString x,
    symbol GfxPoint2 => x -> "data-point2" => jsString x,
    symbol GfxOneSided => x -> "data-onesided" => jsString x,
    symbol GfxFontSize => x -> "data-fontsize" => jsString x,
    }

-- produces SVG element hypertext
svg = method()
svg (GfxObject,Matrix,Matrix,List) := (g,m,p,l) -> ( -- (object,current matrix,perspective matrix,lights)
    if not (class g).?SVGElement then return;
    updateGfxMatrix(g,m,p);
    if g.?GfxContents then scan(g.GfxContents, x -> svg(x,g.cache.GfxCurrentMatrix,p,l));
    updateGfxCache g;
    gfxFilter(g,l);
    prs := pairs g | pairs g.cache; -- TODO restructure
    opts := deepSplice apply(select(prs,(key,val)-> svgLookup#?key), (key,val) -> svgLookup#key(val,g.cache.GfxCurrentMatrix));
    if gfxIs3d g then opts = opts | deepSplice apply(select(prs,(key,val)-> svg3dLookup#?key), (key,val) -> svg3dLookup#key val);
    g.cache.SVGElement = style((class g).SVGElement opts,prs)
    )

svg (GfxObject,Matrix,Matrix) := (g,m,p) -> svg(g,m,p,{})

svg (GfxObject,List) := (g,l) -> (
    p := gfxPerspective g;
    svg(g,p,p,l)
)

svg GfxObject := g -> svg(g,{})

htmlWithTex GfxObject := html

globalAssignment GfxObject
toString GfxObject := g -> if hasAttribute(g,ReverseDictionary) then toString getAttribute(g,ReverseDictionary) else (lookup(toString,HashTable)) g
net GfxObject := g -> if hasAttribute(g,ReverseDictionary) then net getAttribute(g,ReverseDictionary) else (lookup(net,HashTable)) g
expression GfxObject := hold -- bit of a hack: don't want the reverse dictionary to interfere with expression

gfxDistance1 GfxPoly := g -> (
    if instance(g,GfxPath) then s := select(g.GfxPathList, x -> instance(x,Vector)) else s = g.GfxPoints;
    sum(s,x->(g.cache.GfxCurrentMatrix*x)_2) / #s
    )
gfxDistance1 GfxList := g -> (
    if #(g.GfxContents) == 0 then 0_RR else sum(g.GfxContents, gfxDistance) / #(g.GfxContents)
    )
GfxObject ? GfxObject := (x,y) -> (gfxDistance y) ? (gfxDistance x)
gfxDistance1 GfxText := g -> (
    y := g.cache.GfxCurrentMatrix*g.GfxPoint;
    y_2
    )

gfxTagCount := 0;
gfxTag := () -> (
    gfxTagCount=gfxTagCount+1;
    "Gfx_" | toString currentTime() | "_" | toString gfxTagCount
    )

-- defs
svgDefs = withQname_"defs" withOptions_svgAttr new MarkUpType of Hypertext
scanDefs := g -> (
    lst := select(values g | values g.cache, y->instance(y,HypertextInternalLink));
    if g.?GfxContents then lst = lst | flatten apply(g.GfxContents,scanDefs);
    lst
    )


-- full SVG with the headers
new SVG from GfxObject := (S,g) -> (
    p := gfxPerspective g;
    lights := if gfxIs3d g then gfxSetupLights(g,p,p) else {};
    main := svg(g,p,p,lights); -- run this first because it will compute the ranges too
    if main === null then return {};
    if g.?GfxRange then r := g.GfxRange else r = g.cache.GfxRange; -- should be cached at this stage
    if r === null or r#0 == r#1 then (g.cache.GfxWidth=g.cache.GfxHeight=0.; return {}); -- nothing to draw
    r = apply(r,numeric);
    rr := r#1 - r#0;
    if rr_0 == 0 then (
	rr = vector { rr_1 * 16/10, rr_1 };
	r = { vector { r#0_0 - 0.5*rr_0, r#0_1 }, vector { r#1_0 + 0.5*rr_0, r#1_1 } };
	);
    if rr_1 == 0 then (
	rr = vector { rr_0, rr_0 * 10/16 };
	r = { vector { r#0_0, r#0_1 - 0.5*rr_1 }, vector {  r#1_0, r#1_1 + 0.5*rr_1 } };
	);
    -- axes
    axes:=null; axeslabels:=null; defsList:={};
    if g.?GfxAxes and g.GfxAxes =!= false then ( -- TEMP: coordinates wrong
	arr := gfxArrow();
	axes = gfx(
	    GfxLine { GfxPoint1 => vector if gfxIs3d g then {r#0_0,0,0} else {r#0_0,0}, GfxPoint2 => vector if gfxIs3d g then {r#1_0,0,0} else {r#1_0,0}, "marker-end" => arr },
	    GfxLine { GfxPoint1 => vector if gfxIs3d g then {0,r#0_1,0} else {0,r#0_1}, GfxPoint2 => vector if gfxIs3d g then {0,r#1_1,0} else {0,r#1_1}, "marker-end" => arr },
	    if gfxIs3d g then GfxLine { GfxPoint1 => vector{0,0,min(r#0_0,r#0_1)}, GfxPoint2 => vector {0,0,max(r#1_0,r#1_1)}, "marker-end" => arr },
	    "stroke"=>"black", "stroke-width"=>0.01*min(rr_0,rr_1)
	    );
	axeslabels = gfx(
	    GfxHtml { GfxPoint => 1.06*vector if gfxIs3d g then {r#1_0,0,0} else {r#1_0,0}, GfxString => htmlWithTex if instance(g.GfxAxes,List) and #g.GfxAxes>0 then g.GfxAxes#0 else local x , GfxFontSize => 0.08*min(rr_0,rr_1)},
	    GfxHtml { GfxPoint => 1.06*vector if gfxIs3d g then {0,r#1_1,0} else {0,r#1_1}, GfxString => htmlWithTex if instance(g.GfxAxes,List) and #g.GfxAxes>1 then g.GfxAxes#1 else local y, GfxFontSize => 0.08*min(rr_0,rr_1)},
	    if gfxIs3d g then GfxHtml { GfxPoint => 1.06*vector{0,0,max(r#1_0,r#1_1)}, GfxString => htmlWithTex if instance(g.GfxAxes,List) and #g.GfxAxes>2 then g.GfxAxes#2 else local z, GfxFontSize => 0.08*min(rr_0,rr_1)}
	    -*
	    GfxText { GfxPoint => 1.06*vector if gfxIs3d g then {r#1_0,0,0} else {r#1_0,0}, GfxString => if instance(g.GfxAxes,List) then toString g.GfxAxes#0 else "x", GfxFontSize => 0.08*min(rr_0,rr_1)},
	    GfxText { GfxPoint => 1.06*vector if gfxIs3d g then {0,r#1_1,0} else {0,r#1_1}, GfxString => if instance(g.GfxAxes,List) then toString g.GfxAxes#1 else "y", GfxFontSize => 0.08*min(rr_0,rr_1)},
	    if gfxIs3d g then GfxText { GfxPoint => 1.06*vector{0,0,max(r#1_0,r#1_1)}, GfxString => if instance(g.GfxAxes,List) then toString g.GfxAxes#2 else "z", GfxFontSize => 0.08*min(rr_0,rr_1)},
	    "stroke" => "none", "fill"=>"black"
	    *-
	    );
	defsList = scanDefs axes | scanDefs axeslabels;
	axes=svg(axes,p,p);
	axeslabels=svg(axeslabels,p,p);
	);
    if g.?GfxWidth then g.cache.GfxWidth = numeric g.GfxWidth;
    if g.?GfxHeight then g.cache.GfxHeight = numeric g.GfxHeight;
    if not (g.?GfxWidth or g.?GfxHeight) then -- by default, make it fit inside 16 x 10
	if rr_0 > 1.6*rr_1 then g.cache.GfxWidth = 16. else g.cache.GfxHeight = 10.;
    -- at this stage one of the two is set
    if not g.cache.?GfxHeight then g.cache.GfxHeight = g.cache.GfxWidth * rr_1/rr_0;
    if not g.cache.?GfxWidth then g.cache.GfxWidth = g.cache.GfxHeight * rr_0/rr_1;
    -- put some extra blank space around picture
    margin := if g.?GfxMargin then g.GfxMargin else 0.1;
    r = { r#0-margin*rr, r#1+margin*rr }; rr = (1+2*margin)*rr;
    --
--    tag := gfxTag();
    ss := SVG {
	"preserveAspectRatio" => "none",
	"class" => "M2Svg",
--	"id" => tag,
	"style" => concatenate("width:",toString g.cache.GfxWidth,"em;",
	    "height:",toString g.cache.GfxHeight,"em;",
	    if not g#?"stroke-width" then "stroke-width:"|toString(0.005*max(rr_0,rr_1)), -- define a default stroke-width
	),
	"viewBox" => concatenate between(" ",toString \ {r#0_0,r#0_1,r#1_0-r#0_0,r#1_1-r#0_1}),
	"data-pmatrix" => jsString p
	};
    if gfxIs3d g then ss = append(ss, "onmousedown" => "gfxMouseDown.call(this,event)");
    if axes =!= null then ss = append(ss, axes);
    if axeslabels =!= null then ss = append(ss, axeslabels);
    ss = append(ss,main);
    defsList = unique ( defsList | scanDefs g );
    if #defsList>0 then ss=append(ss,svgDefs defsList);
    -- then autorotate button
    if gfxAuto g then (
	sizex := rr_0*min(0.5,1.5/g.cache.GfxWidth); sizey := rr_1*min(0.5,1.5/g.cache.GfxHeight); -- can't be larger than half the pic; default = 1.5em
	ss = append(ss,
	GfxList.SVGElement {
	    "transform" => "translate("|toString(r#0_0)|" "|toString(r#0_1)|") scale("|toString sizex|" "|toString sizey|")",
	    "class" => "gfxauto",
	    "onclick" => "gfxToggleRotation.call(this,event)",
	    GfxCircle.SVGElement { "cx" => "0.5", "cy" => "0.5", "r" => "0.45", "style" => "fill:white; stroke:black; stroke-width:0.05" },
	    GfxPolygon.SVGElement { "class" => "gfxautoplay", "points" => "0.3,0.25 0.8,0.5 0.3,0.75", "style" => "stroke:none; fill:black" },
	    GfxLine.SVGElement { "class" => "gfxautostop", "x1" => "0.3", "y1" => "0.25", "x2" => "0.3", "y2" => "0.75", "style" => "stroke:black; stroke-width:0.15" },
	    GfxLine.SVGElement { "class" => "gfxautostop", "x1" => "0.7", "y1" => "0.25", "x2" => "0.7", "y2" => "0.75", "style" => "stroke:black; stroke-width:0.15" }
	    }
	));
    ss
    )

html GfxObject := g -> html SVG g;

-- now transformations
-- following 2 functions can be used to produce matrices to be fed to either 
-- GfxAutoMatrix (animation) or GfxMatrix (static)

gfxRotation = args -> (
    args = sequence args;
    if #args>3 then error("Too many arguments");
    angle := args#0;
    threeD :=  #args === 3 or (#args === 2 and (( instance(args#1,Vector) and rank class args#1 === 3 ) or ( instance(args#1,Sequence) and #args#1 === 3 )));
    axis := promote(if threeD then if instance(args#1,Vector) then args#1 else vector toList args#1 else vector {0,0,1},RR);
    invr := 1/sqrt(axis_0^2+axis_1^2+axis_2^2);
    axis = invr*axis;
    cross := (axis#0)**transpose(axis#0);
    rot := cross + (sin angle) * matrix {{0,-axis_2,axis_1},{axis_2,0,-axis_0},{-axis_1,axis_0,0}} + (cos angle) * (1-cross);
    rot = rot ++ 1;
    if (#args==2 and threeD) or #args==1 then rot else (
	center := gfxParse last args;
	(gfxTranslation(center))*rot*(gfxTranslation(-center))
    	)
    )
gfxTranslation = vec -> (
    vec = gfxParse vec;
    matrix {{1,0,0,vec_0},{0,1,0,vec_1},{0,0,1,vec_2},{0,0,0,1}}
)

gfxDetermineSide = method()
gfxDetermineSide GfxObject := x -> ()
gfxDetermineSide GfxPoly := g -> (
    -- find first 3 coords
    if instance(g,GfxPath) then coords := select(g.GfxPathList, x -> instance(x,Vector)) else coords = g.GfxPoints;
    if #coords<3 then ( remove(g.cache,GfxFilter); return; );
    coords=apply(take(coords,3),x->(g.cache.GfxCurrentMatrix*x)^{0,1,2});
    g.cache#"visibility" = if det(matrix coords#0 | matrix coords#1 | matrix coords#2) > 0 then "hidden" else "visible";
    )

-- lighting
GfxLight = new GfxType of GfxCircle from ( "circle",
    { symbol GfxCenter => vector {0,0,0,1.}, symbol GfxRadius => 0, symbol GfxSpecular => 64, symbol GfxBlur => 0.3, symbol GfxStatic => true, "fill" => "#FFFFFF", "stroke" => "none" },
    { "r", "cx", "cy" } -- atm these are not inherited
    )
-- in case it's drawn, it's a circle

-- gfxRange1 ignores lights if invisible
gfxRange1 GfxLight := g -> if g.GfxRadius === 0 then null else (lookup(gfxRange1,GfxCircle)) g

gfxSetupLights = (g,m,p) -> if instance(g,GfxLight) then (
    updateGfxMatrix(g,m,p);
    g.cache.GfxTag = gfxTag();
    { g } ) else if g.?GfxContents then (
    updateGfxMatrix(g,m,p);
    flatten apply(g.GfxContents, x -> gfxSetupLights(x,g.cache.GfxCurrentMatrix,p))
    ) else {}; -- yeah, could make a method...

HypertextInternalLink = new Type of Hypertext -- could be useful elsewhere
toString HypertextInternalLink := net HypertextInternalLink := x -> (
    -- ideally we'd use "override" to get the tag, but...
    tag := (select(x, y -> instance(y,Option) and y#0==="id"))#0#1;
    "url(#"|tag|")"
)

noid := x -> select(x,e -> class e =!= Option or e#0 =!= "id")
htmlWithTex HypertextInternalLink := html @@ noid -- bit of a hack: to prevent id from being printed directly in WebApp mode

svgFilter := withQname_"filter" withOptions_{svgAttr,"x","y","width","height"} new MarkUpType of HypertextInternalLink;
feGaussianBlur := withQname_"feGaussianBlur" withOptions_{svgAttr,"in","result","stdDeviation"} new MarkUpType of Hypertext;
feSpecularLighting := withQname_"feSpecularLighting" withOptions_{svgAttr,"result","specularExponent","lighting-color"} new MarkUpType of Hypertext;
fePointLight := withQname_"fePointLight" withOptions_{svgAttr,"x","y","z"} new MarkUpType of Hypertext;
feComposite := withQname_"feComposite" withOptions_{svgAttr,"in","in2","operator","result","k1","k2","k3","k4"} new MarkUpType of Hypertext;

gfxFilter = (g,l) -> if (g.?GfxBlur and g.GfxBlur != 0) or (#l > 0 and instance(g,GfxPoly)) then (
    tag := gfxTag();
    i:=0;
    opts := { "id" => tag };
    if g.?GfxBlur then (
	b := g.GfxBlur;
	opts = opts | { "x" => toString(-100*b)|"%", "y" => toString(-100*b)|"%", "width" => toString(100*(1+2*b))|"%", "height" => toString(100*(1+2*b))|"%" };
	rng := g.cache.GfxRange; if rng =!= null then (
    	    drng:=rng#1-rng#0;
    	    r := b*min(drng_0,drng_1);
	    g.cache.GfxRange={rng#0-vector{r,r},rng#1+vector{r,r}}; -- a bit of a hack
	    opts = append(opts, feGaussianBlur { "in" => "SourceGraphic", 
		    "result" => "result"|toString i, "stdDeviation" => toString(0.5*r) } ); -- problem is, this should be updated dynamically as radius changes...
	    i=i+1;
	)
    );
    if gfxIs3d g and instance(g,GfxPoly) then (
    	-- find first 3 coords
	if instance(g,GfxPath) then coords := select(g.GfxPathList, x -> instance(x,Vector)) else coords = g.GfxPoints;
    	if #coords>=3 then (
	    coords=apply(take(coords,3),x->(g.cache.GfxCurrentMatrix*x)^{0,1,2});
    	    d:=-det(matrix coords#0 | matrix coords#1 | matrix coords#2);
    	    u:=coords#1-coords#0; v:=coords#2-coords#0; w:=vector{u_1*v_2-v_1*u_2,u_2*v_0-v_2*u_0,u_0*v_1-v_0*u_1}; w2:=w_0*w_0+w_1*w_1+w_2*w_2;
	    scan(l, gg -> (
	    	    -- compute reflected coords
		    light := gg.cache.GfxCurrentMatrix*gg.GfxCenter;
	    	    p := light_2/light_3;
	    	    light=light^{0,1,2};
	    	    lightrel := light-coords#0;
	    	    sp := w_0*lightrel_0+w_1*lightrel_1+w_2*lightrel_2;
	    	    c := 2*sp/w2;
	    	    lightmir := light - c*w;
		    if d<0 then sp=-sp;
		    opts = opts | {
			feSpecularLighting { "result" => "spec"|toString i, "specularExponent" => toString gg.GfxSpecular, "lighting-color" => if sp<0 then "black" else toString g#"fill",
			    fePointLight { "data-origin" => gg.cache.GfxTag, "x" => toString(lightmir_0*p/lightmir_2), "y" => toString(lightmir_1*p/lightmir_2), "z" => toString(sp/sqrt(w2)) } },
			feComposite { "in" => "spec"|toString i, "in2" => "SourceGraphic", "operator" => "in", "result" => "clipspec"|toString i },
			feComposite { "in" => (if i==0 then "SourceGraphic" else "result"|toString(i-1)),  "in2" => "clipspec"|toString i, "result" => "result"|toString i,
			    "operator" => "arithmetic", "k1" => "0", "k2" => "1", "k3" => "1", "k4" => "0" }
			};
	    	    i=i+1;
	    	    ));
	    );
	);
    g.cache.GfxFilter=svgFilter opts;
    ) else remove(g.cache,GfxFilter);

svgLinearGradient := withQname_"linearGradient" withOptions_{svgAttr,"in","in2","operator","result","k1","k2","k3","k4"} new MarkUpType of HypertextInternalLink;
svgRadialGradient := withQname_"radialGradient" withOptions_{svgAttr,"in","in2","operator","result","k1","k2","k3","k4"} new MarkUpType of HypertextInternalLink;
svgStop := withQname_"stop" withOptions_{svgAttr,"offset"} new MarkUpType of HypertextInternalLink;
gfxLinearGradient = true >> o -> stop -> (
    tag := gfxTag();
    svgLinearGradient prepend(
	"id" => tag,
	apply(pairs o, (key,val) -> key => val) -- lame but what to do
	|
	apply(stop,(offset,style) -> svgStop { "offset" => offset, "style" => style })
	)
    )
gfxRadialGradient = true >> o -> stop -> (
    tag := gfxTag();
    svgRadialGradient prepend(
	"id" => tag,
	apply(pairs o, (key,val) -> key => val) -- lame but what to do
	|
	apply(stop,(offset,style) -> svgStop { "offset" => offset, "style" => style })
	)
    )

GfxArrow = new OptionTable from gfxParse { symbol GfxPoints => { vector {0,0}, vector {0,4}, vector {3,2} }, "fill" => "black", "stroke" => "none", GfxIs3d => false }
svgMarker := withQname_"marker" withOptions_{svgAttr, "orient" => "auto", "markerWidth" => "3", "markerHeight" => "4", "refX" => "0", "refY" => "2"} new MarkUpType of HypertextInternalLink;
gfxArrow = true >> o -> x -> (
    tag := gfxTag();
    svgMarker {
	"id" => tag,
	svg(new GfxPolygon from (GfxArrow ++ gfxParse o),map(RR^4,RR^4,1),map(RR^4,RR^4,1))  -- eww
	}
    )

-* TODO recreate at some point
gfxLabel = true >> o -> label -> (
    tag := gfxTag();
    f:=1; -- TEMP
--    s:="<marker id='"|tag|"' markerUnits='userSpaceOnUse' markerWidth='"|toString(f*0.6*length label)|"' markerHeight='"|toString f|"' refX='0' refY='0'>"; -- very approximate
    s:="<marker id='"|tag|"' markerWidth='100' markerHeight='100' refX='0' refY='0'>"; -- very approximate
    saveGfxMatrix := currentGfxMatrix;
    s=s|(svg new GfxText from (new GfxObject) ++ { "fill" => "black", "stroke" => "none" } ++ gfxParse o ++ { GfxPoint => vector {0,0}, GfxString => label });
    currentGfxMatrix = saveGfxMatrix;
    s=s|"</marker>";
    new GfxTagged from (tag,s)
    )
*-    

needsPackage "NumericalAlgebraicGeometry"; -- probably overkill

-- note that the range is only where the curve actually lies, not the original range "r" provided.
-- the reason is that it's not clear how to force that original range (there are possible coordinate transformations etc)
gfxPlot = true >> o -> (P,r) -> (
    R := ring P; -- R should have one or two variables
    if not instance(r,List) then error("incorrect ranges");
    if not instance(r#0,List) then r = { r };
    if #r>2 or (numgens R =!= #r and numgens R =!= #r+1) then error("incorrect number of variables / ranges");
    if numgens R === #r then R2 := coefficientRing R else R2 = (coefficientRing R) ( monoid [last gens R] );
    if (#r === 1) then ( r = r#0;
	if (o.?GfxMesh) then n := o.GfxMesh else n = 100;
	val := transpose apply(n+1, i -> (
		x := i*(r#1-r#0)/n+r#0;
		f := map(R2,R, matrix { if numgens R === 1 then { x } else { x, R2_0 } });
		y := if numgens R === 1 then { f P } else sort apply(solveSystem { f P }, p -> first p.Coordinates); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(y, yy -> if abs imaginaryPart yy < 1e-6 then vector { x, realPart yy })));
	new GfxList from (
	    (new OptionTable from { "fill"=>"none", GfxAxes=>gens R, GfxIs3d=>false,
		symbol GfxContents => apply(val, v -> GfxPath { flag:=true; GfxPathList => flatten apply(v, w -> if w === null then (flag=true; {}) else first({ if flag then "M" else "L", w },flag=false))})
		}) ++ gfxParse o
	    )
	) else (
	if (o.?GfxMesh) then n = o.GfxMesh else n = 10;
	val = table(n+1,n+1,(i,j)->(
		x := i*(r#0#1-r#0#0)/n+r#0#0;
		y := j*(r#1#1-r#1#0)/n+r#1#0;
		f := map(R2,R, matrix { if numgens R === 2 then { x,y } else { x, y, R2_0 } });
		z := if numgens R === 2 then { f P } else sort apply(solveSystem { f P }, p -> first p.Coordinates); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(z, zz -> if abs imaginaryPart zz < 1e-6 then vector { x, y, realPart zz })));
	new GfxList from (
	    (new OptionTable from { GfxAxes=>gens R, GfxIs3d=>true,
		symbol GfxContents => flatten flatten table(n,n,(i,j) -> for k from 0 to min(#val#i#j,#val#(i+1)#j,#val#i#(j+1),#val#(i+1)#(j+1))-1 list (
			if val#i#j#k === null or val#(i+1)#j#k === null or val#i#(j+1)#k === null or val#(i+1)#(j+1)#k === null then continue;
			GfxPolygon { GfxPoints => { val#i#j#k, val#(i+1)#j#k, val#(i+1)#(j+1)#k, val#i#(j+1)#k } } ) ) -- technically this is wrong -- the quad isn't flat, we should make triangles
		 }) ++ gfxParse o
	)
    )
)

beginDocumentation()
multidoc ///
 Node
  Key
   Gfx
  Headline
   A package to produce SVG graphics
  Description
   Text
    {\bf Gfx} is a package to produce SVG 2d and 3d graphics.
    All usable types are descendents of the type GfxObject, and are self-initializing.
    Coordinates can be entered as vectors in \mathbb{RR}^2, \mathbb{RR}^3 or \mathbb{RR}^4 (\mathbb{RR}^4 is projective
    coordinates); alternatively, one can enter them as sequences. With the default perspective matrix,
    the x axis points to the right, the y axis points up, and the z axis points towards the viewer.
    All types are option tables, i.e., their arguments are options. There are two types of options:
    Gfx options, that are symbols starting with Gfx (e.g., {\tt GfxRadius} for circles);
    and styling options, which are CSS style options,
    and which are {\bf strings} (e.g., {\tt "fill"} for fill color).
    Gfx does not use units (coordinates are dimensionless).
    In @ TO {Standard} @ mode, the graphical objects are not directly visible; to export them to SVG
    in order to embed them into a web page, use @ TO {html} @. In @ TO {WebApp} @ mode, the graphical objects
    are shown as output.
 Node
  Key
   GfxObject
  Headline
   The ancestor class of all Gfx objects
 Node
  Key
   GfxPoly
  Headline
   The ancestor class of complex Gfx objects
 Node
  Key
   GfxList
  Headline
   A list of Gfx objects
  Description
   Text
    A class that represents a list of Gfx objects, displayed together. See also @ TO{gfx} @.
 Node
  Key
   GfxCircle
  Headline
   An SVG circle
  Description
   Text
    An SVG circle. The two compulsory options are GfxCenter (coordinates of the center) and GfxRadius (radius).
    In 3d, gives a decent approximation of a sphere.
   Example
    GfxCircle{GfxCenter=>vector {10,10},GfxRadius=>50,"fill"=>"green","stroke"=>"none"}
    GfxCircle{(10,10),10} -- equivalent syntax for coordinates
 Node
  Key
   GfxLine
  Headline
   An SVG line
  Description
   Text
    A simple SVG line. The two compulsory options are GfxPoint1 and GfxPoint2, which are vectors (or sequences) describing the two endpoints.
   Example
    GfxLine{GfxPoint1=>vector{0,0},GfxPoint2=>vector{2,1},"stroke"=>"green"}
    GfxLine{(0,0),(2,1),"stroke-width"=>0.1} -- simplified syntax
 Node
  Key
   GfxLight
  Headline
   A source of light
  Description
   Text
    A source of light for a 3d SVG picture.   
    This corresponds to the SVG "specular" lighting, use the property GfxSpecular. The location is given by GfxCenter.
    By default a GfxLight is invisible (it has GfxRadius 0) and is unaffected by matrix transformations outside it (GfxStatic true).
   Example
    GfxLight{GfxRadius=>10,"fill"=>"yellow"}
    v={(74.5571, 52.0137, -41.6631),(27.2634, -29.9211, 91.4409),(-81.3041, 57.8325, 6.71156),(-20.5165, -79.9251, -56.4894)};
    f={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
    c={"red","green","blue","yellow"};
    tetra=gfx(apply(4,i->GfxPolygon{f#i,"fill"=>c#i,"stroke"=>"none"}),GfxLight{(100,0,0),GfxRadius=>10},GfxRange=>{(-100,-100),(100,100)},GfxHeight=>30,GfxMatrix=>gfxRotation(-1.5,(0,1,0)))
 Node
  Key
   GfxEllipse
  Headline
   An SVG ellipse
  Description
   Text
    An SVG ellipse. The three compulsory options are GfxCenter (coordinates of the center) and GfxRadiusX, GfxRadiusY (radii).
   Example
    GfxEllipse{GfxCenter=>vector{10,10},GfxRadiusX=>50,GfxRadiusY=>20,"stroke"=>"none","fill"=>"red"}
    GfxEllipse{(10,10),50,20,"stroke"=>"blue"} -- equivalent syntax
  Caveat
   Does not really make sense in a 3d context.
 Node
  Key
   GfxPath
  Headline
   An SVG path
  Description
   Text
    An SVG path. It follows the syntax of SVG paths, except successive commands must be grouped together in a list called GfxPathList.
   Example
    GfxPath{GfxPathList => {"M", (0, 25), "Q", (25, 25), (25, 0), "M", (50, 25), "Q", (25, 25), (25, 50)},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
 Node
  Key
   GfxPolygon
  Headline
   An SVG polygon
  Description
   Text
    An SVG polygon. The coordinates must form a list called GfxPoints. (the difference with GfxPolyline is that the last coordinate is reconnected to the first)
   Example
    GfxPolygon{GfxPoints=>{(0,10),(100,10),(90,90),(0,80)},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   GfxPolyline
  Headline
   An SVG sequence of lines
  Description
   Text
    An SVG sequence of lines. The coordinates must form a list called GfxPoints. (the difference with GfxPolygon is that the last coordinate is not reconnected to the first)
   Example
    GfxPolyline{GfxPoints=>{(0,10),(100,10),(90,90),(0,80)},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   GfxText
  Headline
   SVG text
  Description
   Text
    Some SVG text. The text itself is the option GfxString (a string). Text can be "stroke"d or "fill"ed.
    Font size should be specified with GfxFontSize.
   Example
    GfxText{(0,0),"Test","stroke"=>"red","fill"=>"none","stroke-width"=>0.5}
  Caveat
   Currently, cannot be rotated. (coming soon)
 Node
  Key
   gfx
  Headline
    Group together Gfx objects
  Description
   Text
    gfx(a,b,...,c, options) results in a new GfxList object containing a,b,...,c 
    and the given options.
   Example
    a=gfx(GfxLine{(-100, 15, 78), (-9, 100, 4)},GfxLine{(-96, -49, -100), (46, -100, 52)},GfxLine{(-100, -42, -51), (59, 100, 76)},GfxLine{(-100, 66, 54), (83, -100, -27)})
    b=gfx(GfxLine{(-30, 100, 20), (9, -100, 8)},GfxLine{(-78, -73, -100), (-64, 84, 100)},"stroke"=>"red")
    gfx(a,b,GfxWidth=>20)
 Node
  Key
   gfxRange
  Headline
    Range of view port
  Description
   Text
    gfxRange gives the range of view port occupied by a Gfx object, as computed by the package.
    See also @ TO{GfxRange} @.
  Caveat
    At the moment gfxRange does not take into account the width of "stroke"s.
 Node
  Key
   gfxIs3d
  Headline
   Whether a Gfx object is 3d
  Description
   Text
    Returns a boolean according to whether the Gfx object is 2d (false) or 3d (true).
 Node
  Key
   gfxDistance
  Headline
   Distance to the viewer
  Description
   Text
    Returns the distance (perpendicularly to the screen) to the viewer of a Gfx 3d object.
 Node
  Key
   gfxRotation   
  Headline
   Computes a rotation matrix
  Usage
   gfxRotation ( angle, axis, center)
   gfxRotation ( angle, center)
  Description
   Text
    Produces a rotation encoded as a 4x4 matrix that can be used as an argument to @TO{GfxMatrix}@ or @TO{GfxAutoMatrix}@.
    For a 3d rotation, use 3d vectors for axis and center.
    For a 2d rotation, use a 2d vector for the center.
    In both cases, the center is optional.
 Node
  Key
   gfxTranslation
  Headline
   Computes a translation matrix
  Description
   Text
    Produces a translation encoded as a 4x4 matrix that can be used as an argument to @TO{GfxMatrix}@ or @TO{GfxAutoMatrix}@.
    The vector can be 2d or 3d.
   Example
    v={(74.5571, 52.0137, -41.6631),(27.2634, -29.9211, 91.4409),(-81.3041, 57.8325, 6.71156),(-20.5165, -79.9251, -56.4894)};
    f={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
    tetra=gfx(apply(4,i->GfxPolygon{f#i,"fill"=>"white"}))
    g = memoize(n -> if n==0 then tetra else gfx apply(4,i->g(n-1)++{GfxMatrix=>gfxTranslation v#i}))
    apply(4,g)
  Usage
   gfxTranslation ( vector )
 Node
  Key
   GfxOneSided
  Description
   Text
    A property of @ TO{GfxPoly} @ 3d objects, means that polygons must be drawn only if they are facing the correct way.
 Node
  Key
   GfxRange
  Headline
   Fix the view port
  Description
   Text
    An option to fix manually the view port range of a Gfx object.
    Only has an effect if in the outermost Gfx object.
    See also @ TO{gfxRange} @ and @ TO{GfxMargin} @.
 Node
  Key
   GfxWidth
  Headline
   Set the width
  Description
   Text
    An option to fix the width of the Gfx object in line width units. 
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxPerspective
  Headline
   Set the amount of perspective
  Description
   Text
    A 4x4 matrix that is applied to 3d coordinates for perspective.
    After this tranformation, the coordinates must be (x,-y,-z,z/p) in the reference frame
    where the viewer is at (0,0,0) and the screen at z=-p.
    One can instead provide a real number p, which is equivalent to placing the screen 
    centered at z=0 and the viewer at (0,0,p).
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxHeight
  Headline
   Set the height
  Description
   Text
    An option to fix the height of the Gfx object in line width units. 
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxAutoMatrix
  Headline
   Create a rotation animation matrix
  Description
   Text
    An option to create a rotation animation for the Gfx 3d object.
    The value can be a single 4x4 matrix, or a list which is cycled.
    The syntax n => ... can be used to repeat a sequence n times (where 0 means infinity).
    The animation automatically loops (use {\tt 0 => \{ \}} to stop!)
    In order for the animation to work, Gfx.css and Gfx.js must be included in the web page.
   Example
    (anim1=gfxRotation(0.1,(0,0,1),(0,0,0)); anim2=gfxRotation(-0.1,(0,0,1),(0,0,0)); anim3 = { 5 => {5 => anim1, 5 => anim2}, 10 => anim1 });
    gfx(GfxPolygon{{(-1,0),(1,0.1),(1,-0.1)},"fill"=>"red",GfxAutoMatrix=>anim1},GfxCircle{(1,0),0.1},GfxCircle{(0,0),1})
    gfx(GfxPolygon{{(-1,0),(1,0.1),(1,-0.1)},"fill"=>"red",GfxAutoMatrix=>anim3},GfxCircle{(1,0),0.1},GfxCircle{(0,0),1})
 Node
  Key
   GfxMatrix
  Headline
   Create a rotation matrix
  Description
   Text
    An option to rotate the coordinates of the Gfx 3d object.
    Must be a 4x4 matrix (projective coordinates).
   Example
    a=GfxPolygon{{(-1,0),(1,0.1),(1,-0.1)},"fill"=>"red"}
    gfx(a,a++{GfxMatrix=>gfxRotation(2*pi/3)})
 Node
  Key
   GfxBlur
  Headline
   An option to blur a Gfx object
  Description
   Text
    This corresponds to the feGaussianBlur SVG filter. 
    The value is the amount of blurriness relative to the size of the object.
  Caveat
    In animated 3d, the amount of blurriness does not vary as the size varies.
 Node
  Key
   GfxStatic
  Headline
   An option to make a Gfx object unmoving
  Description
   Text
    The Gfx 3d object is unaffected by matrix tranformations of its ancestors.
 Node
  Key
   gfxLinearGradient
  Headline
   An SVG gradient
  Description
   Text
    This corresponds to the linearGradient SVG gradient.
    The argument is a list of pairs of offsets and styles.
    Optional arguments (e.g., "x1", "y1", "x2", "y2") are used to determine the orientation of the gradient.
   Example
    GfxEllipse{(60,60),40,30, "fill"=>gfxLinearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   gfxRadialGradient
  Headline
   An SVG gradient
  Description
   Text
    This corresponds to the radialGradient SVG gradient.
    The argument is a list of pairs of offsets and styles.
    Optional arguments (e.g., "cx", "cy", "r", "fx", "fy") are used to position the gradient.
   Example
    GfxEllipse{(60,60),40,30, "fill"=>gfxRadialGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   gfxPlot
  Headline
   Draws a curve or surface
  Description
   Text
    Draws a curve or surface defined implicitly or explicitly by a polynomial.
    The first argument is a polynomial, the second is a (list of) range(s) of variable(s).
    If the number of ranges is equal to the number of variables of the polynomial, the graph of the polynomial
    is drawn. If it is one fewer, then the zero set of the polynomial is drawn.
    The option GfxMesh specifies the number of sampled values of the variables.
   Example
    R=RR[x,y];
    P=y^2-(x+1)*(x-1)*(x-2);
    gfxPlot(P,{-2,3},"stroke-width"=>0.05,GfxHeight=>25,"stroke"=>"red")
 Node
  Key
   GfxAxes
  Headline
   An option to draw axes
 Node
  Key
   GfxMargin
  Headline
   An option to specify the margin
  Description
   Text
    The margin is proportional to the size of the image.
    It increases the view port beyond the value returned by @ TO{gfxRange} @ or set by @ TO{GfxRange} @.
   Example
    GfxCircle{"fill"=>"red","stroke"=>"none",GfxMargin=>0}
    GfxCircle{"fill"=>"red","stroke"=>"none",GfxMargin=>0.5}
 Node
  Key
   gfxArrow
  Headline
   A marker to add to paths
  Description
   Text
    Must be used as styling options "marker-start", "marker-mid" or "marker-end", to add an arrow to a path.
   Example
    GfxPolyline{GfxPoints=>{(0,0),(50,50),(0,100),(50,150)},"stroke"=>"yellow","stroke-width"=>5,"marker-end"=>gfxArrow("fill"=>"orange"),GfxMargin=>0.3}
 Node
  Key
   GfxHtml
  Headline
   Html content
  Description
   Text
    Some arbitrary HTML content, specified by the option GfxString (a string).
  Caveat
   Due to a limitation of <foreignObject>, coordinates are rounded to the nearest integer. So use large enough coordinate systems.
 Node
  Key
   SVG
  Headline
   hypertext SVG item
///

end--

-- ex of use
gr=gfxLinearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")};
gfx(GfxEllipse{(0,0),90,30,"stroke"=>"none","fill"=>gr,GfxBlur=>0.3},GfxText{(-65,-7),"Macaulay2",GfxFontSize=>25,"stroke"=>"black","fill"=>"white"},GfxHeight=>12)

a=GfxCircle{"fill"=>"yellow","stroke"=>"green",GfxWidth=>1,GfxHeight=>1}
b=GfxLine{(10,10),(20,50),"stroke"=>"black"}
c=GfxCircle{(50,50),50,"fill"=>"blue","fill-opacity"=>0.25}
d=GfxEllipse{(60,60),40,30, "fill"=>"blue", "stroke"=>"grey"}
e=GfxPolyline{{(0,0),(100,100),(100,50)},"fill"=>"pink","stroke"=>"green"}
f=GfxPolygon{{(0,10),(100,10),(90,90),(0,80)},"stroke"=>"red","fill"=>"white"}
gfx (f,a,b,c,d,e)
-- or
rgb={"red","green","blue"};
scan(rgb, x -> (value x <- GfxCircle{"fill"=>x,"stroke"=>"black",GfxWidth=>0.8,GfxHeight=>0.8,GfxMargin=>0}))
value\rgb
R=QQ[x_red,x_green,x_blue]
describe R
x_red^2-x_green^2
factor oo
-- worse:
OO.texMath = texMath green;
OO_(Proj R)

-- or
z=GfxRectangle{"fill"=>"white"}
b1=GfxPath{{"M", (0, 25), "Q", (25, 25), (25, 0), "M", (50, 25), "Q", (25, 25), (25, 50)},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
b2=GfxPath{{"M", (0, 25), "Q", (25, 25), (25, 0), "M", (50, 25), "Q", (25, 25), (25, 50)},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
b=gfx(z,b1,b2,GfxWidth=>2,GfxHeight=>2,GfxMargin=>0)
a1=GfxPath{{"M", (50, 25), "Q", (25, 25), (25, 0), "M", (0, 25), "Q", (25, 25), (25, 50)},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
a2=GfxPath{{"M", (50, 25), "Q", (25, 25), (25, 0), "M", (0, 25), "Q", (25, 25), (25, 50)},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
a=gfx(z,a1,a2,GfxWidth=>2,GfxHeight=>2,GfxMargin=>0)
ab=a|b
ba=b|a
ab||ba||ba
tile = (I,i,j)->(if m_(i+1,j+1)%I == 0 then if c_(i+1,j+1)%I==0 then () else a else b);
tiledRow = (I,i)->new RowExpression from apply(n,j->tile(I,i,j));
loopConfig = I->new ColumnExpression from apply(k,i->tiledRow(I,i));


-- or
barside1=GfxPath{{"M",(80,60,100),"L",(80,55,100),"L",(220,55,100),"L",(220,60,100),"Z"},"fill"=>"#222","stroke-width"=>0}; -- stroke-width shouldn't be necessary
triangle1=GfxPath{{"M",(-50,160,2),"L",(0,80,2),"L",(50,160,2),"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
triangle2=GfxPath{{"M",(30,160,98),"L",(80,80,98),"L",(130,160,98),"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
edge1=GfxPath{{"M",(30,160,98),"L",(30,160,102),"L",(80,80,102),"L",(80,80,98),"Z"},"fill"=>"#4080e0","stroke-width"=>1};
edge2=GfxPath{{"M",(130,160,98),"L",(130,160,102),"L",(80,80,102),"L",(80,80,98),"Z"},"fill"=>"#4080e0","stroke-width"=>1};
bartop=GfxPath{{"M",(80,55,98),"L",(80,55,102),"L",(220,55,102),"L",(220,55,98),"Z"},"fill"=>"#aaa","stroke-width"=>0}; -- stroke-width shouldn't be necessary
thread=GfxPath{{"M",(80,55,100),"L",(80,80,100),"Z"},"stroke"=>"#111","stroke-width"=>0.5,"stroke-opacity"=>0.8};
gfx{barside1,triangle1,triangle2,edge1,edge2,bartop,thread}

-- tetrahedron
v={(74.5571, 52.0137, -41.6631),(27.2634, -29.9211, 91.4409),(-81.3041, 57.8325, 6.71156),(-20.5165, -79.9251, -56.4894)};
c={"red","green","blue","yellow"};
vv={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
triangles=apply(4,i->GfxPath{{"M",vv#i#0,"L",vv#i#1,"L",vv#i#2,"Z"},"fill"=>c#i,GfxOneSided=>true});
gfx(triangles,GfxLight{(100,0,0),GfxRadius=>10},GfxRange=>{(-100,-150),(150,150)},GfxHeight=>30,GfxMatrix=>gfxRotation(-1.5,(0,1,0)))

-- dodecahedron
vertices={vector{-137.638,0.,26.2866},vector{137.638,0.,-26.2866},vector{-42.5325,-130.902,26.2866},vector{-42.5325,130.902,26.2866},vector{111.352,-80.9017,26.2866},vector{111.352,80.9017,26.2866},vector{-26.2866,-80.9017,111.352},vector{-26.2866,80.9017,111.352},vector{-68.8191,-50.,-111.352},vector{-68.8191,50.,-111.352},vector{68.8191,-50.,111.352},vector{68.8191,50.,111.352},vector{85.0651,0.,-111.352},vector{-111.352,-80.9017,-26.2866},vector{-111.352,80.9017,-26.2866},vector{-85.0651,0.,111.352},vector{26.2866,-80.9017,-111.352},vector{26.2866,80.9017,-111.352},vector{42.5325,-130.902,-26.2866},vector{42.5325,130.902,-26.2866}};
faces={{14,9,8,13,0},{1,5,11,10,4},{4,10,6,2,18},{10,11,7,15,6},{11,5,19,3,7},{5,1,12,17,19},{1,4,18,16,12},{3,19,17,9,14},{17,12,16,8,9},{16,18,2,13,8},{2,6,15,0,13},{15,7,3,14,0}};
centers=apply(faces,f->1/5*sum(f,i->vertices#i));
steps=30;
dodeca=apply(faces,centers,(f,c)->GfxPolygon{apply(f,j->vertices#j),"fill"=>concatenate("rgb(",toString(134+round(1.2*c_0)),",",toString(134+round(1.2*c_1)),",",toString(134+round(1.2*c_2)),")")});
label=apply(#vertices,i->GfxText{vertices#i,toString i});
dodecasplit=apply(faces,centers,(f,c)->GfxPolygon{apply(f,j->vertices#j),
	GfxAutoMatrix=>apply(steps,j->gfxRotation(2*pi/5/steps*4*min(j/steps,1-j/steps),c,c)*gfxTranslation(0.075*sin(2*pi*j/steps)*c)),
	"fill"=>concatenate("rgb(",toString(134+round(1.2*c_0)),",",toString(134+round(1.2*c_1)),",",toString(134+round(1.2*c_2)),")")});
d=gfx(dodecasplit,"fill-opacity"=>0.65,GfxAutoMatrix=>gfxRotation(0.02,(1,2,3)));
d1=gfx(d,GfxMatrix=>gfxTranslation(200,0,0)); -- using alternate syntax of Sequence instead of Vector
d2=gfx(d,GfxMatrix=>gfxTranslation(-200,0,0));
gfx(d1,d2,GfxRange=>{vector{-400,-400},vector{400,400}},GfxHeight=>25,"stroke-width"=>2)

p=random splice{0..11};


-- icosahedron
vertices={vector{0.,0.,-95.1057},vector{0.,0.,95.1057},vector{-85.0651,0.,-42.5325},vector{85.0651,0.,42.5325},vector{68.8191,-50.,-42.5325},vector{68.8191,50.,-42.5325},vector{-68.8191,-50.,42.5325},vector{-68.8191,50.,42.5325},vector{-26.2866,-80.9017,-42.5325},vector{-26.2866,80.9017,-42.5325},vector{26.2866,-80.9017,42.5325},vector{26.2866,80.9017,42.5325}};
faces={{1,11,7},{1,7,6},{1,6,10},{1,10,3},{1,3,11},{4,8,0},{5,4,0},{9,5,0},{2,9,0},{8,2,0},{11,9,7},{7,2,6},{6,8,10},{10,4,3},{3,5,11},{4,10,8},{5,3,4},{9,11,5},{2,7,9},{8,6,2}};
icosa=apply(faces,f->GfxPolygon{apply(f,j->vertices#j),"fill"=>"gray","stroke"=>"none"});
i=gfx(icosa,GfxMatrix=>matrix{{0.7,0,0,0},{0,0.7,0,0},{0,0,0.7,0},{0,0,0,1}})

rnd = () -> random(-1.,1.); cols={"red","green","blue","yellow","magenta","cyan"};
gfx(i, apply(cols, c -> GfxLight{100*vector{1.5+rnd(),rnd(),rnd()},GfxRadius=>10,"fill"=>c,GfxSpecular=>10,GfxAutoMatrix=>gfxRotation(0.02,(rnd(),rnd(),rnd()))}),GfxRange=>{(-200,-200),(200,200)},GfxHeight=>30)

subdivide = (v,f) -> (
    u := v#0;
    c := u_0*u_0+u_1*u_1+u_2*u_2;
    e := unique flatten apply(f,x->{sort{x#0,x#1},sort{x#0,x#2},sort{x#1,x#2}});
    mid := apply(e, x -> (u=0.5*(v#(x#0)+v#(x#1)); r:=sqrt(c/(u_0*u_0+u_1*u_1+u_2*u_2)); r*u));
    ff := flatten apply(f, x -> (
	    i:=#v+position(e,y->y==sort{x#0,x#1});
	    j:=#v+position(e,y->y==sort{x#0,x#2});
	    k:=#v+position(e,y->y==sort{x#1,x#2});
	    {{x#0,i,j},{x#1,i,k},{x#2,j,k},{i,j,k}}
	    ));
    (v|mid,ff)
    )
(v2,f2)=subdivide(vertices,faces);
(v3,f3)=subdivide(v2,f2);
sph=apply(f3,f->GfxPolygon{apply(f,j->v3#j),"stroke"=>"white","stroke-width"=>0.01,"fill"=>"gray"});
gfx(sph, apply(cols, c -> GfxLight{100*vector{1.5+rnd(),rnd(),rnd()},GfxRadius=>10,"fill"=>c,GfxSpecular=>10,GfxAutoMatrix=>gfxRotation(0.02,(rnd(),rnd(),rnd()))}),GfxRange=>{(-200,-200),(200,200)},GfxHeight=>30)

-- simple plot
R=RR[x,y]; P=0.1*(x^2-y^2);
gfx(gfxPlot(P,{{-10,10},{-10,10}},GfxMesh=>15,"stroke-width"=>0.05,"fill"=>"gray"),GfxLight{(200,0,-500),GfxSpecular=>10,"fill"=>"rgb(180,0,100)"},GfxLight{(-200,100,-500),GfxSpecular=>10,"fill"=>"rgb(0,180,100)"},GfxHeight=>40,GfxAxes=>false)

-- implicit plot
R=RR[x,y];
P=y^2-(x+1)*(x-1)*(x-2);
gfxPlot(P,{-2,3},"stroke-width"=>0.05,GfxHeight=>25,"stroke"=>"red")

-- Schubert calculus
a=gfx(GfxLine{(-100, 15, 78), (-9, 100, 4)},GfxLine{(-96, -49, -100), (46, -100, 52)},GfxLine{(-100, -42, -51), (59, 100, 76)},GfxLine{(-100, 66, 54), (83, -100, -27)})
b=gfx(GfxLine{(-30, 100, 20), (9, -100, 8)},GfxLine{(-78, -73, -100), (-64, 84, 100)},"stroke"=>"red")
c=gfx(GfxPolygon{{(-100,100,100),(-100,-100,100),(-100,-100,-100),(-100,100,-100)}},
		  GfxPolygon{{(100,100,100),(100,-100,100),(100,-100,-100),(100,100,-100)}},
		  GfxPolygon{{(100,-100,100),(-100,-100,100),(-100,-100,-100),(100,-100,-100)}},
		  GfxPolygon{{(100,100,100),(-100,100,100),(-100,100,-100),(100,100,-100)}},
		  GfxPolygon{{(100,100,-100),(100,-100,-100),(-100,-100,-100),(-100,100,-100)}},
		  GfxPolygon{{(100,100,100),(100,-100,100),(-100,-100,100),(-100,100,100)}},
		  "stroke"=>"black","fill"=>"grey", "opacity"=>"0.25")
gfx(a,b,c,GfxWidth=>20)

-- stars
n=100;
speed=100;
far=-10000;
screen=1000;
stars=apply(1..n,i->(
z=speed*(random(far,screen)//speed);
GfxCircle{(random(-200,200),random(-200,200),z),10,"fill"=>"yellow","stroke"=>"none",GfxBlur=>0.3,
GfxAutoMatrix=>{((screen-z)//speed)=>gfxTranslation (0,0,speed),gfxTranslation (0,0,far-screen),((-far+z)//speed)=>gfxTranslation (0,0,speed)}}
));
gfx(stars,GfxRange=>{(-100,-100),(100,100)})
style(SVG oo,"background"=>"black")

-- removed
 Node
  Key
   GfxRectangle
  Headline
   An SVG rectangle
  Description
   Text
    An SVG rectangle. The SW coordinate is given as GfxPoint, the difference between NE and SW corners is given as GfxSize.
   Example
    GfxRectangle{(10,10),(20,50),"fill"=>"pink","stroke"=>"black"} -- first argument is GfxPoint, second GfxSize
  Caveat
   GfxRectangle can only be used in 2d. Use GfxPolygon for 3d.

