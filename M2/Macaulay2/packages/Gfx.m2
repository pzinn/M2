-- -*- coding: utf-8 -*-
newPackage(
        "Gfx",
        Version => "0.1", 
        Date => "May 18, 2018",
        Authors => {{Name => "Paul Zinn-Justin", 
                  Email => "pzinn@unimelb.edu.au", 
                  HomePage => "http://http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "A package to produce SVG graphics",
        DebuggingMode => false,
	AuxiliaryFiles => true
        )

export{"GfxType", "GfxObject", "GfxPrimitive", "GfxPolyPrimitive",
    "GfxList", "GfxCircle", "GfxLight", "GfxEllipse", "GfxPath", "GfxPolygon", "GfxPolyline", "GfxText", "GfxLine", "GfxHtml",
    "gfx", "gfxRange", "gfxIs3d", "gfxDistance", "gfxRotation", "gfxTranslation", "gfxLinearGradient", "gfxRadialGradient", "gfxArrow", "gfxPlot",
    "GfxContents", "GfxOneSided", "GfxScaledRadius", "GfxRadiusX", "GfxRadiusY", "GfxSpecular", "GfxVertical", "GfxPoint1", "GfxPoint2", "GfxPoint", "GfxScaledRadiusX", "GfxScaledRadiusY", "GfxRange", "GfxWidth",
    "GfxDistance", "GfxPerspective", "GfxFontSize", "GfxFilterTag", "GfxCenter", "GfxHorizontal", "GfxHeight", "GfxAutoMatrix", "GfxMatrix", "GfxGadgets", "GfxPoints", "GfxRadius",
    "GfxAuto", "GfxBlur", "GfxIs3d", "GfxSize", "GfxStatic", "GfxString", "GfxPathList", "GfxTag", "GfxAxes", "GfxMargin"
    }

GfxObject = new Type of OptionTable -- ancestor type

new GfxObject := T -> new T from { symbol cache => new CacheTable } -- every Gfx object should have a cache

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
-- * GfxGadgets for 3d picture -- list of Gfx{Vertical, Horizontal, Auto}
-- * GfxAxes (draw axes)

-- 3d: turns on lights, enables sorting, shows gadgets, axes are diff

currentGfxMatrix := null; -- yeah, it's a ``global'' variable -- scary
currentGfxPMatrix := null; -- the perspective matrix -- used for unmoving objects
currentGfxLights := {}; -- list of lights. needs to be preprocessed
currentGfxDefs = new MutableHashTable; -- list of defs. postprocessed

GfxType = new Type of Type -- all usable Gfx objects are ~ self-initialized

gfxParseFlag = false;
gfxParse = method()
gfxParse Array := x -> gfxParse vector(toList x)
gfxParse VisibleList := x -> apply(x,gfxParse)
gfxParse HashTable := x -> applyValues(x,gfxParse)
gfxParse CacheTable := identity
gfxParse Option := x -> x#0 => gfxParse x#1
gfxParse Thing := identity
gfxParse Matrix := identity -- TODO: convert 2,3d -> 4d
gfxParse Vector := x -> if rank class x === 2 then x || vector {0,1.} else (gfxParseFlag=true; if rank class x === 3 then x || vector {1.} else if rank class x === 4 then x else error "wrong coordinates")

GfxType List := (T,opts) -> (
    opts0 := T.Options;
    -- scan the first few arguments in case we skipped the keys for standard arguments. also, turn into a list and parse
    gfxParseFlag = false;
    temp := gfxParse((new GfxObject) ++ opts0 ++ apply(#opts, i -> if i < #opts0 and class opts#i =!= Option then opts0#i#0 => opts#i else opts#i));
    new T from temp ++ {symbol GfxIs3d => gfxParseFlag}
)

gfxRange1 = method() -- returns [xmin,ymin],[xmax,ymax]
gfxRange1 GfxObject := x -> null

gfxIs3d = x -> if x.?GfxIs3d then x.GfxIs3d else error "BUG"; --false; -- TODO: finalize
gfxIs3d1 = method()
gfxIs3d1 GfxObject := x -> false -- by default, 2d

gfxDistance = x -> if x.cache.?GfxDistance then x.cache.GfxDistance else error "distance of 3d object can only be obtained by rendering it"
gfxDistance1 = method()
gfxDistance1 GfxObject := x -> 0_RR -- by default 2d -> on top of everything

updateGfxCache = g -> (
    g.cache.GfxRange = gfxRange1 g; -- update the range
    g.cache.GfxDistance = gfxDistance1 g; -- update the squared distance
    if g.?GfxOneSided and g.GfxOneSided then gfxDetermineSide g;
    -- bit of a hack: 2d objects GfxCircle, GfxEllipse get scaled in a 3d context
    if instance(g,GfxCircle) then (
	scale := 1/(currentGfxMatrix*g.GfxCenter)_3;
	g.cache.GfxScaledRadius=g.GfxRadius*scale;
	) else if instance(g,GfxEllipse) then (
	scale = 1/(currentGfxMatrix*g.GfxCenter)_3;
	g.cache.GfxScaledRadiusX=g.GfxRadiusX*scale;
	g.cache.GfxScaledRadiusY=g.GfxRadiusY*scale;
	)
    )

--gfxRange = x -> if x.?GfxRange then x.GfxRange else if x.cache.?GfxRange then x.cache.GfxRange else if not gfxIs3d x then gfxRange1 x else error "range of 3d object can only be obtained by rendering it"
gfxRange = x -> if x.cache.?GfxRange then x.cache.GfxRange else if not gfxIs3d x then gfxRange1 x else error "range of object can only be obtained by rendering it"

project2d = x -> (
    xx := currentGfxMatrix*x;
    vector {xx_0/xx_3,xx_1/xx_3}
    )

GfxPrimitive = new Type of GfxObject

GfxCircle = new GfxType of GfxPrimitive from hashTable { symbol Name => "circle", symbol Options => { symbol GfxCenter => vector {0.,0.}, symbol GfxRadius => 50. }}
gfxIs3d1 GfxCircle := x -> rank class x.GfxCenter > 2
gfxRange1 GfxCircle := g -> (
    p := currentGfxMatrix * g.GfxCenter;
    r:=g.GfxRadius/p_3;
    p=vector {p_0/p_3,p_1/p_3};
    r = vector {r,r};
    { p - r, p + r }
    )
gfxDistance1 GfxCircle := g -> (
    y := currentGfxMatrix * g.GfxCenter;
    y_0^2+y_1^2+y_2^2
    )

GfxEllipse = new GfxType of GfxPrimitive from hashTable { symbol Name => "ellipse", symbol Options => { symbol GfxCenter => vector {0.,0.}, symbol GfxRadiusX => 50., symbol GfxRadiusY => 50. }}
gfxIs3d1 GfxEllipse := x -> rank class x.GfxCenter > 2
gfxRange1 GfxEllipse := g -> (
    p := currentGfxMatrix * g.GfxCenter;
    rx:=g.GfxRadiusX/p_3; ry:=g.GfxRadiusY/p_3;
    p=vector {p_0/p_3,p_1/p_3};
    r := vector {rx,ry};
    { p - r, p + r }
    )
gfxDistance1 GfxEllipse := g -> (
    y := currentGfxMatrix * g.GfxCenter;
    y_0^2+y_1^2+y_2^2
    )

GfxText = new GfxType of GfxObject from hashTable { symbol Name => "text", symbol Options => { GfxPoint => vector {0.,0.}, GfxString => "" }}
gfxRange1 GfxText := g -> (
    f := if g.?GfxFontSize then g.GfxFontSize else 14.;
    p := currentGfxMatrix*g.GfxPoint;
    f=f/p_3;
    p=vector {p_0/p_3,p_1/p_3};
    { p - vector {0,f}, p + vector{f*0.6*length g.GfxString,0} } -- very approximate TODO properly
    )

GfxLine = new GfxType of GfxPrimitive from hashTable { symbol Name => "line", symbol Options => { GfxPoint1 => vector {0.,0.}, GfxPoint2 => vector {50.,50.} }}
gfxIs3d1 GfxLine := x -> rank class x.GfxPoint1 > 2
gfxRange1 GfxLine := g -> (
    p1 := project2d g.GfxPoint1;
    p2 := project2d g.GfxPoint2;
    p := transpose{entries p1,entries p2};
    { vector(min\p), vector(max\p) }
    )
gfxDistance1 GfxLine := g -> (
    p1 := currentGfxMatrix * g.GfxPoint1;
    p2 := currentGfxMatrix * g.GfxPoint1;
    0.5*(p1_0^2+p1_1^2+p1_2^2+p2_0^2+p2_1^2+p2_2^2)
    )


GfxPolyPrimitive = new Type of GfxPrimitive;

GfxPolyline = new GfxType of GfxPolyPrimitive from hashTable { symbol Name => "polyline", symbol Options => { symbol GfxPoints => {} }}
GfxPolygon = new GfxType of GfxPolyPrimitive from hashTable { symbol Name => "polygon", symbol Options => { symbol GfxPoints => {} }}
--GfxPathCmds = new HashTable from {"M"=>1,"L"=>1,"Q"=>2,"S"=>2,"T"=>1,"C"=>3,"A"=>5,"Z"=>0} -- number of args (coords count as 1)
GfxPath = new GfxType of GfxPolyPrimitive from hashTable { symbol Name => "path", symbol Options => { symbol GfxPathList => {} }}
gfxRange1 GfxPolyPrimitive := g -> ( -- relative coordinates *not* supported, screw this
    if instance(g,GfxPath) then s := select(g.GfxPathList, x -> instance(x,Vector)) else s = g.GfxPoints;
    s = transpose apply(s, x -> entries project2d x);
    {vector(min\s), vector(max\s)}
    )

-- to make lists of them
GfxList = new GfxType of GfxObject from hashTable { symbol Name => "g", symbol Options => { symbol GfxContents => {} } }
-- slightly simpler syntax: gfx (a,b,c, opt=>xxx) rather than GfxList { {a,b,c}, opt=>xxx }. plus updates is3d correctly!
gfx = true >> opts -> x -> (
    x=if instance(x,BasicList) then select(flatten toList x, y -> y =!=null) else {x};
    gfxParseFlag = false;
    opts = gfxParse opts;
    new GfxList from (new GfxObject) ++ opts ++ { symbol GfxContents => x, symbol GfxIs3d => gfxParseFlag or any(x,y->y.GfxIs3d) }
    )
gfxRange1 GfxList := x -> (
    s := select(apply(x.GfxContents, gfxRange),x->x=!=null);
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
GfxHtml = new GfxType of GfxText from hashTable { symbol Name => "foreignObject", symbol Options => { GfxPoint => vector {0.,0.}, GfxString => "" }}
gfxRange1 GfxHtml := g -> (
    p := project2d g.GfxPoint;
    { p, p } -- TODO properly
    )

--GfxList | GfxList := (a,b) -> new GfxList from (a++b++{symbol GfxContents => a.GfxContents | b.GfxContents})
--GfxObject | GfxObject := (a,b) -> new RowExpression from {a,b}
--GfxObject || GfxObject := (a,b) -> new ColumnExpression from {a,b}

-- GfxAnimation takes a Gfx option Obj (what to animate) -> shit, disabled
--GfxAnimation = new GfxType from ("animate", { "attributeName" => "", "from" => "", "to" => "", "dur" => "", "attributeType" => "XML", "repeatCount" => "indefinite" })
--gfxRange1 GfxAnimation := g -> gfxRange1 g#Obj;
--svg GfxAnimation := g -> svgBegin g#Obj | ((lookup(svg,GfxPrimitive)) g) | svgEnd g#Obj

-- for javascript stuff
jsString = method(Dispatch=>Thing)
jsString Thing := toString
jsString String := toExternalString
jsString Matrix := x -> "new Matrix(" | jsString entries x | ")"
jsString Vector := x -> "new Float32Array(" | jsString entries x | ")"
jsString VisibleList := x -> "[" | demark(",",jsString\x) | "]"
jsString HashTable := x -> "{" | demark(",",apply(pairs x, (key,val) -> jsString key | ":" | jsString val)) | "}"
-- svg output
svgString = method(Dispatch=>Thing)
svgString Thing := toString
svgString List := x -> demark(" ", apply(x,svgString))
svgString Vector := x -> svgString entries project2d x 
svg = method()
svgLookup := hashTable { 
    symbol GfxMatrix => x -> "data-matrix='"|jsString x|"'",
    symbol GfxAutoMatrix => x -> "data-dmatrix='"|jsString x|"'",
    symbol GfxCenter => x -> concatenate(
	"data-center='",jsString x,"' ",
	(x = project2d x;),
	"cx='", toString x_0, "' cy='", toString x_1, "'"
	),
    symbol GfxRadius => x ->  "data-r='"|toString x|"'",
    symbol GfxRadiusX => x -> "data-rx='"|toString x|"'",
    symbol GfxRadiusY => x -> "data-ry='"|toString x|"'",
    symbol GfxScaledRadius => x ->  "r='"|toString x|"'",
    symbol GfxScaledRadiusX => x ->  "rx='"|toString x|"'",
    symbol GfxScaledRadiusY => x ->  "ry='"|toString x|"'",
    symbol GfxPathList => x -> "data-coords='"|jsString x|"' d='"|svgString x|"'",
    symbol GfxPoints => x -> "data-coords='"|jsString x|"' points='"|svgString x|"'",
    symbol GfxPoint => x -> concatenate(
	"data-point='",jsString x,"' ",
	(x = project2d x;),
	"x='", toString x_0, "' y='", toString x_1, "'"
	),
    symbol GfxPoint1 => x -> concatenate(
	"data-point1='",jsString x,"' ",
	(x = project2d x;),
	"x1='", toString x_0, "' y1='", toString x_1, "'"
	),
    symbol GfxPoint2 => x -> concatenate(
	"data-point2='",jsString x,"' ",
	(x = project2d x;),
	"x2='", toString x_0, "' y2='", toString x_1, "'"
	),
    symbol GfxSize => x -> ( -- 2d only?
	x = project2d x;
	concatenate("width='", toString x_0, "' height='", toString x_1, "'")
	),
    symbol GfxStatic => x -> if x then "data-pmatrix='"|jsString currentGfxPMatrix|"'" else "",
    symbol GfxTag => x -> "id='"| x |"'",
    symbol GfxFilterTag => x -> "filter=\"url(#" | x | ")\"",
    symbol GfxOneSided => x -> "data-onesided='"|jsString x|"'",
    symbol GfxFontSize => x -> "data-fontsize='"|jsString x|"'"
    }

updateGfxMatrix = g -> (
    first ( currentGfxMatrix,
    	if g.?GfxStatic and g.GfxStatic then currentGfxMatrix=currentGfxPMatrix, -- reset to perspective matrix	
	if g.?GfxMatrix then currentGfxMatrix = currentGfxMatrix*g.GfxMatrix
    )
)

svgBegin = g -> (
    gfxFilter g; -- set up filter if need be
    prs := pairs g | pairs g.cache;
    concatenate (
    	"<", (class g)#Name,
    	concatenate apply(prs,(key,val) -> if svgLookup#?key then " " | (svgLookup#key val) ),
    	(style := select(prs,(key,val) -> class key === String);
    	    if #style>0 then " style='" | demark(";",apply(style,(key,val) -> key|":"|toString val))|"'"),
    	">"
	)
    )
svgEnd = g -> (
    concatenate(
    "</", (class g)#Name, ">"
    )
)

svg GfxObject := g -> ""
svg GfxPrimitive := g -> ( 
    saveGfxMatrix := updateGfxMatrix g;
    updateGfxCache g;
    first(svgBegin g | svgEnd g,
    	currentGfxMatrix = saveGfxMatrix)
    )

-- careful that sort is *not* a stable sort (it's quicksort) so we can't use it in 2d. also means mixing 2d and 3d will be a mess :/
svg GfxList := g -> (
    saveGfxMatrix := updateGfxMatrix g;
    stuff := apply(g.GfxContents, svg);
    if gfxIs3d g then stuff = (transpose sort(transpose{g.GfxContents,stuff}))#1; -- sort stuff according to g.GfxContents distance
    updateGfxCache g;
    first(svgBegin g | concatenate stuff | svgEnd g,
    	currentGfxMatrix = saveGfxMatrix)
    )

svg GfxText := g -> (
    saveGfxMatrix := updateGfxMatrix g;
    -- choose font size
    f := if g.?GfxFontSize then g.GfxFontSize else 14.;
    f = f / (currentGfxMatrix*g.GfxPoint)_3;
    g.cache#"font-size"= toString f|"px";
    updateGfxCache g;
    first(svgBegin g | g.GfxString | svgEnd g,
    	currentGfxMatrix = saveGfxMatrix)
    )

svg GfxHtml := g -> (
    g.cache#"overflow"="visible";
    g.cache.GfxSize=vector{1,-1,0,1}; -- TEMP? weird bug with chrome. needs more testing
    (lookup(svg,GfxText)) g
    )

mathJax GfxObject := html
-- the 0.4 is approximate and should correspond to depth vs height of current font
texMath GfxObject := x -> (
    h := html x; -- this way height is computed
    "\\rawhtml{" | h | "}{" | toString((x.cache.GfxHeight+0.4)/2.) |"em}{" | toString((x.cache.GfxHeight-0.4)/2.) | "em}"
    )
expression GfxObject := hold

gfxIs3d1 GfxPolyline := g -> any(g.GfxPoints, x-> rank class x>2)
gfxIs3d1 GfxPolygon := lookup(gfxIs3d1, GfxPolyline)
gfxIs3d1 GfxPath := g -> any(select(g.GfxPathList, x -> instance(x,Vector)), x-> rank class x>2)
gfxIs3d1 GfxList := g -> any(g.GfxContents,gfxIs3d) -- really there can't be any mix at this stage
gfxIs3d1 GfxText := g -> rank class g.GfxPoint >2

gfxDistance1 GfxPolyPrimitive := x -> (
    if not gfxIs3d x then return 0_RR; -- default for 2d things
    if instance(x,GfxPath) then s := select(x.GfxPathList, y -> instance(y,Vector)) else s = x.GfxPoints;
    sum(apply(s,y->currentGfxMatrix*y), y -> y_0^2+y_1^2+y_2^2 ) / #s
    )
gfxDistance1 GfxList := x -> (
    if not gfxIs3d x then return 0_RR; -- default for 2d things
    sum(x.GfxContents, gfxDistance) / #(x.GfxContents)
    )
GfxObject ? GfxObject := (x,y) -> (gfxDistance y) ? (gfxDistance x)
gfxDistance1 GfxText := g -> (
    if not gfxIs3d g then return 0_RR; -- default for 2d things
    y := currentGfxMatrix*g.GfxPoint;
    y_0^2+y_1^2+y_2^2
    )

gfxTagCount := 0;
gfxTag = () -> (
    gfxTagCount=gfxTagCount+1;
    "Gfx_" | toString currentTime() | "_" | toString gfxTagCount
    )

-- with the headers
html GfxObject := g -> (
    persp := if g.?GfxPerspective then g.GfxPerspective else 1000.; -- some arbitrary number
    currentGfxMatrix = currentGfxPMatrix = if instance(persp,Matrix) then persp else matrix {{1,0,0,0},{0,-1,0,0},{0,0,-1,persp},{0,0,-1/persp,1}}; -- useful to have output {x,y,z+p,1+z/p}
    currentGfxLights = if gfxIs3d g then gfxSetupLights g else {};
    currentGfxDefs = new MutableHashTable;
    s := svg g; -- run this first because it will compute the ranges too
    if g.?GfxRange then r := g.GfxRange else r = gfxRange g; -- should be cached at this stage
    if r === null then (g.cache.GfxWidth=g.cache.GfxHeight=0.; return ""); -- nothing to draw
    r = apply(r,numeric);
    rr := r#1 - r#0;
    -- axes
    axes := null; axeslabels := null;
    if g.?GfxAxes and g.GfxAxes =!= false then ( -- semi temp: axes should be broken into little bits
	arr := gfxArrow();
	axes = gfx(
	    GfxLine { GfxPoint1 => vector if gfxIs3d g then {r#0_0,0,0} else {r#0_0,0}, GfxPoint2 => vector if gfxIs3d g then {r#1_0,0,0} else {r#1_0,0}, "marker-end" => arr },
	    GfxLine { GfxPoint1 => vector if gfxIs3d g then {0,r#0_1,0} else {0,r#0_1}, GfxPoint2 => vector if gfxIs3d g then {0,r#1_1,0} else {0,r#1_1}, "marker-end" => arr },
	    if gfxIs3d g then GfxLine { GfxPoint1 => vector{0,0,min(r#0_0,r#0_1)}, GfxPoint2 => vector {0,0,max(r#1_0,r#1_1)}, "marker-end" => gfxArrow() },
	    "stroke"=>"black", "stroke-width"=>0.01*min(rr_0,rr_1)
	    );
	axeslabels = gfx(
	    GfxHtml { GfxPoint => 1.06*vector if gfxIs3d g then {r#1_0,0,0} else {r#1_0,0}, GfxString => if instance(g.GfxAxes,List) then mathJax g.GfxAxes#0 else "\\(x\\)" , GfxFontSize => 0.08*min(rr_0,rr_1)},
	    GfxHtml { GfxPoint => 1.06*vector if gfxIs3d g then {0,r#1_1,0} else {0,r#1_1}, GfxString => if instance(g.GfxAxes,List) then mathJax g.GfxAxes#1 else "\\(y\\)", GfxFontSize => 0.08*min(rr_0,rr_1)},
	    if gfxIs3d g then GfxHtml { GfxPoint => 1.06*vector{0,0,max(r#1_0,r#1_1)}, GfxString => if instance(g.GfxAxes,List) then mathJax g.GfxAxes#2 else "\\(z\\)", GfxFontSize => 0.08*min(rr_0,rr_1)}
-*
	    	GfxText { GfxPoint => 1.06*vector if gfxIs3d g then {r#1_0,0,0} else {r#1_0,0}, GfxString => if instance(g.GfxAxes,List) then toString g.GfxAxes#0 else "x", GfxFontSize => 0.08*min(rr_0,rr_1)},
	    	GfxText { GfxPoint => 1.06*vector if gfxIs3d g then {0,r#1_1,0} else {0,r#1_1}, GfxString => if instance(g.GfxAxes,List) then toString g.GfxAxes#1 else "y", GfxFontSize => 0.08*min(rr_0,rr_1)},
	    	if gfxIs3d g then GfxText { GfxPoint => 1.06*vector{0,0,max(r#1_0,r#1_1)}, GfxString => if instance(g.GfxAxes,List) then toString g.GfxAxes#2 else "z", GfxFontSize => 0.08*min(rr_0,rr_1)},
		"stroke" => "none", "fill"=>"black"
		*-
	    );
	axes=svg axes;
	axeslabels=svg axeslabels;
	);
    if g.?GfxWidth then g.cache.GfxWidth = numeric g.GfxWidth;
    if g.?GfxHeight then g.cache.GfxHeight = numeric g.GfxHeight;
    if not (g.?GfxWidth or g.?GfxHeight) then -- by default, make it fit inside 16 x 10
	if rr_0 > 1.6*rr_1 then g.cache.GfxWidth = 16. else g.cache.GfxHeight = 10.;
    -- at this stage one of the two is set
    if not g.cache.?GfxHeight then g.cache.GfxHeight = g.cache.GfxWidth * (if rr_0 != 0 then rr_1/rr_0 else 10/16);
    if not g.cache.?GfxWidth then g.cache.GfxWidth = g.cache.GfxHeight * (if rr_1 != 1 then rr_0/rr_1 else 16/10);
    -- put some extra blank space around picture
    margin := if g.?GfxMargin then g.GfxMargin else 0.1;
    r = { r#0-margin*rr, r#1+margin*rr };
    --
    tag := gfxTag();
    concatenate(
	if gfxIs3d g then "<span class=\"gfx3d\">" else "",
	 -- svg first
	"<svg xmlns=\"http://www.w3.org/2000/svg\"",
	" class=\"M2Svg\" id=\""|tag|"\"",
	" style='width:",toString g.cache.GfxWidth,"em;height:",toString g.cache.GfxHeight,"em;",
    	if not g#?"stroke-width" then "stroke-width:"|toString(0.01*min(rr_0,rr_1)), -- define a default stroke-width
	"'",
    	" viewBox=\"",between(" ",toString \ {r#0_0,r#0_1,r#1_0-r#0_0,r#1_1-r#0_1}),"\"",
	" data-pmatrix='"|jsString currentGfxMatrix|"'",
    	">",
	axes, axeslabels,
    	s,
	if #currentGfxDefs>0 then "<defs>" | concatenate values currentGfxDefs | "</defs>",
    	"</svg>",
	-- then sliders
	if gfxIs3d g and (not g.?GfxGadgets or member(symbol GfxVertical,g.GfxGadgets)) then
	    "<input oninput='gfxVRange(this,"|jsString tag|")' onclick='event.stopPropagation(); return false' type=\"range\" orient=\"vertical\" style=\"position:absolute;top:0;right:0\" min=\"-100\" max=\"100\" class=\"vertical gadget\">"
	    else "",
	if gfxIs3d g and (not g.?GfxGadgets or member(symbol GfxHorizontal,g.GfxGadgets)) then
	    "<input oninput='gfxHRange(this,"|jsString tag|")' onclick='event.stopPropagation(); return false' type=\"range\" style=\"position:absolute;bottom:0;left:0\" min=\"-100\" max=\"100\" class=\"horizontal gadget\">"
	    else "",
	-- then autorotate button
	if gfxIs3d g and (not g.?GfxGadgets or member(symbol GfxAuto,g.GfxGadgets)) then
	    "<button class=\"mdl-button mdl-button--icon gadget\" style=\"position:absolute;top:0;left:0\" onclick='gfxToggleRotation(this,"|jsString tag|"); event.stopPropagation(); return false'><i class=\"material-icons\">3d_rotation</i></button>"
	    else "",
	if gfxIs3d g then "</span>" else ""
	)
    )

-- now transformations
-- following 2 functions can be used to produce matrices to be fed to either 
-- GfxAutoMatrix (animation) or GfxMatrix (static)

gfxRotation = args -> (
    angle := args#0;
    axis := args#1;
    if instance(axis,Array) then axis = vector toList axis; -- let's be tolerant. for now.
    axis = promote(axis,RR);
    invr := 1/sqrt(axis_0^2+axis_1^2+axis_2^2);
    axis = invr*axis;
    cross := (axis#0)**transpose(axis#0);
    rot := cross + (sin angle) * matrix {{0,-axis_2,axis_1},{axis_2,0,-axis_0},{-axis_1,axis_0,0}} + (cos angle) * (1-cross);
    rot = rot ++ matrix {{1}};
    if #args==2 then rot else (
	center := args#2;
    	if instance(center,Array) then center = vector toList center; 
	(gfxTranslation(center))*rot*(gfxTranslation(-center))
    	)
    )
gfxTranslation = vec -> (
    if instance(vec,Array) then vec = vector toList vec;
    matrix {{1,0,0,vec_0},{0,1,0,vec_1},{0,0,1,vec_2},{0,0,0,1}}
)

gfxDetermineSide = method()
gfxDetermineSide GfxObject := x -> ()
gfxDetermineSide GfxPolyPrimitive := x -> (
    -- find first 3 coords
    if instance(x,GfxPath) then coords := select(x.GfxPathList, y -> instance(y,Vector)) else coords = x.GfxPoints;
    if #coords<3 then ( remove(x.cache,GfxFilterTag); return; );
    coords=apply(take(coords,3),x->(currentGfxMatrix*x)^{0,1,2});
    x.cache#"visibility" = if det(matrix coords#0 | matrix coords#1 | matrix coords#2) > 0 then "hidden" else "visible";
    )

-- lighting
GfxLight = new GfxType of GfxCircle from hashTable { Name => "circle", Options => { symbol GfxCenter => vector {0,0,0,1.}, "fill" => "#FFFFFF", symbol GfxSpecular => 64, symbol GfxRadius => 0, symbol GfxBlur => 0.3, symbol GfxStatic => true, "stroke" => "none" }}
-- in case it's drawn, it's a circle

-- gfxRange ignores lights if invisible
gfxRange1 GfxLight := x -> if x.GfxRadius === 0 then null else (lookup(gfxRange1,GfxCircle)) x

gfxSetupLights = method()
gfxSetupLights GfxObject := g -> {}
gfxSetupLights GfxList := g -> (
    	saveGfxMatrix := updateGfxMatrix g;
	first(
	    flatten apply(g.GfxContents,gfxSetupLights),
	    currentGfxMatrix = saveGfxMatrix
	    )
	)
gfxSetupLights GfxLight := g -> (
    saveGfxMatrix := updateGfxMatrix g;
    g.cache.GfxCenter = currentGfxMatrix*g.GfxCenter;
    currentGfxMatrix = saveGfxMatrix;
    g.cache.GfxTag = gfxTag();
    g )

gfxFilter = x -> if x.?GfxBlur or (#currentGfxLights > 0 and instance(x,GfxPolyPrimitive)) then (
    x.cache.GfxFilterTag = gfxTag();
    i:=0;
    if x.?GfxBlur then (
    	b := x.GfxBlur;
    	s := "<filter id=\""| x.cache.GfxFilterTag|"\" x=\""|toString(-100*b)|"%\" y=\""|toString(-100*b)|"%\" width=\""|toString(100*(1+2*b))|"%\" height=\""|toString(100*(1+2*b))|"%\">";
    	rng := x.cache.GfxRange; if rng =!= null then (
    	    drng:=rng#1-rng#0;
    	    r := b*min(drng_0,drng_1);
	    x.cache.GfxRange={rng#0-vector{r,r},rng#1+vector{r,r}};
    	    s=s|"<feGaussianBlur in=\"SourceGraphic\" result=\"result"|toString i|"\" stdDeviation=\""|toString(0.5*r)|"\" />"; -- problem is, this should be updated dynamically as radius changes...
	    i=i+1;
	)
    )
    else s = "<filter id=\""| x.cache.GfxFilterTag|"\">";
    if gfxIs3d x and (instance(x,GfxPolygon) or instance(x,GfxPolyline) or instance(x,GfxPath)) then (
    	-- find first 3 coords
    	if instance(x,GfxPath) then coords := select(x.GfxPathList, y -> instance(y,Vector)) else coords = x.GfxPoints;
    	if #coords>=3 then (
    	    coords=apply(take(coords,3),x->(currentGfxMatrix*x)^{0,1,2});
    	    d:=-det(matrix coords#0 | matrix coords#1 | matrix coords#2);
    	    u:=coords#1-coords#0; v:=coords#2-coords#0; w:=vector{u_1*v_2-v_1*u_2,u_2*v_0-v_2*u_0,u_0*v_1-v_0*u_1}; w2:=w_0*w_0+w_1*w_1+w_2*w_2;
    	    scan(currentGfxLights, g -> (
	    	    -- compute reflected coords
	    	    light := g.cache.GfxCenter; -- note the cached version is already rotated appropriately
	    	    p := light_2/light_3;
	    	    light=light^{0,1,2};
	    	    lightrel := light-coords#0;
	    	    sp := w_0*lightrel_0+w_1*lightrel_1+w_2*lightrel_2;
	    	    c := 2*sp/w2;
	    	    lightmir := light - c*w;
		    if d<0 then sp=-sp;
	    	    s=s| "<feSpecularLighting result=\"spec"|toString i|"\" specularExponent=\""|toString g.GfxSpecular|"\" lighting-color=\""|(if sp<0 then "black" else toString g#"fill")|"\">";
	    	    s=s|"<fePointLight data-origin=\""|g.cache.GfxTag|"\" x=\""|toString(lightmir_0*p/lightmir_2)|"\" y=\""|toString(lightmir_1*p/lightmir_2)|"\" z=\""|toString(sp/sqrt(w2))|"\" />";
	    	    s=s|"</feSpecularLighting><feComposite in=\"spec"|toString i|"\" in2=\"SourceGraphic\" operator=\"in\" result=\"clipspec"|toString i|"\"/>";
	    	    s=s|"<feComposite in=\""|(if i==0 then "SourceGraphic" else "result"|toString(i-1))|"\"  in2=\"clipspec"|toString i|"\" result=\"result"|toString i|"\" operator=\"arithmetic\" k1=\"0\" k2=\"1\" k3=\"1\" k4=\"0\" />";
	    	    i=i+1;
	    	    ));
	    );
	);
    currentGfxDefs#(x.cache.GfxFilterTag)=s|"</filter>";
    ) else remove(x.cache,GfxFilterTag);

GfxTagged = new Type of BasicList

net GfxTagged := toString GfxTagged := x -> (
    tag := x#0;
    if not currentGfxDefs#?tag then currentGfxDefs#tag=x#1;
    "url(#"|tag|")"
    )
texMath GfxTagged := texMath @@ toString

gfxLinearGradient = true >> o -> stop -> (
    tag := gfxTag();
    s:="<linearGradient id='"|tag|"'"|concatenate apply(pairs o,(key,val) -> " "|key|"='"|toString val|"'")|">";
    scan(stop, (offset,style) -> s = s | "<stop offset='"|offset|"' style='"|style|"' />");
    s=s|"</linearGradient>";
    new GfxTagged from (tag,s)
    )

gfxRadialGradient = true >> o -> stop -> (
    tag := gfxTag();
    s:="<radialGradient id='"|tag|"'"|concatenate apply(pairs o,(key,val) -> " "|key|"='"|toString val|"'")|">";
    scan(stop, (offset,style) -> s = s | "<stop offset='"|offset|"' style='"|style|"' />");
    s=s|"</radialGradient>";
    new GfxTagged from (tag,s)
    )

gfxArrow = true >> o -> () -> (
    tag := gfxTag();
    s:="<marker id='"|tag|"' orient='auto' markerWidth='3' markerHeight='4' refX='0' refY='2'>";
    saveGfxMatrix := currentGfxMatrix;
    currentGfxMatrix = map(RR^4,RR^4,1); -- ???
    s=s|(svg new GfxPolygon from (new GfxObject) ++ { "fill" => "black", "stroke" => "none" } ++ gfxParse o 
	++ { GfxIs3d=>false, GfxPoints => { vector {0,0,0,1}, vector {0,4,0,1}, vector {3,2,0,1} } } ); -- need to rethink this
    currentGfxMatrix = saveGfxMatrix;
    s=s|"</marker>";
    new GfxTagged from (tag,s)
    )

-*    
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

gfxPlot = true >> o -> (P,r) -> (
    R := ring P; -- R should have one or two variables
    if not instance(r,List) then error("incorrect ranges");
    if not instance(r#0,List) then r = { r };
    if #r>2 or (numgens R =!= #r and numgens R =!= #r+1) then error("incorrect number of variables / ranges");
    if numgens R === #r then R2 := coefficientRing R else R2 = (coefficientRing R) ( monoid [last gens R] );
    if (#r === 1) then ( r = r#0;
    	if (o.?GfxPoints) then n := o.GfxPoints else n = 100;
	val := transpose apply(n+1, i -> (
		x := i*(r#1-r#0)/n+r#0;
		f := map(R2,R, matrix { if numgens R === 1 then { x } else { x, R2_0 } });
		y := if numgens R === 1 then { f P } else sort apply(solveSystem { f P }, p -> first p.Coordinates); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(y, yy -> if abs imaginaryPart yy < 1e-6 then vector { x, realPart yy })));
	new GfxList from (new GfxObject) ++ { "fill"=>"none", GfxAxes=>gens R, GfxIs3d=>false } ++ gfxParse o
	++ { symbol GfxContents => apply(val, v -> GfxPath { flag:=true; GfxPathList => flatten apply(v, w -> if w === null then (flag=true; {}) else first({ if flag then "M" else "L", w },flag=false))})}
	) else (
    	if (o.?GfxPoints) then n = o.GfxPoints else n = 10;
	val = table(n+1,n+1,(i,j)->(
		x := i*(r#0#1-r#0#0)/n+r#0#0;
		y := j*(r#1#1-r#1#0)/n+r#1#0;
		f := map(R2,R, matrix { if numgens R === 2 then { x,y } else { x, y, R2_0 } });
		z := if numgens R === 2 then { f P } else sort apply(solveSystem { f P }, p -> first p.Coordinates); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(z, zz -> if abs imaginaryPart zz < 1e-6 then vector { x, y, realPart zz })));
	new GfxList from (new GfxObject) ++ { GfxAxes=>gens R, GfxIs3d=>true } ++ gfxParse o
	++ { symbol GfxContents => flatten flatten table(n,n,(i,j) -> for k from 0 to min(#val#i#j,#val#(i+1)#j,#val#i#(j+1),#val#(i+1)#(j+1))-1 list (
		    if val#i#j#k === null or val#(i+1)#j#k === null or val#i#(j+1)#k === null or val#(i+1)#(j+1)#k === null then continue;
		    GfxPolygon { GfxPoints => { val#i#j#k, val#(i+1)#j#k, val#(i+1)#(j+1)#k, val#i#(j+1)#k } } ) ) } -- technically this is wrong -- the quad isn't flat, we should make triangles
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
    {\em Gfx} is a package to produce SVG 2d and 3d graphics. 
    All usable types are descendents of the type GfxObject, and are self-initializing.
    Coordinates can be entered as vectors in RR^2 for 2d, RR^3 or RR^4 for 3d (RR^4 is projective
    coordinates); alternatively, one can enter them as arrays. The y axis points down, and the z axis points
    away from the viewer.
    All types are option tables, i.e., their arguments are options. There are two types of options:
    Gfx options, that are symbols starting with Gfx (e.g., GfxRadius for circles); and styling options, which are CSS style options,
    and which are {\bf strings} (e.g., "fill" for fill color).
    Gfx does not use units (coordinates are dimensionless).
  Caveat
    Mixing 2d and 3d graphics leads to unpredictable results.
 Node
  Key
   GfxObject
  Headline
   The ancestor class of all Gfx objects
 Node
  Key
   GfxList
  Headline
   A class that represents a list of Gfx objects, displayed together
  Description
   Text
    A list of Gfx objects. see also gfx
 Node
  Key
   GfxCircle
  Headline
   An SVG circle
  Description
   Text
    An SVG circle. The two compulsory options are GfxCenter (coordinates of the center) and GfxRadius (radius).
   Example
    GfxCircle{GfxCenter=>[10,10],GfxRadius=>50,"fill"=>"green","stroke"=>"none"}
    GfxCircle{[10,10],10} -- equivalent syntax
 Node
  Key
   GfxLight
  Headline
   A source of light for a 3d SVG picture.   
  Description
   Text
    This corresponds to the SVG "specular" lighting, use the property GfxSpecular. The location is given by GfxCenter.
    By default a GfxLight is invisible (it has GfxRadius 0) and is unaffected by matrix transformations outside it (GfxStatic true).
   Example
    GfxLight{GfxRadius=>10,"fill"=>"yellow"}
 Node
  Key
   GfxEllipse
  Headline
   An SVG ellipse
  Description
   Text
    An SVG ellipse. The three compulsory options are GfxCenter (coordinates of the center) and GfxRadiusX, GfxRadiusY (radii).
   Example
    GfxEllipse{GfxCenter=>[10,10],GfxRadiusX=>50,GfxRadiusY=>20,"stroke"=>"none"}
    GfxEllipse{[10,10],50,20,"stroke"=>"none"} -- equivalent syntax
 Node
  Key
   GfxPath
  Headline
   An SVG path
  Description
   Text
    An SVG path. It follows the syntax of SVG paths, except successive commands must be grouped together in a list called GfxPathList.
   Example
    GfxPath{GfxPathList => {"M", [0, 25], "Q", [25, 25], [25, 0], "M", [50, 25], "Q", [25, 25], [25, 50]},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
 Node
  Key
   GfxPolygon
  Headline
   An SVG polygon
  Description
   Text
    An SVG polygon. The coordinates must form a list called GfxPoints. (the difference with GfxPolyline is that the last coordinate is reconnected to the first)
   Example
    GfxPolygon{GfxPoints=>{[0,10],[100,10],[90,90],[0,80]},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   GfxPolyline
  Headline
   An SVG sequence of lines
  Description
   Text
    An SVG sequence of lines. The coordinates must form a list called GfxPoints. (the difference with GfxPolygon is that the last coordinate is not reconnected to the first)
   Example
    GfxPolyline{GfxPoints=>{[0,10],[100,10],[90,90],[0,80]},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   GfxText
  Headline
   Some SVG text
  Description
   Text
    SVG text. The text itself is the option GfxString (a string). Text can be "stroke"d or "fill"ed.
    Font size should be specified with GfxFontSize.
   Example
    GfxText{[0,0],"Test","stroke"=>"red","fill"=>"none","stroke-width"=>0.5}
 Node
  Key
   gfx
  Headline
    Groups together multiple Gfx objects
  Description
   Text
    gfx(a,b,...,c, options) results in a new GfxList object containing a,b,...,c 
    and the given options.
 Node
  Key
   gfxRange   
  Headline
    gives the range of view port occupied by a Gfx object
  Description
   Text
    gfxRange gives the range of view port occupied by a Gfx object, either as computed by the package or as given by the option GfxRange
  Caveat
    At the moment gfxRange does not take into account the width of "stroke"s.
 Node
  Key
   gfxIs3d
  Headline
   returns a boolean according to whether the Gfx object is 2d (false) or 3d (true).
 Node
  Key
   gfxDistance
  Headline
   returns the distance to the viewer of a Gfx 3d object.
 Node
  Key
   gfxRotation   
  Headline
   Produces a 3d rotation encoded as a 4x4 matrix that can be used as an argument to GfxMatrix or GfxAutoMatrix.
  Usage
   gfxRotation ( angle, axis, center)
 Node
  Key
   gfxTranslation
  Headline
   Produces a 3d translation encoded as a 4x4 matrix that can be used as an argument to GfxMatrix or GfxAutoMatrix.
  Usage
   gfxTranslation ( vector )
 Node
  Key
   GfxOneSided
  Headline
   a property of GfxPolyPrimitive 3d objects, means that polygons must be drawn only if they are facing the correct way.
 Node
  Key
   GfxVertical
  Headline
   A possible element of the list GfxGadgets of a Gfx 3d object; specifies that the vertical slider must be drawn.
 Node
  Key
   GfxRange
  Headline
   An option to fix manually the view port range of a Gfx object.
  Description
   Text
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxWidth
  Headline
   An option to fix the width of the Gfx object in line width units. 
  Description
   Text
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxPerspective
  Headline
   An option to fix the amount of perspective
  Description
   Text
    A 4x4 matrix that is applied to 3d coordinates for perspective.
    After this tranformation, the coordinates must be (x,y,z,z/p) in the reference frame
    where the viewer is at (0,0,0) and the screen at z=p.
    One can instead provide a real number p, which is equivalent to placing the screen 
    centered at z=0 and the viewer at (0,0,-p).
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxHorizontal
  Headline
   Used to draw the horiontal slider of a Gfx 3d object
  Description
   Text
    A possible element of the list GfxGadgets of a Gfx 3d object;
    specifies that the horizontal slider must be drawn.
 Node
  Key
   GfxHeight
  Headline
   An option to fix the height of the Gfx object in line width units. 
  Description
   Text
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxAutoMatrix
  Headline
   An option to create a rotation animation for the Gfx 3d object.
  Description
   Text
    The value can be a single 4x4 matrix, or a list which is cycled.
    In order for the animation to work, Gfx.css and Gfx.js must be included in the web page.   
 Node
  Key
   GfxMatrix
  Headline
   An option to rotate the coordinates of the Gfx 3d object.
  Description
   Text
    Must be a 4x4 matrix (projective coordinates).
 Node
  Key
   GfxGadgets
  Headline
   An option for a Gfx 3d object
  Description
   Text
    Determines which of the following elements are drawn:
    vertical and horizontal sliders, and 3d animation button.
    It's a list with possible elemnts GfxVertical, GfxHorizontal, GfxAuto.
    By default all three are drawn.
    Only has an effect if in the outermost Gfx object.
 Node
  Key
   GfxBlur
  Headline
   An option to blur a Gfx object
  Description
   Text
    This corresponds to the feGaussianBlur SVG filter. 
    The value is the amount of blurriness relative to the size of the object.
 Node
  Key
   GfxStatic
  Headline
   An option for a Gfx 3d object; it is unaffected by matrix tranformations of its ancestors
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
    GfxEllipse{[60,60],40,30, "fill"=>gfxLinearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
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
    GfxEllipse{[60,60],40,30, "fill"=>gfxRadialGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   gfxPlot
  Headline
   Draws a curve or surface defined implicitly or explicitly by a polynomial
  Description
   Text
    The first argument is a polynomial, the second is a (list of) range(s) of variable(s).
    If the number of ranges is equal to the number of variables of the polynomial, the graph of the polynomial
    is drawn. If it is one fewer, then the zero set of the polynomial is drawn.
   Example
    R=RR[x,y];
    P=y^2-(x+1)*(x-1)*(x-2);
    gfxPlot(P,{-2,3},"stroke-width"=>0.05,GfxHeight=>25,"stroke"=>"red")
 Node
  Key
   GfxAxes
  Headline
   An option to draw axes
///

end--

-- ex of use
gr=gfxLinearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")};
gfx(GfxEllipse{[0,0],90,30,"stroke"=>"none","fill"=>gr,GfxBlur=>0.3},GfxText{[-65,-7],"Macaulay2",GfxFontSize=>25,"stroke"=>"black","fill"=>"white"},GfxHeight=>12)

a=GfxCircle{"fill"=>"yellow","stroke"=>"green",GfxWidth=>1,GfxHeight=>1}
b=GfxLine{[10,10],[20,50],"stroke"=>"black"}
c=GfxCircle{[50,50],50,"fill"=>"blue","fill-opacity"=>0.25}
d=GfxEllipse{[60,60],40,30, "fill"=>"blue", "stroke"=>"grey"}
e=GfxPolyline{{[0,0],[100,100],[100,50]},"fill"=>"pink","stroke"=>"green"}
f=GfxPolygon{{[0,10],[100,10],[90,90],[0,80]},"stroke"=>"red","fill"=>"white"}
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
b1=GfxPath{{"M", [0, 25], "Q", [25, 25], [25, 0], "M", [50, 25], "Q", [25, 25], [25, 50]},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
b2=GfxPath{{"M", [0, 25], "Q", [25, 25], [25, 0], "M", [50, 25], "Q", [25, 25], [25, 50]},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
b=gfx(z,b1,b2,GfxWidth=>2,GfxHeight=>2,GfxMargin=>0)
a1=GfxPath{{"M", [50, 25], "Q", [25, 25], [25, 0], "M", [0, 25], "Q", [25, 25], [25, 50]},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
a2=GfxPath{{"M", [50, 25], "Q", [25, 25], [25, 0], "M", [0, 25], "Q", [25, 25], [25, 50]},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
a=gfx(z,a1,a2,GfxWidth=>2,GfxHeight=>2,GfxMargin=>0)
ab=a|b
ba=b|a
ab||ba||ba
tile = (I,i,j)->(if m_(i+1,j+1)%I == 0 then if c_(i+1,j+1)%I==0 then () else a else b);
tiledRow = (I,i)->new RowExpression from apply(n,j->tile(I,i,j));
loopConfig = I->new ColumnExpression from apply(k,i->tiledRow(I,i));


-- or
barside1=GfxPath{{"M",[80,60,100],"L",[80,55,100],"L",[220,55,100],"L",[220,60,100],"Z"},"fill"=>"#222","stroke-width"=>0}; -- stroke-width shouldn't be necessary
triangle1=GfxPath{{"M",[-50,160,2],"L",[0,80,2],"L",[50,160,2],"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
triangle2=GfxPath{{"M",[30,160,98],"L",[80,80,98],"L",[130,160,98],"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
edge1=GfxPath{{"M",[30,160,98],"L",[30,160,102],"L",[80,80,102],"L",[80,80,98],"Z"},"fill"=>"#4080e0","stroke-width"=>1};
edge2=GfxPath{{"M",[130,160,98],"L",[130,160,102],"L",[80,80,102],"L",[80,80,98],"Z"},"fill"=>"#4080e0","stroke-width"=>1};
bartop=GfxPath{{"M",[80,55,98],"L",[80,55,102],"L",[220,55,102],"L",[220,55,98],"Z"},"fill"=>"#aaa","stroke-width"=>0}; -- stroke-width shouldn't be necessary
thread=GfxPath{{"M",[80,55,100],"L",[80,80,100],"Z"},"stroke"=>"#111","stroke-width"=>0.5,"stroke-opacity"=>0.8};
gfx{barside1,triangle1,triangle2,edge1,edge2,bartop,thread}

-- tetrahedron
v={[74.5571, 52.0137, -41.6631],[27.2634, -29.9211, 91.4409],[-81.3041, 57.8325, 6.71156],[-20.5165, -79.9251, -56.4894]};
c={"red","green","blue","yellow"};
vv={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
triangles=apply(4,i->GfxPath{{"M",vv#i#0,"L",vv#i#1,"L",vv#i#2,"Z"},"fill"=>c#i,GfxOneSided=>true});
gfx(triangles,GfxLight{[100,0,0],GfxRadius=>10},GfxRange=>{[-100,-150],[150,150]},GfxHeight=>30,GfxMatrix=>gfxRotation(-1.5,[0,1,0]))

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
d=gfx(dodecasplit,"fill-opacity"=>0.65,GfxAutoMatrix=>gfxRotation(0.02,[1,2,3]));
d1=gfx(d,GfxMatrix=>gfxTranslation[200,0,0]); -- using alternate syntax of Array instead of Vector
d2=gfx(d,GfxMatrix=>gfxTranslation[-200,0,0]);
gfx(d1,d2,GfxRange=>{vector{-400,-400},vector{400,400}},GfxHeight=>25,"stroke-width"=>2)

p=random splice{0..11};


-- icosahedron
vertices={vector{0.,0.,-95.1057},vector{0.,0.,95.1057},vector{-85.0651,0.,-42.5325},vector{85.0651,0.,42.5325},vector{68.8191,-50.,-42.5325},vector{68.8191,50.,-42.5325},vector{-68.8191,-50.,42.5325},vector{-68.8191,50.,42.5325},vector{-26.2866,-80.9017,-42.5325},vector{-26.2866,80.9017,-42.5325},vector{26.2866,-80.9017,42.5325},vector{26.2866,80.9017,42.5325}};
faces={{1,11,7},{1,7,6},{1,6,10},{1,10,3},{1,3,11},{4,8,0},{5,4,0},{9,5,0},{2,9,0},{8,2,0},{11,9,7},{7,2,6},{6,8,10},{10,4,3},{3,5,11},{4,10,8},{5,3,4},{9,11,5},{2,7,9},{8,6,2}};
icosa=apply(faces,f->GfxPolygon{apply(f,j->vertices#j),"fill"=>"gray"});
i=gfx(icosa,GfxMatrix=>matrix{{0.7,0,0,0},{0,0.7,0,0},{0,0,0.7,0},{0,0,0,1}})

rnd = () -> random(-1.,1.); cols={"red","green","blue","yellow","magenta","cyan"};
gfx(i, apply(cols, c -> GfxLight{100*vector{1.5+rnd(),rnd(),rnd()},GfxRadius=>10,"fill"=>c,GfxSpecular=>10,GfxAutoMatrix=>gfxRotation(0.02,[rnd(),rnd(),rnd()])}),GfxRange=>{[-200,-200],[200,200]},GfxHeight=>30)

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
sph=apply(f3,f->GfxPolygon{apply(f,j->v3#j),"stroke"=>"white","fill"=>"gray"});
gfx(sph, apply(cols, c -> GfxLight{100*vector{1.5+rnd(),rnd(),rnd()},GfxRadius=>10,"fill"=>c,GfxSpecular=>10,GfxAutoMatrix=>gfxRotation(0.02,[rnd(),rnd(),rnd()])}),GfxRange=>{[-200,-200],[200,200]},GfxHeight=>30)

-- simple plot
R=RR[x,y]; P=0.1*(x^2-y^2);
gfx(gfxPlot(P,{{-10,10},{-10,10}},GfxPoints=>15,"stroke-width"=>0.05,"fill"=>"gray"),GfxLight{[200,0,-500],GfxSpecular=>10,"fill"=>"rgb(180,0,100)"},GfxLight{[-200,100,-500],GfxSpecular=>10,"fill"=>"rgb(0,180,100)"},GfxHeight=>40,GfxAxes=>false)

-- implicit plot
R=RR[x,y];
P=y^2-(x+1)*(x-1)*(x-2);
gfxPlot(P,{-2,3},"stroke-width"=>0.05,GfxHeight=>25,"stroke"=>"red")

-- to rerun examples/doc: (possibly adding topLevelMode=MathJax to init.m2)
installPackage("Gfx", RemakeAllDocumentation => true, IgnoreExampleErrors => false, RerunExamples => true, CheckDocumentation => true, AbsoluteLinks => false, UserMode => true, InstallPrefix => "/home/pzinn/M2/M2/BUILD/fedora/usr-dist/", SeparateExec => true, DebuggingMode => true)

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
    GfxRectangle{[10,10],[20,50],"fill"=>"pink","stroke"=>"black"} -- first argument is GfxPoint, second GfxSize
  Caveat
   GfxRectangle can only be used in 2d. Use GfxPolygon for 3d.
