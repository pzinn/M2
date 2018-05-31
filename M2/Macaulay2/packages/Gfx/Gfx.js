// ranges
function gfxVRange(el,svgid) {
    var svgel=document.getElementById(svgid);
    if (!svgel) return;
    if (typeof el.oldvalue == "undefined") el.oldvalue=0;
    var cs=Math.cos((el.value-el.oldvalue)*0.031416);
    var sn=Math.sin((el.value-el.oldvalue)*0.031416);
    var mat = new Matrix([[1,0,0,0],[0,cs,sn,0],[0,-sn,cs,0],[0,0,0,1]]);
    gfxRotate(svgel,mat);
    gfxRecompute(svgel);
    el.oldvalue=el.value;
}

function gfxHRange(el,svgid) {
    var svgel=document.getElementById(svgid);
    if (!svgel) return;
    if (typeof el.oldvalue == "undefined") el.oldvalue=0;
    var cs=Math.cos((el.value-el.oldvalue)*0.031416);
    var sn=-Math.sin((el.value-el.oldvalue)*0.031416); // because extra reflection... note weakness: depends on choice of pmatrix
    var mat=new Matrix([[cs,0,-sn,0],[0,1,0,0],[sn,0,cs,0],[0,0,0,1]]);
    gfxRotate(svgel,mat);
    gfxRecompute(svgel);
    el.oldvalue=el.value;
}

// auto-rotation button
function gfxToggleRotation(el,svgid) {
    el.blur();
    var svgel=document.getElementById(svgid);
    if (!svgel) return;
    if (!svgel.pmatrix) {
	svgel.pmatrix = eval(svgel.dataset.pmatrix); // parse once and for all
	// should *always* exist. the perspective matrix
    }
    if (!svgel.cmatrix) svgel.cmatrix = new Matrix(svgel.pmatrix);
    if (el.hasAttribute("data-toggle")) {
	clearInterval(el.intervalId);
	el.removeAttribute("data-toggle");
    }
    else
    {
	el.setAttribute("data-toggle","true");
	el.intervalId=setInterval(function() {
	    if (!document.getElementById(svgel.id)) {
		clearInterval(el.intervalId);
		el.removeAttribute("data-toggle");
	    } else {
		gfxAutoRotate(svgel);
		gfxRecompute(svgel);
	    }
	},50);
    }
}

function gfxAnim(el,svgel) {
    // check if we're still in the document
}

function gfxAutoRotate(el) {
    if ((!el.dmatrix)&&(el.dataset.dmatrix))
    {
	// parse once and for all
	el.dmatrix = eval(el.dataset.dmatrix);
    }
    if (el.dmatrix instanceof Matrix) gfxRotate(el,el.dmatrix);
    else if ((el.dmatrix instanceof Array)&&(el.dmatrix.length>0)) {
	if (!el.dmatrixindex) el.dmatrixindex=0;
	gfxRotate(el,el.dmatrix[el.dmatrixindex]);
	el.dmatrixindex++; if (el.dmatrixindex==el.dmatrix.length) el.dmatrixindex=0;
    }
    for (var i=0; i<el.children.length; i++) gfxAutoRotate(el.children[i]);
}

function gfxRotate(el,mat) {
    if ((!el.matrix)&&(el.dataset.matrix))
	el.matrix = eval(el.dataset.matrix); // parse once and for all
    if (!el.matrix) el.matrix = new Matrix(mat); else el.matrix.leftmultiply(mat);
}

function gfxRecompute(el) {
    // find rotation matrix
    if ((!el.pmatrix)&&(el.dataset.pmatrix))
	el.pmatrix = eval(el.dataset.pmatrix); // parse once and for all
    var mat;
    if (el.pmatrix) mat = el.pmatrix; else { // if not unmoving, get the ancestors' cmatrix
	var el1=el.parentElement;
	if (!el1.cmatrix) return; // shouldn't happen
	mat = el1.cmatrix;
    }
    // cmatrix is the compound rotation matrix (just an optimization to avoid repeated multiplications)
    // at the end of the day "cmatrix" is the *ordered* product over ancestors of matrices "matrix" (plus the leftmost perspective matrix "pmatrix"
    if ((!el.matrix)&&(el.dataset.matrix))
	el.matrix = eval(el.dataset.matrix); // parse once and for all
    //    if (!el.matrix) el.cmatrix = new Matrix(mat); else { el.cmatrix = new Matrix(el.matrix); el.cmatrix.leftmultiply(mat); }
        if (!el.matrix) el.cmatrix = mat; else { el.cmatrix = new Matrix(el.matrix); el.cmatrix.leftmultiply(mat); }

    if ((el.tagName=="polyline")||(el.tagName=="polygon")||(el.tagName=="path")) {
	var pth,s,coords,distance;
	if (!el.coords)
	    el.coords=eval(el.dataset.coords);
	// parse path
	s = ""; coords=[]; distance=0;
    	for (var j=0; j<el.coords.length; j++) {
	    if (el.coords[j] instanceof Float32Array) {
		var u=el.cmatrix.vectmultiply(el.coords[j]);
		var v=[u[0]/u[3],u[1]/u[3]];
		coords.push(u);
		s+=v[0]+" "+v[1]+" ";
		distance+=u[0]*u[0]+u[1]*u[1]+u[2]*u[2];
	    }
	    else s+=el.coords[j]+" ";
	}
	// rewrite "d" or "points"
	if (el.tagName=="path") el.setAttribute("d",s); else el.setAttribute("points",s);
	// recompute square distance as average of square distances of vertices
	el.distance=distance/coords.length;
	if (coords.length>2) {	
	    var det = coords[0][2]*coords[1][1]*coords[2][0]-coords[0][1]*coords[1][2]*coords[2][0]-coords[0][2]*coords[1][0]*coords[2][1]+coords[0][0]*coords[1][2]*coords[2][1]+coords[0][1]*coords[1][0]*coords[2][2]-coords[0][0]*coords[1][1]*coords[2][2]; // TODO optimize
	    // visibility
	    if (el.dataset.onesided) {
		if (det<0) { el.style.visibility="hidden"; return; } else el.style.visibility="visible";
	    }
	    // lighting
	    var lightname = el.getAttribute("filter");
	    if (lightname) {
		lightname=lightname.substring(5,lightname.length-1); // eww. what is correct way??
		var lightel=document.getElementById(lightname);
		var u=[coords[1][0]-coords[0][0],coords[1][1]-coords[0][1],coords[1][2]-coords[0][2]];
		var v=[coords[2][0]-coords[0][0],coords[2][1]-coords[0][1],coords[2][2]-coords[0][2]];
		var w=[u[1]*v[2]-v[1]*u[2],u[2]*v[0]-v[2]*u[0],u[0]*v[1]-v[0]*u[1]];
		var w2=w[0]*w[0]+w[1]*w[1]+w[2]*w[2];
		for (var j=0; j<lightel.children.length; j++)
		    if (lightel.children[j].tagName == "feSpecularLighting") {
			var lightel2=lightel.children[j].firstElementChild; // eww
			// move the center of the light to its mirror image in the plane of the polygon
			var origin=document.getElementById(lightel2.dataset.origin);
			if (!origin.pcenter) gfxRecompute(origin); // hopefully won't create infinite loops
			var light = new Float32Array(origin.pcenter); // phew
			var sp = w[0]*(light[0]-coords[0][0])+w[1]*(light[1]-coords[0][1])+w[2]*(light[2]-coords[0][2]);
			var c = 2*sp/w2;
			var p = light[2]/light[3]; // eww
			for (var i=0; i<3; i++) light[i]-=c*w[i];
			if (det<0) sp=-sp;
			if (sp<0) lightel.children[j].setAttribute("lighting-color","#000000"); else {
			    lightel.children[j].setAttribute("lighting-color",origin.style.fill);
			    lightel2.setAttribute("x",light[0]*p/light[2]);
			    lightel2.setAttribute("y",light[1]*p/light[2]);
			    lightel2.setAttribute("z",sp/Math.sqrt(w2));
			}
		    }
	    }
	}
    }
    else if (el.tagName=="line") {
	if (!el.point1)
	    el.point1=eval(el.dataset.point1);
	if (!el.point2)
	    el.point2=eval(el.dataset.point2);
	var u1=el.cmatrix.vectmultiply(el.point1);
	var v1=[u1[0]/u1[3],u1[1]/u1[3]];
	var u2=el.cmatrix.vectmultiply(el.point2);
	var v2=[u2[0]/u2[3],u2[1]/u2[3]];
	el.distance=0.5*(u1[0]*u1[0]+u1[1]*u1[1]+u1[2]*u1[2]+u2[0]*u2[0]+u2[1]*u2[1]+u2[2]*u2[2]);
	el.setAttribute("x1",v1[0]);
	el.setAttribute("y1",v1[1]);
	el.setAttribute("x2",v2[0]);
	el.setAttribute("y2",v2[1]);
    }
    else if (el.tagName=="text") {
	if (!el.point)
	    el.point=eval(el.dataset.point);
	if (!el.fontsize)
	    if (el.dataset.fontsize) el.fontsize=eval(el.dataset.fontsize); else el.fontsize=14;
	var u=el.cmatrix.vectmultiply(el.point);
	var v=[u[0]/u[3],u[1]/u[3]];
	el.distance=u[0]*u[0]+u[1]*u[1]+u[2]*u[2];
	el.setAttribute("x",v[0]);
	el.setAttribute("y",v[1]);
	// rescale font size
	el.style.fontSize = el.fontsize/u[3]+"px"; // chrome doesn't mind absence of units but firefox does
    }
    else if ((el.tagName=="circle")||(el.tagName=="ellipse")) {
	if (!el.center)
	    el.center=eval(el.dataset.center);
	var u=el.cmatrix.vectmultiply(el.center);
	el.pcenter = u; // in case someone needs it ... (light)
	var v=[u[0]/u[3],u[1]/u[3]];
	el.distance=u[0]*u[0]+u[1]*u[1]+u[2]*u[2];
	el.setAttribute("cx",v[0]);
	el.setAttribute("cy",v[1]);
	// also, rescale radius
	if (el.tagName=="circle")
	    el.setAttribute("r",eval(el.dataset.r)/u[3]);
	else {
	    el.setAttribute("rx",eval(el.dataset.rx)/u[3]);
	    el.setAttribute("ry",eval(el.dataset.ry)/u[3]);
	}	    
    }
    else if ((el.tagName=="svg")||(el.tagName=="g")) {
	// must call inductively children's
	for (var i=0; i<el.children.length; i++) gfxRecompute(el.children[i]);
	gfxReorder(el);
	// recompute square distance as average of square distances of children
	el.distance=0; var cnt = 0;
	for (var i=0; i<el.children.length; i++)
	    if (el.children[i].distance != 0) {
		el.distance+=el.children[i].distance;
		cnt++;
	    }
	if (cnt>0) el.distance/=cnt;
    }
    else el.distance=0; // bit of a hack -- for 2d objects but also filters...
}

function gfxReorder(el) {
    if ((el.tagName=="svg")||(el.tagName=="g")) {
	// order children according to distance
	for (i=1; i<el.children.length; i++) {
	    var child = el.children[i];
	    j=i; while ((j>0)&&(child.distance>el.children[j-1].distance)) j--;
	    if (j<i) {
		el.removeChild(child);
		el.insertBefore(child,el.children[j]);
	    }
	}
    }
}


// a simple square matrix type
var msize=4;
var matrix_identity=new Array(msize); for (var i=0; i<msize; i++) { matrix_identity[i]=new Array(msize); for (var j=0; j<msize; j++) if (i==j) matrix_identity[i][j]=1.; else matrix_identity[i][j]=0.; }

function doubleArrayToFloat32Array(mat) // used internally
{
    var val=new Float32Array(msize*msize);
    var i,j;
    for (i=0; i<msize; i++)
	for (j=0; j<msize; j++)
	    val[i+msize*j]=mat[i][j]; // note the transposition to conform to openGL's silly convention
    return val;
}

function Matrix(mat) // simple constructor
{
    if (typeof(mat)=='number') // a number means multiple of identity
    {	
	this.elem=doubleArrayToFloat32Array(matrix_identity);
	this.leftmultiply(mat);
    }
    else if (typeof(mat) == 'object')
    {
	if (mat instanceof Matrix)
	{
	    this.elem=new Float32Array(mat.elem);
	}
	if (mat instanceof Float32Array)
	{
	    this.elem=new Float32Array(mat);
	}
	else if (mat instanceof Array)
	{
	    this.elem=doubleArrayToFloat32Array(mat);
	}
    }
    else
	this.elem=new Float32Array(msize*msize);
} 
Matrix.prototype = 
{
    // Returns element (i,j) of the matrix
    e: function(i,j) { return this.elem[i+msize*j]; },

    zero: function() 
    {
	this.elem.fill(0);
    },

    // display
    print: function() 
    { 
	a="{"; 
	for (var i=0; i<msize; i++) 
	    {
		a+="{";
		for (var j=0; j<msize; j++)
		    {
			a+=this.e(i,j);
			if (j<msize-1) a+=",";
		    }
		a+="}";
		if (i<msize-1) a+=",";
	    }
	a+="}";
	return a;
    },

    // add another matrix or a scalar (multiple of identity)
    add: function(mat)
    {
	if (typeof(mat)=='number')
	    {
		for (var i=0; i<msize; i++)
		    this.elem[i*(msize+1)]+=mat;
	    }
	else if (typeof(mat)=='object')
	    {
		for (var i=0; i<msize*msize; i++)
		    this.elem[i]+=mat.elem[i];
	    }
    },

    // left multiply by a matrix or scalar
    leftmultiply: function(mat) 
    {
	if (typeof(mat)=='number')
	    {
		for (var i=0; i<msize*msize; i++)
		    this.elem[i]*=mat;
	    }
	else if (typeof(mat)=='object')
	    {
		var temp=new Float32Array(msize*msize);
		for (var i=0; i<msize; i++)
		    for (var j=0; j<msize; j++)
			for (var k=0; k<msize; k++)
			    temp[i+msize*k]+=mat.elem[i+msize*j]*this.elem[j+msize*k];
		this.elem=temp;
	    }
    },

    vectmultiply: function(vec)
    {
	// rotate
	var u=new Float32Array(msize);
	for (var k=0; k<msize; k++)
	{
	    u[k]=0.;
	    for (var l=0; l<msize; l++)
		u[k]+=this.elem[k+msize*l]*vec[l];
	}
	return u;
    },
    
    transpose: function()
    {
	var temp;
	for (i=0; i<msize-1; i++)
	    for (j=i+1; j<msize; j++)
		{
		    temp=this.elem[i+msize*j];
		    this.elem[i+msize*j]=this.elem[j+msize*i];
		    this.elem[j+msize*i]=temp;
		}
    },


    // orthogonalize
    orthogonalize: function()
    {
	var q=new Matrix();
	var qq;
	var eps=1.; var t=0;
	while ((eps>matrix_accuracy)&&(t<20)) // safeguard here: can't iterate more than ... times
	    {
		eps=0.;
		for (var i=0; i<msize; i++)
		    for (var j=0; j<msize; j++)
			{
			    qq=0;
			    for (var k=0; k<msize; k++) qq+=this.elem[i+msize*k]*this.elem[j+msize*k];
			    if (i==j) { eps+=Math.abs(qq-1); q.elem[i+msize*j]=1.5-0.5*qq; }
			    else { eps+=Math.abs(qq); q.elem[i+msize*j]=-0.5*qq; }
			}
		this.leftmultiply(q);
		t++;
	    } 
    },

    // random antisymmetric matrix
    randomanti: function(amp)
    {
	for (var i=0; i<msize; i++)
	    for (var j=0; j<msize; j++)
		if (i<j)
		    this.elem[i+msize*j]=amp*(Math.random()-0.5);
		else if (i>j)
		    this.elem[i+msize*j]=-this.elem[j+msize*i];
		else
		    this.elem[i*(msize+1)]=0;
    },

    //generates a random orthogonal
    //matrix in the neighborhood of the origin
    randomorthog: function(amp)
    {
	this.randomanti(amp);
	this.add(1);
	this.orthogonalize();
    },

};

