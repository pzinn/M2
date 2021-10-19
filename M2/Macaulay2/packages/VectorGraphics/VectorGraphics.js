function gfxInitMouse(el) {
    el.onmousedown = gfxMouseDown1;
    el.onmouseleave = gfxMouseUp;
    el.onmouseup = gfxMouseUp;
    el.onclick = gfxMouseClick;
    el.oncontextmenu = gfxMouseClick;
}

function gfxInitData(el) {
    gfxCheckData(el);
    gfxRedo(el);
}

// auto-rotation button
window.gfxToggleRotation = function(event) {
    //    this.blur();
    //    var svgel=document.getElementById(svgid);
    //    if (!svgel) return;
    var el = event.currentTarget;
    var svgel = el.ownerSVGElement; // var svgel = el.parentElement; // weak but works
    if (!svgel.gfxdata) gfxInitData(svgel);
    if (!el.ondblclick) el.ondblclick= function(event) { event.stopPropagation(); }; // weak
    
    // the perspective matrix should *always* exist
    if (!svgel.gfxdata.cmatrix) svgel.gfxdata.cmatrix = new Matrix(svgel.gfxdata.pmatrix);
    if (el.classList.contains("active")) {
	clearInterval(el.intervalId);
	el.classList.remove("active");
    }
    else
    {
	el.classList.add("active");
	el.intervalId=setInterval(() => {
	    if ((!svgel)||(!document.body.contains(svgel))) {
		svgel=null; // for garbage collecting
		el.classList.remove("active");
		clearInterval(el.intervalId);
	    } else {
		gfxAutoRotate(svgel);
		gfxRedo(svgel);
	    }
	},50);
    }
    event.stopPropagation();
}

var mouseDown=false;

var dragTarget=null; // what's being dragged

// mouse handling
window.gfxMouseDown = function (event) {
    var el = event.currentTarget;
    if (!el.gfxdata) gfxInitData(el);
    gfxInitMouse(el);
    gfxMouseDown1.call(el,event);
}

function gfxMouseDown1(event) {
    var el=event.target; // determine if we're dragging
    while (el && el.tagName!="svg" && !el.classList.contains("M2SvgDraggable")) el=el.parentElement;
    //    dragTarget = !el || !el.classList.contains("M2SvgDraggable") ? null : event.target;
    dragTarget = el && el.classList.contains("M2SvgDraggable") ? el : null;
    this.onmousemove = gfxMouseMove;
    this.onwheel = gfxMouseWheel;
    event.preventDefault();
    event.stopPropagation();
}


function gfxMouseUp(event) {
    this.onmousemove = null;
    this.onwheel = null;
    dragTarget=null;
    event.preventDefault();
    event.stopPropagation();
}


function gfxTranslateDrag(x,y,z) { // TODO 3d vs 2d?
    var r=dragTarget.gfxdata.ctr; // should already exist
    var mat = dragTarget.gfxdata.cmatrix; // should already exist
    var u = mat.vectmultiply(r);
    var mati = mat.inverse();
    var t = mati.vectmultiply(new Vector([x,-y,-z,0]));
    var gam = -t[3]; // often but not always zero
    var cf = u[3]/(gam*u[3]+r[3]);
    for (i=0; i<3; i++) t[i]=cf*(gam*r[i]/r[3]+t[i]);
    var mat = new Matrix([[1,0,0,t[0]],[0,1,0,t[1]],[0,0,1,t[2]],[0,0,0,1]]);
    // TODO merge with gfxRotateDrag?
    if (dragTarget.gfxdata.matrix) mat.leftmultiply(dragTarget.gfxdata.matrix);
    dragTarget.gfxdata.matrix=mat;
}

function gfxRotateDrag(rot0) {
    // TODO optimize?
    var rot=new Matrix(dragTarget.gfxdata.cmatrix);
    rot[3]=rot[7]=rot[11]=rot[12]=rot[13]=rot[14]=0; rot[15]=1; // sub 3x3
    var rot1 = rot.inverse();
    rot.leftmultiply(rot0);
    rot.leftmultiply(rot1);
    var ctr = dragTarget.gfxdata.ctr;
    var transl = new Matrix([[1,0,0,ctr[0]/ctr[3]],[0,1,0,ctr[1]/ctr[3]],[0,0,1,ctr[2]/ctr[3]],[0,0,0,1]]);
    var mat = new Matrix([[1,0,0,-ctr[0]/ctr[3]],[0,1,0,-ctr[1]/ctr[3]],[0,0,1,-ctr[2]/ctr[3]],[0,0,0,1]]);
    mat.leftmultiply(rot);
    mat.leftmultiply(transl);
//    if (!dragTarget.gfxdata.matrix) dragTarget.gfxdata.matrix = new Matrix(mat); else dragTarget.gfxdata.matrix.leftmultiply(mat);
    if (dragTarget.gfxdata.matrix) mat.leftmultiply(dragTarget.gfxdata.matrix);
    dragTarget.gfxdata.matrix=mat;
}

function gfxMouseMove(event) {
    if (!dragTarget) return;
    if (event.buttons & 1 && !event.shiftKey) {
	/*
	var x=event.offsetX*this.viewBox.baseVal.width/this.width.baseVal.value+this.viewBox.baseVal.x;
	var y=event.offsetY*this.viewBox.baseVal.height/this.height.baseVal.value+this.viewBox.baseVal.y;
	*/
	x=event.movementX*this.viewBox.baseVal.width/this.width.baseVal.value;
	y=event.movementY*this.viewBox.baseVal.height/this.height.baseVal.value;
	gfxTranslateDrag(x,y,0);
	gfxRedo(this);
    } else if ((event.buttons & 2 && !event.shiftKey)||(event.buttons & 1 && event.shiftKey)) {
	var x=event.movementX/this.width.baseVal.value;
	var y=event.movementY/this.height.baseVal.value;

	var d=x*x+y*y;
	x*=1+d/3; y*=1+d/3; // nonlinear correction

	var mat=new Matrix([[1-x*x+y*y,2*x*y,2*x,0],[2*x*y,1+x*x-y*y,-2*y,0],[-2*x,2*y,1-x*x-y*y,0],[0,0,0,1+x*x+y*y]]);
	mat.leftmultiply(1/(1+x*x+y*y));
	gfxRotateDrag(mat);
	gfxRedo(this);
    }
    event.preventDefault();
    event.stopPropagation();
}

function gfxMouseWheel(event) {
    if (!dragTarget) return;
    if (event.buttons & 1 && !event.shiftKey) {
	/*
	var x=event.offsetX*this.viewBox.baseVal.width/this.width.baseVal.value+this.viewBox.baseVal.x;
	var y=event.offsetY*this.viewBox.baseVal.height/this.height.baseVal.value+this.viewBox.baseVal.y;
	*/
	gfxTranslateDrag(0,0,-event.deltaY); // unit???
	gfxRedo(this);
    } else if ((event.buttons & 2 && !event.shiftKey)||(event.buttons & 1 && event.shiftKey)) {
	var z = event.deltaY*0.001; // unit???

	var mat=new Matrix([[(1-z*z)/(1+z*z),2*z/(1+z*z),0,0],[-2*z/(1+z*z),(1-z*z)/(1+z*z),0,0],[0,0,1,0],[0,0,0,1]]);
	gfxRotateDrag(mat);
	gfxRedo(this);
    }
    event.preventDefault();
    event.stopPropagation();
}

function gfxMouseClick(event) {
    if (event.buttons & 2 && event.shiftKey) return; // normal behavior of right-click if shift
    event.preventDefault();
    event.stopPropagation();
}

function gfxAutoRotate(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return;
    gfxAutoRotateInt(el,el.gfxdata.dmatrix);
    for (var i=0; i<el.children.length; i++) gfxAutoRotate(el.children[i]);
}
function gfxAutoRotateInt(el,dmatrix) { // returns true if can move to next in list
    if (!dmatrix) return true;
    else if (dmatrix instanceof Matrix) { gfxRotate(el,dmatrix); return true; } // single
    else if ((dmatrix instanceof Array)&&(dmatrix.length>0)) { // list
	if (!dmatrix.index) dmatrix.index=0;
	if (gfxAutoRotateInt(el,dmatrix[dmatrix.index])) {
	    dmatrix.index++;
	    if (dmatrix.index==dmatrix.length) { dmatrix.index=0; return true; } else return false;
	} else return false;
    } else { // repeated
	if (!dmatrix.index) dmatrix.index=0;
	if (gfxAutoRotateInt(el,dmatrix.content)) {
	    dmatrix.index++;
	    if (dmatrix.index==dmatrix.number) { dmatrix.index=0; return true; } else return false;
	} else return false;
    }
}

function gfxRotate(el,mat) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return;
    if (!el.gfxdata.matrix) el.gfxdata.matrix = new Matrix(mat); else el.gfxdata.matrix.leftmultiply(mat);
}

function isActive(el) { // tricky concept: draggable or autorotated
    while(el.tagName!="svg" && el != dragTarget && !el.gfxdata.dmatrix) // TODO should only be tested if autorotate turned on
	el=el.parentElement;
    return el.tagName!="svg";
}

function gfxRecompute(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return; // gadgets and non svg aren't affected by transformations
    var mat;
    if (el.gfxdata.pmatrix) mat = el.gfxdata.pmatrix; else { // if not unmoving, get the ancestors' cmatrix
	var el1=el.parentElement;
	if (!el1.gfxdata.cmatrix) return; // shouldn't happen
	mat = el1.gfxdata.cmatrix;
    }
    // cmatrix is the compound rotation matrix (just an optimization to avoid repeated multiplications)
    // at the end of the day "cmatrix" is the *ordered* product over ancestors of matrices "matrix" (plus the leftmost perspective matrix "pmatrix"
    if (!el.gfxdata.matrix) el.gfxdata.cmatrix = mat; else { el.gfxdata.cmatrix = new Matrix(el.gfxdata.matrix); el.gfxdata.cmatrix.leftmultiply(mat); }

    if ((el.tagName=="svg")||(el.tagName=="g")) {
	// must call inductively children's
	for (var i=0; i<el.children.length; i++) gfxRecompute(el.children[i]);
	return;
    }
    if (!el.gfxdata.coords || el.gfxdata.coords.length == 0) {
	return;
    }
    el.gfxdata.coords1 = el.gfxdata.coords.map ( (c,i) => {
	var u=el.gfxdata.cmatrix.vectmultiply(c);
	var j = el.gfxdata.names ? el.gfxdata.names[i] : null;
	if (j && isActive(el))
	    el.ownerSVGElement.gfxdata.gcoords[j] = u;
	return u;
    });
}

function gfxRedo(el) {
    el.gfxdata.gcoords={};
    gfxRecompute(el);
    gfxRedraw(el);
}

function gfxRedraw(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return; // gadgets and non svg aren't affected by transformations
    // update passive coords, project
    if (el.gfxdata.coords1 && el.gfxdata.coords1.length>0) {
	var flag=false;
	el.gfxdata.coords2d = el.gfxdata.coords1.map( (u,i) => {
	    var j = el.gfxdata.names ? el.gfxdata.names[i] : null;
	    if (j && el.ownerSVGElement.gfxdata.gcoords[j] && u != el.ownerSVGElement.gfxdata.gcoords[j]) {
		u = el.gfxdata.coords1[i]=el.ownerSVGElement.gfxdata.gcoords[j];
		el.gfxdata.coords[i]=el.gfxdata.cmatrix.inverse().vectmultiply(u);
	    }
	    if (u[3]/el.gfxdata.coords[i][3] <= 0 ) { if (u[3]==0) u[3]=.00001; flag=true; } // to avoid division by zero
	    return [u[0]/u[3],-u[1]/u[3]];
	});
	// compute center
	var ctr=new Vector;
	el.gfxdata.coords.forEach(u => ctr.add(u)); // should we normalize u first?
	el.gfxdata.ctr=ctr.multiply(1/ctr[3]);
	// distance
	var u=el.gfxdata.cmatrix.vectmultiply(ctr);
	el.gfxdata.distance=-u[2]/u[3]; // this is the only z coord we really need
	el.gfxdata.scale = 1/u[3]; // dirty trick for semi-3d objects (circles, ellipses, text); makes certain assumptions on form of cmatrix TODO retire
	// behind screen?
	if (flag) {
	    el.style.display="none";
	    return;
	}
    }
    el.style.display="";
    if ((el.tagName=="polyline")||(el.tagName=="polygon")||(el.tagName=="path")) {
	// parse path
	if (el.tagName=="path") { // annoying
	    var path = el.getAttribute("d").split(" "); // for lack of better
	    var j=0;
	    var s = "";
	    for (var i=0; i<path.length; i++)
		if (path[i] != "") {
		    if ( path[i] >= "A" && path[i] <= "Z" ) s+=path[i]+" "; else
		    {
			s+=el.gfxdata.coords2d[j][0]+" "+el.gfxdata.coords2d[j][1]+" ";
			j++;
			i++;
		    }
		}
	    el.setAttribute("d",s);
	} else {
	    var s = "";
	    for (var j=0; j<el.gfxdata.coords2d.length; j++)
		s+=el.gfxdata.coords2d[j][0]+" "+el.gfxdata.coords2d[j][1]+" ";
	    el.setAttribute("points",s);
	}
	if (el.gfxdata.coords2d.length>2) {
	    var u=[],v=[];
	    for (var i=0; i<2; i++) {
		u.push(el.gfxdata.coords2d[1][i]-el.gfxdata.coords2d[0][i])
		v.push(el.gfxdata.coords2d[2][i]-el.gfxdata.coords2d[0][i]);
	    }
	    var w=v[0]*u[1]-u[0]*v[1];
	    // visibility
	    var flipflag=false;
	    if (w<0) {
		if (el.gfxdata.onesided) {
		    el.style.visibility="hidden";
		    return;
		}
		else flipflag=true;
	    }
	    el.style.visibility="visible";
	    // lighting TODO optimize (right now it recomputes a lot of stuff in particular pmat^{-1} is lame)
	    var lightname = el.getAttribute("filter");
	    if (lightname) {
		lightname=lightname.substring(5,lightname.length-1); // eww. what is correct way??
		var lightel=document.getElementById(lightname);
		var mat = el.ownerSVGElement.gfxdata.pmatrixinv;
		var r=[];
		for (i=0; i<3; i++) {
		    var v=mat.vectmultiply(el.gfxdata.coords1[i]);
		    r.push([v[0]/v[3],v[1]/v[3],v[2]/v[3]]);
		}
		var u=[]; var v=[];
		for (i=0; i<3; i++) {
		    u.push(r[1][i]-r[0][i]);
		    v.push(r[2][i]-r[0][i]);
		}
		var w=[u[1]*v[2]-v[1]*u[2],u[2]*v[0]-v[2]*u[0],u[0]*v[1]-v[0]*u[1]];
		if (flipflag) { w=[-w[0],-w[1],-w[2]]; }
		var w2=w[0]*w[0]+w[1]*w[1]+w[2]*w[2];
		for (var j=0; j<lightel.children.length; j++)
		    if (lightel.children[j].tagName == "feSpecularLighting") {
			var lightel2=lightel.children[j].firstElementChild; // eww
			// move the center of the light to its mirror image in the plane of the polygon
			//var origin=document.getElementById(lightel2.gfxdata.origin);
			var origin=lightel2.gfxdata.origin; // eval acts as getElementById
			var light = mat.vectmultiply(origin.gfxdata.coords1[0]); // phew TODO what if it's a named passive coord?
			light = [light[0]/light[3],light[1]/light[3],light[2]/light[3]];
			var sp = w[0]*(light[0]-r[0][0])+w[1]*(light[1]-r[0][1])+w[2]*(light[2]-r[0][2]);
			var c = 2*sp/w2;
			var lightrefl = light.map( (x,i) => x-c*w[i] );
			lightrefl = el.ownerSVGElement.gfxdata.pmatrix.vectmultiply(new Vector ([lightrefl[0],lightrefl[1],lightrefl[2],1]));
			if (sp<0)
			    lightel.children[j].setAttribute("lighting-color","#000000");
			else {
			    lightel.children[j].setAttribute("lighting-color",origin.style.fill); // TODO should make it proportional to sp/||?
			    lightel2.setAttribute("x",lightrefl[0]/lightrefl[3]);
			    lightel2.setAttribute("y",-lightrefl[1]/lightrefl[3]);
			    lightel2.setAttribute("z",4*origin.gfxdata.r/lightrefl[3]);
			}
		    }
	    }
	}
    }
    else if (el.tagName=="line") {
	    el.setAttribute("x1",el.gfxdata.coords2d[0][0]);
	    el.setAttribute("y1",el.gfxdata.coords2d[0][1]);
	    el.setAttribute("x2",el.gfxdata.coords2d[1][0]);
	    el.setAttribute("y2",el.gfxdata.coords2d[1][1]);
    }
    else if ((el.tagName=="text")||(el.tagName=="foreignObject")) {
	if (!el.gfxdata.fontsize) el.gfxdata.fontsize=14;
	el.setAttribute("x",el.gfxdata.coords2d[0][0]);
	el.setAttribute("y",el.gfxdata.coords2d[0][1]);
	// rescale font size
	el.style.fontSize = el.gfxdata.fontsize*el.gfxdata.scale+"px"; // chrome doesn't mind absence of units but firefox does
    }
    else if ((el.tagName=="circle")||(el.tagName=="ellipse")) {
	    el.setAttribute("cx",el.gfxdata.coords2d[0][0]);
	    el.setAttribute("cy",el.gfxdata.coords2d[0][1]);
	    // also, rescale radius
	    if (el.tagName=="circle")
		el.setAttribute("r", el.gfxdata.r*el.gfxdata.scale);
	    else {
		el.setAttribute("rx", el.gfxdata.rx*el.gfxdata.scale);
		el.setAttribute("ry", el.gfxdata.ry*el.gfxdata.scale);
	    }
    }
    else if ((el.tagName=="svg")||(el.tagName=="g")) {
	// must call inductively children's
	for (var i=0; i<el.children.length; i++) gfxRedraw(el.children[i]);
	gfxReorder(el);
	// recompute ctr as average of ctrs of children
	var ctr=new Vector;
	var cnt = 0;
	for (var i=0; i<el.children.length; i++)
	    if (el.children[i].gfxdata && el.children[i].gfxdata.ctr[3]!=0 && el.children[i].style.opacity!="0") { // the  opacity condition is hacky -- for lights
		if (el.children[i].gfxdata.matrix) ctr.add(el.children[i].gfxdata.matrix.vectmultiply(el.children[i].gfxdata.ctr)); else ctr.add(el.children[i].gfxdata.ctr); // waste of calculation?
		cnt++;
	    }
	if (cnt>0) {
	    el.gfxdata.ctr=ctr.multiply(1/cnt);
	    var u=el.gfxdata.cmatrix.vectmultiply(ctr);
	    el.gfxdata.distance=-u[2]/u[3]; // this is the only z coord we really need
	} else {
	    el.gfxdata.ctr=new Vector([0,0,0,0]);
	    el.gfxdata.distance=0;
	}
    }
    else {
	el.gfxdata.ctr=new Vector([0,0,0,0]); // bit of a hack -- for filters...
	el.gfxdata.distance=0;
    }
}

function gfxReorder(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg") return;
    if ((el.tagName=="svg")||(el.tagName=="g")) {
	// order children according to distance
	for (i=1; i<el.children.length; i++) if (el.children[i].gfxdata) {
	    var child = el.children[i];
	    j=i; while ((j>0)&&(child.gfxdata.distance>el.children[j-1].gfxdata.distance)) j--;
	    if (j<i) el.insertBefore(child,el.children[j]);
	}
    }
}

function gfxCheckData(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg") return;
    if (el.classList.contains("gfxauto")) return;
    // TODO should probably eliminate other things e.g. <title>
    el.gfxdata={};
    for (var v in el.dataset)
	    el.gfxdata[v]=eval("("+el.dataset[v]+")");
    if (el.gfxdata.pmatrix) el.gfxdata.pmatrixinv = el.gfxdata.pmatrix.inverse(); // almost never used
    // start the computation of matrices... (see gfxRecompute() for details)
    var mat = el.gfxdata.pmatrix ? el.gfxdata.pmatrix : el.parentElement.gfxdata.cmatrix;
    if (!el.gfxdata.matrix) el.gfxdata.cmatrix = mat; else { el.gfxdata.cmatrix = new Matrix(el.gfxdata.matrix); el.gfxdata.cmatrix.leftmultiply(mat); }

    if (el.gfxdata.coords) return;
    var mat = el.gfxdata.cmatrix.inverse();

    if ((el.tagName=="polyline")||(el.tagName=="polygon")) {
	var pts = el.points;
	el.gfxdata.coords = [];
	for (var i=0; i<pts.length; i++)
	    el.gfxdata.coords.push(mat.vectmultiply(vector([pts[i].x,-pts[i].y,0,1])));
    }
    else if (el.tagName=="path") { // annoying special case
	var path = el.getAttribute("d").split(" "); // for lack of better
	el.gfxdata.coords = [];
	for (var i=0; i<path.length; i++)
	    if (path[i] != "" && ( path[i] < "A" || path[i] > "Z" )) // ...
	{
	    el.gfxdata.coords.push(mat.vectmultiply(vector([+path[i],-path[i+1],0,1])));
	    i++;
	}
    }
    else if (el.tagName=="line") {
	el.gfxdata.coords=[mat.vectmultiply(vector([el.x1.baseVal.value,-el.y1.baseVal.value,0,1])),
			   mat.vectmultiply(vector([el.x2.baseVal.value,-el.y2.baseVal.value,0,1]))];
    }
    else if (el.tagName=="text") {
	el.gfxdata.coords=[mat.vectmultiply(vector([el.x.baseVal[0].value,-el.y.baseVal[0].value,0,1]))]; // weird
	el.gfxdata.fontsize=el.style.fontSize.substring(0,el.style.fontSize.length-2);
    }
    else if (el.tagName=="foreignObject") {
	el.gfxdata.coords=[mat.vectmultiply(vector([el.x.baseVal.value,-el.y.baseVal.value,0,1]))];
	el.gfxdata.fontsize=el.style.fontSize.substring(0,el.style.fontSize.length-2);
    }
    else if (el.tagName=="circle") {
	el.gfxdata.coords=[mat.vectmultiply(vector([el.cx.baseVal.value,-el.cy.baseVal.value,0,1]))];
	el.gfxdata.r=el.r.baseVal.value;
    }
    else if (el.tagName=="ellipse") {
	el.gfxdata.coords=[mat.vectmultiply(vector([el.cx.baseVal.value,-el.cy.baseVal.value,0,1]))];
	el.gfxdata.rx=el.rx.baseVal.value;
	el.gfxdata.ry=el.ry.baseVal.value;
    }
    for (var i=0; i<el.children.length; i++)
	gfxCheckData(el.children[i]);
}

// a simple square matrix type
var dim=4;
var matrix_identity=new Array(dim); for (var i=0; i<dim; i++) { matrix_identity[i]=new Array(dim); for (var j=0; j<dim; j++) if (i==j) matrix_identity[i][j]=1.; else matrix_identity[i][j]=0.; }

class Vector extends Float32Array {
    constructor(v) {
	if ((v instanceof Array)&&(v.length===dim))
	    super(v);
	else
	    super(dim);
    }
    add(v)
    {
	if (v instanceof Vector)
	{
	    for (var i=0; i<dim; i++)
		this[i]+=v[i];
	}
	return this;
    }
    multiply(x) // scalar
    {
	for (i=0; i<dim; i++)
	    this[i]*=x;
	return this;
    }
}

function doubleArrayToFloat32Array(mat,fl) // used internally
{
    var i,j;
    for (i=0; i<dim; i++)
	for (j=0; j<dim; j++)
	    fl[i+dim*j]=mat[i][j]; // note the transposition to conform to openGL's silly convention
    return fl;
}

class Matrix extends Float32Array {
    constructor(mat) // simple constructor
    {
	if (typeof(mat)=='number') // a number means multiple of identity
	{
	    super(dim*dim);
	    doubleArrayToFloat32Array(matrix_identity,this);
	    this.leftmultiply(mat);
	}
	else if (mat instanceof Array && mat[0] instanceof Array)
	{
	    super(dim*dim);
	    doubleArrayToFloat32Array(mat,this);
	}
	else if (mat instanceof Float32Array || mat instanceof Array)
	{
	    super(mat);
	}
	else super(dim*dim);
    }

    // Returns element (i,j) of the matrix
    e(i,j) { return this[i+dim*j]; }

    zero()
    {
	this.fill(0);
    }

    // display
    print()
    { 
	a="{"; 
	for (var i=0; i<dim; i++)
	    {
		a+="{";
		for (var j=0; j<dim; j++)
		    {
			a+=this.e(i,j);
			if (j<dim-1) a+=",";
		    }
		a+="}";
		if (i<dim-1) a+=",";
	    }
	a+="}";
	return a;
    }

    // add another matrix or a scalar (multiple of identity)
    add(mat)
    {
	if (typeof(mat)=='number')
	    {
		for (var i=0; i<dim; i++)
		    this[i*(dim+1)]+=mat;
	    }
	else if (mat instanceof Matrix)
	    {
		for (var i=0; i<dim*dim; i++)
		    this[i]+=mat[i];
	    }
	return this;
    }

    // left multiply by a matrix or scalar
    leftmultiply(mat)
    {
	if (typeof(mat)=='number')
	    {
		for (var i=0; i<dim*dim; i++)
		    this[i]*=mat;
	    }
	else if (mat instanceof Matrix)
	    {
		var temp=new Matrix(this); // it's assumed mat is *not* this
		this.zero();
		for (var i=0; i<dim; i++)
		    for (var j=0; j<dim; j++)
			for (var k=0; k<dim; k++)
			    this[i+dim*k]+=mat[i+dim*j]*temp[j+dim*k];
	    }
	return this;
    }

    vectmultiply(vec)
    {
	var u=new Vector();
	for (var k=0; k<dim; k++)
	{
	    for (var l=0; l<dim; l++)
		u[k]+=this[k+dim*l]*vec[l];
	}
	return u;
    }

    inverse() // size 4 only!
    {
	var A2323 = this[10] * this[15] - this[11] * this[14],
	    A1323 = this[9] * this[15] - this[11] * this[13],
	    A1223 = this[9] * this[14] - this[10] * this[13],
	    A0323 = this[8] * this[15] - this[11] * this[12],
	    A0223 = this[8] * this[14] - this[10] * this[12],
	    A0123 = this[8] * this[13] - this[9] * this[12],
	    A2313 = this[6] * this[15] - this[7] * this[14],
	    A1313 = this[5] * this[15] - this[7] * this[13],
	    A1213 = this[5] * this[14] - this[6] * this[13],
	    A2312 = this[6] * this[11] - this[7] * this[10],
	    A1312 = this[5] * this[11] - this[7] * this[9],
	    A1212 = this[5] * this[10] - this[6] * this[9],
	    A0313 = this[4] * this[15] - this[7] * this[12],
	    A0213 = this[4] * this[14] - this[6] * this[12],
	    A0312 = this[4] * this[11] - this[7] * this[8],
	    A0212 = this[4] * this[10] - this[6] * this[8],
	    A0113 = this[4] * this[13] - this[5] * this[12],
	    A0112 = this[4] * this[9] - this[5] * this[8];
	var det =
	    this[0] * ( this[5] * A2323 - this[6] * A1323 + this[7] * A1223 )
	    - this[1] * ( this[4] * A2323 - this[6] * A0323 + this[7] * A0223 )
	    + this[2] * ( this[4] * A1323 - this[5] * A0323 + this[7] * A0123 )
	    - this[3] * ( this[4] * A1223 - this[5] * A0223 + this[6] * A0123 );
	det = 1 / det;
	return new Matrix(
	    [ det *   ( this[5] * A2323 - this[6] * A1323 + this[7] * A1223 ),
	      det * - ( this[1] * A2323 - this[2] * A1323 + this[3] * A1223 ),
	      det *   ( this[1] * A2313 - this[2] * A1313 + this[3] * A1213 ),
	      det * - ( this[1] * A2312 - this[2] * A1312 + this[3] * A1212 ),
	      det * - ( this[4] * A2323 - this[6] * A0323 + this[7] * A0223 ),
	      det *   ( this[0] * A2323 - this[2] * A0323 + this[3] * A0223 ),
	      det * - ( this[0] * A2313 - this[2] * A0313 + this[3] * A0213 ),
	      det *   ( this[0] * A2312 - this[2] * A0312 + this[3] * A0212 ),
	      det *   ( this[4] * A1323 - this[5] * A0323 + this[7] * A0123 ),
	      det * - ( this[0] * A1323 - this[1] * A0323 + this[3] * A0123 ),
	      det *   ( this[0] * A1313 - this[1] * A0313 + this[3] * A0113 ),
	      det * - ( this[0] * A1312 - this[1] * A0312 + this[3] * A0112 ),
	      det * - ( this[4] * A1223 - this[5] * A0223 + this[6] * A0123 ),
	      det *   ( this[0] * A1223 - this[1] * A0223 + this[2] * A0123 ),
	      det * - ( this[0] * A1213 - this[1] * A0213 + this[2] * A0113 ),
	      det *   ( this[0] * A1212 - this[1] * A0212 + this[2] * A0112 ) ] );
    }

    transpose()
    {
	var m=new Matrix();
	for (var i=0; i<dim; i++)
	    for (var j=0; j<dim; j++)
		m[i+dim*j]=this[j+dim*i];
	return m;
    }
};

function vector(v) { return new Vector(v); }
function matrix(m) { return new Matrix(m); }
function times(n,x) { return { number: n, content: x }; }
