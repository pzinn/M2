--		Copyright 1994 by Daniel R. Grayson
--- This file contains functions for parsing and expressions that need to use stdio.

use pthread0;
use stdiop;
use gmp1;
use xml;
use engine;
use varnets;
use expr;


-- misc

export flushToken(f:TokenFile):void := (f.nexttoken=NULL; flushInput(f.posFile););
export close(file:TokenFile):int := close(file.posFile);
export fileErrorMessage(f:TokenFile):string := fileErrorMessage(f.posFile);
export fileError(f:TokenFile):bool := fileError(f.posFile);
export clearFileError(f:TokenFile):void := clearFileError(f.posFile);
export isatty(f:TokenFile):bool := isatty(f.posFile);

-- Expr Functions

export getLocalDictionary(frameID:int):Dictionary := (
     p := allDictionaries;
     while (
	  if p.dictionary.frameID == frameID then return p.dictionary;
	  p != p.next) do p = p.next;
     error("internal error: local dictionary with frameID " + tostring(frameID) + " not found");
     dummyDictionary);
export localDictionaryClosure(f:Frame):DictionaryClosure := DictionaryClosure(noRecycle(f),getLocalDictionary(f.frameID));





-- debugging

export returnMessage := Expr(stringCell("unhandled return command"));
export continueMessage := Expr(stringCell("unhandled continue command"));
export continueMessageWithArg := Expr(stringCell("unhandled continue command, with argument"));
export stepMessage := Expr(stringCell("unhandled step command"));
export stepMessageWithArg := Expr(stringCell("unhandled step command, with argument"));
export breakMessage := Expr(stringCell("unhandled break command"));
export throwMessage := Expr(stringCell("unhandled throw command"));
export unwindMessage := Expr(stringCell("unhandled unwind command"));
export interruptMessage := Expr(stringCell("interrupted"));
export alarmMessage := Expr(stringCell("alarm occurred"));
export steppingMessage := Expr(stringCell("--stepping limit reached"));
--export buildErrorPacket(message:string):Expr := Expr(Error(dummyPosition,message,nullE,false,dummyFrame));
--export buildErrorPacket(pos:Position,message:string):Expr := Expr(Error(pos,message,nullE,false,dummyFrame));
--export buildErrorPacketErrno(msg:string,errnum:int):Expr := buildErrorPacket( msg + ": " + strerror(errnum) );
export cwd():Expr := (
     r := getcwd();
     if r === "" then buildErrorPacket("can't get current working directory: " + syserrmsg())
     else Expr(stringCell(r)));
dummyDebuggerFun(f:Frame,c:Code):Expr := nullE;
export debuggerFun := dummyDebuggerFun;
export handleInterrupts := true;
(threadLocal export stopIfError := true) = false;
(threadLocal export debuggingMode := false) = true;

export location(t:Token):Location := Location(t.filename,t.line,t.column,t.line2,t.column2,t.line,t.column,t.loadDepth);
export position(t:Token):Position := Position(t.filename,t.line,t.column,t.loadDepth);

export (x:SymbolClosure) === (y:SymbolClosure) : bool := (
     x == y || x.symbol == y.symbol && x.frame == y.frame
     );
export (x:SymbolBody) === (y:SymbolBody) : bool := (
     x == y || x.symbol == y.symbol
     );
export (x:SymbolClosure) === (y:SymbolBody) : bool := (
     x.symbol == y.symbol
     );
export (x:SymbolBody) === (y:SymbolClosure) : bool := (
     x.symbol == y.symbol
     );
export (x:Symbol) === (y:SymbolClosure) : bool := x == y.symbol;
export (x:Symbol) === (y:SymbolBody   ) : bool := x == y.symbol;
export (x:SymbolClosure) === (y:Symbol) : bool := x.symbol == y;
export (x:SymbolBody   ) === (y:Symbol) : bool := x.symbol == y;
export (x:SymbolClosure) === (y:Expr):bool := (
     x == y || (
	  when y
	  is z:SymbolClosure do x.symbol == z.symbol && x.frame == z.frame
	  is z:SymbolBody    do x.symbol == z.symbol
	  else false
	  ));
export (x:Expr) === (y:SymbolClosure):bool := (
     y == x || (
	  when x
	  is z:SymbolClosure do y.symbol == z.symbol && y.frame == z.frame
	  is z:SymbolBody    do y.symbol == z.symbol
	  else false
	  ));
export (x:Symbol) === (y:Expr):bool := (
     when y
     is z:SymbolClosure do x == z.symbol
     is z:SymbolBody    do x == z.symbol
     else false);
export (x:Expr) === (y:Symbol):bool := (
     when x 
     is z:SymbolClosure do y == z.symbol     
     is z:SymbolBody    do y == z.symbol
     else false);

-- operator names for the disassembler
export dummyUnop(c:Code):Expr := nullE;
export dummyBinop(c:Code,d:Code):Expr := nullE;
export dummyTernop(c:Code,d:Code,e:Code):Expr := nullE;
export dummyMultop(s:CodeSequence):Expr := nullE;
export unopNameListCell := {f:unop,name:Symbol,next:unopNameListCell};
export binopNameListCell := {f:binop,name:Symbol,next:binopNameListCell};
export ternopNameListCell := {f:ternop,name:Symbol,next:ternopNameListCell};
export multopNameListCell := {f:multop,name:Symbol,next:multopNameListCell};
export unopNameList := unopNameListCell(dummyUnop,dummySymbol,self);
export binopNameList := binopNameListCell(dummyBinop,dummySymbol,self);
export ternopNameList := ternopNameListCell(dummyTernop,dummySymbol,self);
export multopNameList := multopNameListCell(dummyMultop,dummySymbol,self);
export getUnopName(f:unop):Symbol := (
     p := unopNameList;
     while true do (
	  if p == p.next || p.f == f then return p.name;
	  p = p.next;
	  ));
export getBinopName(f:binop):Symbol := (
     p := binopNameList;
     while true do (
	  if p == p.next || p.f == f then return p.name;
	  p = p.next;
	  ));
export getTernopName(f:ternop):Symbol := (
     p := ternopNameList;
     while true do (
	  if p == p.next || p.f == f then return p.name;
	  p = p.next;
	  ));
export getMultopName(f:multop):Symbol := (
     p := multopNameList;
     while true do (
	  if p == p.next || p.f == f then return p.name;
	  p = p.next;
	  ));


-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d tokens.o "
-- End:
