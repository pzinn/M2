use nets;

-- from stdiop
export Position := {filename:string, line:ushort, column:ushort, line2:ushort, column2:ushort, loadDepth:ushort};
export dummyPosition := Position("-*dummy file name*-",ushort(0),ushort(0),ushort(0),ushort(0),loadDepth);
export (s:Position) === (t:Position) : bool := s == t || s.filename === t.filename && s.line == t.line && s.column == t.column && s.line2 == t.line2 && s.column2 == t.column2;

