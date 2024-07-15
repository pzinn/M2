use nets;

-- from stdiop
export Position := {filename:string, line:ushort, column:ushort, line1:ushort, column1:ushort, line2:ushort, column2:ushort, loadDepth:ushort};
export dummyPosition := Position("-*dummy file name*-",ushort(0),ushort(0),ushort(0),ushort(0),ushort(0),ushort(0),loadDepth);
export tempPosition := Position("-*temp*-",ushort(0),ushort(0),ushort(0),ushort(0),ushort(0),ushort(0),loadDepth);
export (s:Position) === (t:Position) : bool := s == t || s.filename === t.filename && s.line == t.line && s.column == t.column; -- TODO review

