wikipedia = method(TypicalValue => Hypertext)
wikipedia String          :=       title  -> HREF{ "https://en.wikipedia.org/wiki/" | title, title }
wikipedia(String, String) := (url, title) -> HREF{ "https://en.wikipedia.org/wiki/" |   url, title }

arXiv = method(TypicalValue => Hypertext)
arXiv String          :=  ref         -> HREF{ "https://arxiv.org/abs/" | ref, "arXiv:" | ref }
arXiv(String, String) := (ref, title) -> HREF{ "https://arxiv.org/abs/" | ref, title }

stacksProject = method(TypicalValue => Hypertext)
stacksProject(String, String) := (tag, title) -> HREF{ "https://stacks.math.columbia.edu/tag/" | tag, title }

oeis = method(TypicalValue => NumberedVerticalList,
    Options => {Limit => 100, Position => 0})
oeisHTTP := "http://oeis.org";
oeisHTTPS := "https://oeis.org";
oeis VisibleList := o -> L -> oeis (demark("%2C",toString\L),o)
oeis String := o -> search -> (
    url:=oeisHTTP|"/search?q="|search|"&fmt=text&start="|o.Position|"&n="|o.Limit; -- limit the number of results
    www := last splitWWW getWWW url;
    ans := select("(?<=^%N ).*$",www);    
    NumberedVerticalList apply(ans, line -> SPAN(
            blank := regex(" ",line);
            if blank === null then line -- shouldn't happen
            else (
                pos := blank#0#0;
                seq := substring(0,pos,line);
                {HREF {oeisHTTPS|"/"|seq,seq} ,substring(pos,line)}
            )))
    )
-- e.g. oeis {1,2,7,42}
