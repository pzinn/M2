

YngTableau = new Type of SkewTableau

yngTableau = method(TypicalValue => YngTableau)
yngTableau (Partition,List) := (lam,theList) -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    if (numBoxesNeeded != #theList) then error "partition sizes do not match with the length of the list";
    if any(theList, theElt -> theElt === null) then error "filling must not contain null entries";

    mu := new Partition from {};

    new YngTableau from {
        "outerShape" => lam,
        "innerShape" => mu,
        values => theList
        }
    )
yngTableau Partition := lam -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    yngTableau(lam, toList(numBoxesNeeded:""))
    )

skewTableau YngTableau := T -> new SkewTableau from T

shape = method(TypicalValue => Partition)
shape YngTableau := T -> (
    T#"outerShape"
    )

truncate Partition := theInput -> (
    lam := theInput#1;
    numTrailingZeros := # for i from 1 to #lam list (if lam#-i == 0 then 1 else break);

    lamShortened := (toList lam)_(toList(0..(#lam-1-numTrailingZeros)));

    new Partition from lamShortened
    )
