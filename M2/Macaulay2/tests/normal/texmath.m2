stripTexMathSpaces = s -> concatenate for i to #s-1 list (
    if s#i === " " and (i === 0 or s#(i-1) =!= "\\") then "" else s#i)
stripTexMathBraces = s -> (
    while #s >= 2 and s#0 === "{" and s#(#s-1) === "}" do s = substring(s, 1, #s-2);
    s)
normalizeTexMath = stripTexMathBraces @@ stripTexMathSpaces
assertTexMath = (x, s) -> assert Equation(normalizeTexMath texMath x, normalizeTexMath s)

assertTexMath(0, ///0///)
assertTexMath(-7, ///-7///)
assertTexMath(3/5, ///\frac{3}{5}///)
assertTexMath(2.5, ///{2.5}///)
assertTexMath(true, ///\texttt{true}///)
assertTexMath(infinity, ///\infty///)
assertTexMath(-infinity, ///{-\infty}///)

assertTexMath("alpha_beta", ///\texttt{alpha\char95 beta}///)
assertTexMath(symbol alpha, ///\mathit{alpha}///)
assertTexMath(ZZ, ///{\mathbb Z}///)
assertTexMath(QQ, ///{\mathbb Q}///)

assertTexMath((1,2,3), ///\left(1,\,2,\,3\right)///)
assertTexMath({1,2,3}, ///\left\{1,\,2,\,3\right\}///)
assertTexMath([1,2,3], ///\left[1,\,2,\,3\right]///)
assertTexMath(new Option from (a=>1), ///a\ \Rightarrow \ 1///)
assertTexMath(new HashTable, ///\texttt{HashTable}\left\{\,\right\}///)
assertTexMath(new HashTable from {a=>1,b=>{2,3}}, ///\texttt{HashTable}\left\{a\ \Rightarrow \ 1,\,b\ \Rightarrow \ \left\{2,\,3\right\}\right\}///)
assertTexMath(new MutableList from {1,2}, ///\texttt{MutableList}\left\{\ldots 2\ldots\right\}///)

R = QQ[x,y,z]
assertTexMath(R, ///R///)
assertTexMath(x*y^2, ///x\,y^{2}///)
assertTexMath(x^2 + y*z - 3, ///x^{2}+y\,z-3///)
assertTexMath(ideal(x^2,y*z), ///\texttt{ideal}{}\left(x^{2},\,y\,z\right)///)
assertTexMath(R/ideal(x^2-y), ///\frac{R}{x^{2}-y}///)
assertTexMath(R^2, ///R^{2}///)
assertTexMath(map(R,R,{x+y,y,z}), ///\texttt{map}{}\left(R,\,R,\,\left\{x+y,\,y,\,z\right\}\right)///)

assertTexMath(matrix{{x,y},{z,1}}, ///\left(\!\begin{array}{cc}
x&y\\
z&1
\end{array}\!\right)///)
assertTexMath(transpose matrix{{x,y,z}}, ///\begin{array}{l}\left\{-1\right\}\vphantom{x}\\\left\{-1\right\}\vphantom{y}\\\left\{-1\right\}\vphantom{z}\end{array}\left(\!\begin{array}{c}
\vphantom{\left\{-1\right\}}x\\
\vphantom{\left\{-1\right\}}y\\
\vphantom{\left\{-1\right\}}z
\end{array}\!\right)///)
assertTexMath(betti res coker matrix{{x,y}}, ///\begin{matrix}
 & 0 & 1 & 2\\
\text{total:} & 1 & 2 & 1\\
0: & 1 & 2 & 1
\end{matrix}///)
assertTexMath(net matrix{{1,2},{3,4}}, ///\begin{array}{l}\texttt{\char124  1 2 \char124 }\\
\texttt{\char124  3 4 \char124 }\end{array}///)
assertTexMath(new VerticalList from {x,y}, ///\left\{\begin{aligned}&x\\&y\end{aligned}\right\}///)
