# Expressions
## The philosophy of <tt>expression</tt>
<i>Expressions</i> are an intermediate layer between mathematical objects (or other Macaulay2 objects) and their output to the screen:
```
help Expression
```

blah blah blah

## The change [on refactor/expression]
Expressions are now by default turned on for (almost) all types. Let us take an example.
Say you define a new type
```
MyType = new Type of BasicList
```
Traditionally you would directly define output routines such as <tt>net</tt> (for <tt>Standard</tt> output) or <tt>texMath</tt> (for <tt>WebApp</tt> output).
Here we do both so this tutorial works in both modes:
```
net MyType := x -> net x#0 | " |- " | net x#1;
texMath MyType := x -> texMath x#0 | "\\vdash " | texMath x#1;
```

Then you would create a new object, and everything looks okay:
```
X = new MyType from {a,b}
```

but now you use <tt>X</tt> somewhere, say
```
someOption => X
```
and surprise, instead of the expected \( \textit{someOption}\ \Rightarrow\ a\vdash b\),
it says \( \textit{someOption}\ \Rightarrow\ \texttt{MyType}\{a,\,b\} \).

Why? because <tt>expression</tt> has not been redefined, so
```
expression X
```
is inherited from <tt>BasicList</tt>.

Two possible fixes:

1. The quick fix: return to the previous situation by deactivating <tt>expression</tt>:
```
expression MyType := hold
```
Now everything works fine. (modulo parenthesization issues which we ignore for now)

2. The better way: switch to using <tt>expression</tt>. Let's start over:
```
MyType = new Type of BasicList
```
but this time define only the expression:
```
expression MyType := x -> expression x#0 |- expression x#1
```
This will automatically take care of every possible form of output (ascii, tex, html...).

##Remark on value
For experts only.
It is good taste to produce with <tt>expression</tt> an </tt>Expression</tt> on which
<tt>value</tt> can be run, returning the original object (or at least the closest possible
approximation to it). By default this will not be the case with the example of the previous page:
```
value expression X
```
However, this can be easily fixed by adding e.g.
```
Thing |- Thing := (x,y) -> new MyType from {x,y}
```
Now it will work, and in fact
```
value expression X === X
```

##Some remark on globalAssign
TODO