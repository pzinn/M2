document { Key => "foo", "hi there", EXAMPLE "a", "ho there", EXAMPLE "b" }
ex = examples "foo"
assert( ex#-2 == "a" )
assert( ex#-1 == "b" )

-- if changing this, make sure that for instance
-- Tutorial: Beginning Macaulay2 still looks okay.
debug Core
expected = "<table class=\"examples\">
  <tr>
    <td>
      <pre><code class=\"language-macaulay2\">M2</code></pre>
    </td>
  </tr>
</table>\n"
result = EXAMPLE PRE "M2"
assert Equation(html result, expected)

-- A WebApp installer must also accept cached Standard-mode examples.
savedM2outputRE = M2outputRE
cellTag = ascii 19
M2outputRE = "(?=" | cellTag | ")"
plainTranscript = "-- cached output\n\ni1 : 1\n\no1 = 1\n\ni2 : 2\n\no2 = 2\n"
assert(separateM2output plainTranscript == {"i1 : 1\n\no1 = 1", "i2 : 2\n\no2 = 2"})
webTranscript = cellTag | "first result\ni99 : quoted prompt" | cellTag | "second result"
assert(separateM2output webTranscript == {
    cellTag | "first result\ni99 : quoted prompt", cellTag | "second result"})
M2outputRE = savedM2outputRE
