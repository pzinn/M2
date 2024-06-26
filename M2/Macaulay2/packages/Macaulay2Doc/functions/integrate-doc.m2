doc ///
  Key
    integrate
    (integrate, Function, InfiniteNumber, InfiniteNumber)
    (integrate, Function, InfiniteNumber, Number)
    (integrate, Function, Number, InfiniteNumber)
    (integrate, Function, Number, Number)
  Headline
    numerical integration
  Usage
    integrate(f, a, b)
  Inputs
    f:Function
    a:{Number, InfiniteNumber}
    b:{Number, InfiniteNumber}
  Outputs
    :RR
  Description
    Text
      Integrate @TT "f"@ from @TT "a"@ to @TT "b"@ numerically, using
      @wikipedia "Gaussian quadrature"@.
    Example
      integrate(sin, 0, pi)
    Text
      For half-infinite intervals, 20-point
      @wikipedia "Gauss-Laguerre quadrature"@ is used.
    Example
      integrate(x -> exp(-x), 0, infinity)
    Text
      For infinite intervals, 20-point
      @wikipedia "Gauss-Hermite quadrature"@ is used.
    Example
      integrate(x -> exp(-x^2), -infinity, infinity)
///
