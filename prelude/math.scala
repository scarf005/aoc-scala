package prelude

extension (n: Int)
  inline infix def divmod(d: Int) = (n / d, n % d)
  inline infix def moddiv(d: Int) = (n % d, n / d)
