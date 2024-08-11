package utils.uint

opaque type UInt16 = Int

private def to16(value: Int) = value & 0xffff

object UInt16:
  given CanEqual[UInt16, UInt16] = CanEqual.derived
  def apply(value: Int): UInt16 = to16(value)
  extension (x: UInt16)
    def +(y: UInt16): UInt16 = to16(x + y)
    def -(y: UInt16): UInt16 = to16(x - y)
    def *(y: UInt16): UInt16 = to16(x * y)
    def /(y: UInt16): UInt16 = to16(x / y)
    def &(y: UInt16): UInt16 = to16(x & y)
    def |(y: UInt16): UInt16 = to16(x | y)
    def ^(y: UInt16): UInt16 = to16(x ^ y)
    def <<(y: UInt16): UInt16 = to16(x << y)
    def >>(y: UInt16): UInt16 = to16(x >> y)
    def %(y: UInt16): UInt16 = to16(x % y)
    def unary_~ : UInt16 = to16(~x)
    def <(y: UInt16): Boolean = x < y
    def <=(y: UInt16): Boolean = x <= y
    def >(y: UInt16): Boolean = x > y
    def >=(y: UInt16): Boolean = x >= y
    def ==(y: UInt16): Boolean = x == y
