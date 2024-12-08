import munit.FunSuite
import utils.uint.*

class UInt16Tests extends FunSuite:
  test("overflow"):
    assertEquals(UInt16(65535) + UInt16(1), UInt16(0))

  test("underflow"):
    assertEquals(UInt16(0) - UInt16(1), UInt16(65535))

  test("binary ops"):
    assertEquals(UInt16(123) & UInt16(456), UInt16(72))
    assertEquals(UInt16(123) | UInt16(456), UInt16(507))
    assertEquals(UInt16(123) << UInt16(2), UInt16(492))
    assertEquals(UInt16(456) >> UInt16(2), UInt16(114))
    assertEquals(~UInt16(123), UInt16(65412))
    assertEquals(~UInt16(456), UInt16(65079))
