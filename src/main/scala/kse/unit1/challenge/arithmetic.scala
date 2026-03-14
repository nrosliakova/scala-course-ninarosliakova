package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val Z: Number => Number = x => x - 1
  val S: Number => Number = x => x + 1

  /**
   * Optional task: make `addition` tail-recursive.
   */
  val addition: (Number, Number) => Number = (x, y) =>
    if y == 0 then x
    else addition(S(x), Z(y))

  /**
   * Optional task: make `multiplication` tail-recursive.
   */
  val multiplication: (Number, Number) => Number = (x, y) =>
    var result = 0L
    var i = y
    while (y != 0)
      result = addition(result, x)
      i = Z(i)
    result


  /**
   * Optional task: make `power` tail-recursive.
   */
  val power: (Number, Number) => Number = (x, y) =>
    var result = 0L
    var i = y
    while (y != 0)
      result = multiplication(result, x)
      i = Z(i)
    result
