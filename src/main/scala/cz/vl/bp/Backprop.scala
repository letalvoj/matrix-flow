package cz.vl.bp

import breeze.linalg._
import cats.implicits._
import cz.vl.bp.Utils._
import cz.vl.bp.Op._

object Backprop extends App {

  def vect(d: Double*): DenseMatrix[Double] = DenseMatrix.create(d.length, 1, d.toArray)

  val s1 = Placeholder("i1")
  val s2 = Placeholder("i2")
  val s3 = Placeholder("i3")

  val w1 = Param("w1")
  val w2 = Param("w2")
  val w3 = Param("w3")

  val expression = (s1 * w1 + s2 * w2) * (s3 * w3)
  println(expression.toJson)

  val initialParams = Map(w1 -> 1.0, w2 -> -1.0, w3 -> 1.0)
  val inputValues = Map(
    s1 -> vect(1, 2),
    s2 -> vect(2, 3),
    s3 -> vect(-1, 1)
  )
  val expectedOutput = vect(0.5)

  (0 to 20)
    .map(_ => (inputValues, expectedOutput))
    .foldLeft(initialParams) {
      case (params, (inputs, output)) =>
        val context = BpContext(params, inputs, stepSize = 0.3)
        println(expression.forward(context))
        FrobeniusNorm(expectedOutput, expression).optimize(context)
    }

}
