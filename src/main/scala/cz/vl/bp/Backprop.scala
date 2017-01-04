package cz.vl.bp

import breeze.linalg._
import cats.implicits._
import cz.vl.bp.Op._
import cz.vl.bp.Utils._

object Backprop {

  def vect(d: Double*): DenseMatrix[Double] = DenseMatrix.create(d.length, 1, d.toArray)

  val s1 = Placeholder("i1")
  val s2 = Placeholder("i2")
  val s3 = Placeholder("i3")

  val w1 = Param("w1")
  val w2 = Param("w2")
  val w3 = Param("w3")

  val expression = (s1 * w1 + s2 * w2) * (s3 * w3)
  println(expression.toJson)

  val initialParams: ParamContext = Map(w1 -> 1.5, w2 -> -1.5, w3 -> -1.0)
  val inputValues = Map(s1 -> vect(1, 2), s2 -> vect(2, 3), s3 -> vect(-1, 1))

  val data: Seq[(Map[Placeholder, Mat], Double)] = List(
    (Map(s3 -> vect(8, 3), s1 -> vect(0, -3), s2 -> vect(-5, 8)), 7),
    (Map(s3 -> vect(9, 6), s1 -> vect(1, -3), s2 -> vect(2, 6)), -63),
    (Map(s3 -> vect(-4, -3), s1 -> vect(-10, 8), s2 -> vect(-5, 3)), 5),
    (Map(s3 -> vect(-4, -8), s1 -> vect(-2, 9), s2 -> vect(-9, -8)), -164),
    (Map(s3 -> vect(4, 3), s1 -> vect(8, 4), s2 -> vect(-5, -2)), 70),
    (Map(s3 -> vect(0, -4), s1 -> vect(-10, -3), s2 -> vect(3, 2)), 20),
    (Map(s3 -> vect(4, 9), s1 -> vect(0, -1), s2 -> vect(9, -7)), 18),
    (Map(s3 -> vect(9, 0), s1 -> vect(3, -8), s2 -> vect(0, 9)), 27),
    (Map(s3 -> vect(-1, 7), s1 -> vect(1, -10), s2 -> vect(6, -4)), -37),
    (Map(s3 -> vect(0, -1), s1 -> vect(-1, 6), s2 -> vect(4, -2)), -8),
    (Map(s3 -> vect(7, -8), s1 -> vect(-3, -4), s2 -> vect(-7, 8)), 124),
    (Map(s3 -> vect(-5, -3), s1 -> vect(-4, 0), s2 -> vect(3, 9)), 62),
    (Map(s3 -> vect(-10, 3), s1 -> vect(3, -5), s2 -> vect(-1, 2)), -61),
    (Map(s3 -> vect(-6, 2), s1 -> vect(-6, -9), s2 -> vect(6, 3)), 48),
    (Map(s3 -> vect(-4, 4), s1 -> vect(-1, 9), s2 -> vect(-9, -2)), 12),
    (Map(s3 -> vect(9, 0), s1 -> vect(7, -4), s2 -> vect(-7, -3)), 126),
    (Map(s3 -> vect(1, -2), s1 -> vect(9, -3), s2 -> vect(-8, 6)), 35),
    (Map(s3 -> vect(2, 9), s1 -> vect(6, -3), s2 -> vect(-8, 5)), -44),
    (Map(s3 -> vect(-1, 8), s1 -> vect(1, 9), s2 -> vect(1, -1)), 80),
    (Map(s3 -> vect(6, 3), s1 -> vect(-5, -3), s2 -> vect(-5, -8)), 15)
  )

  def runEpoch(params: ParamContext, data: Seq[(Map[Placeholder, Mat], Double)]): ParamContext = {
    val (paramFinal, err) = data.foldLeft((params, 0.0)) {
      case ((paramsBefore, errBefore), (inputs, output)) =>
        implicit val context = BpContext(paramsBefore, inputs, stepSize = 0.000005)
        val distanceFunction = l2Distance(vect(output), expression)

        val outputActual = expression.forward.data(0)
        val distanceActual = distanceFunction.forward.data(0)

        val paramsAfter = distanceFunction.optimize
        val outputAfter = expression.forward(context.copy(params = paramsAfter)).data(0)

        println(f"$distanceActual%.2f - e:$output%.2f a:$outputActual%.2f o:$outputAfter%.2f")

        (paramsAfter, errBefore + distanceActual)
    }

    println(s"AVERAGE ERROR --------- ${err / data.length}")
    println(s"PARAMS --------- $paramFinal")

    paramFinal
  }

  def main(args: Array[String]): Unit = {
    val nEpochs = 20

    Iterator
      .fill(nEpochs)(data)
      .foldLeft(initialParams)(runEpoch)
  }

}
