package cz.vl.bp

import breeze.linalg.{DenseMatrix, _}
import cats.implicits._

object Bp extends App {

  type Mat = DenseMatrix[Double]
  type ParamContext = Map[Param, Double]

  trait Op {
    def forward(implicit context: BpContext): Mat

    def backward(dir: Mat)(implicit context: BpContext): ParamContext

    def *(p: Param): Op = Scale(this, p)

    def *(that: Op): Op = Dot(this, that)

    def +(that: Op): Op = Sum(this, that)

    override lazy val hashCode: Int = super.hashCode
  }

  case class Placeholder(name: String) extends Op {
    override def forward(implicit context: BpContext): Mat = context.inputs(this)

    override def backward(dir: Mat)(implicit context: BpContext): ParamContext = Map.empty
  }

  case class Param(name: String)

  case class BpContext(params: ParamContext, inputs: Map[Placeholder, Mat], step: Double)

  case class FrobeniusNorm(expected: Mat, actual: Op) extends Op {
    override def forward(implicit context: BpContext): Mat = {
      val dist = (expected - actual.forward).data.map(math.pow(_, 2)).sum

      DenseMatrix.fill(1, 1)(dist)
    }

    override def backward(dir: Mat)(implicit context: BpContext): ParamContext =
      actual.backward(dir)

    def optimize(implicit context: BpContext): ParamContext = {
      val dir = (actual.forward - expected).t
      this.backward(dir)
    }
  }

  case class Source(in: Placeholder) extends Op {
    override def forward(implicit context: BpContext): Mat = context.inputs(in)

    override def backward(dir: Mat)(implicit context: BpContext): ParamContext = Map.empty
  }

  case class Scale(m: Op, p: Param) extends Op {

    override def forward(implicit context: BpContext): Mat = m.forward * value

    override def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
      val diff = context.step * (dir * m.forward)
      assert(diff.size == 1)

      val optOther = m.backward(dir * value)
      val optP = value - diff.data(0)

      optOther |+| Map(p -> optP)
    }

    private def value(implicit context: BpContext) = context.params(p)

  }

  case class Sum(l: Op, r: Op) extends Op {
    override def forward(implicit context: BpContext): Mat = l.forward + r.forward

    override def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
      val lOpt = l.backward(dir)
      val rOpt = r.backward(dir)

      lOpt |+| rOpt
    }
  }

  case class Dot(l: Op, r: Op) extends Op {
    override def forward(implicit context: BpContext): Mat = l.forward.t * r.forward

    override def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
      val lOpt = l.backward(dir * r.forward.t)
      val rOpt = r.backward(dir * l.forward.t)

      lOpt |+| rOpt
    }
  }

  def vect(d: Double*): DenseMatrix[Double] = DenseMatrix.create(d.length, 1, d.toArray)

  val s1 = Placeholder("i1")
  val s2 = Placeholder("i2")
  val s3 = Placeholder("i3")

  val w1 = Param("w1")
  val w2 = Param("w2")
  val w3 = Param("w3")

  val expression = (s1 * w1 + s2 * w2) * (s3 * w3)

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
        val context = BpContext(params, inputs, step = 0.3)
        println(expression.forward(context))
        FrobeniusNorm(expectedOutput, expression).optimize(context)
    }

}


