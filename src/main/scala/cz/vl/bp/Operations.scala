package cz.vl.bp

import breeze.linalg.DenseMatrix
import cats.implicits._

object Op {
  type Mat = DenseMatrix[Double]
  type ParamContext = Map[Param, Double]

  case class Param(name: String)
  case class BpContext(params: ParamContext, inputs: Map[Placeholder, Mat], stepSize: Double)
}

import cz.vl.bp.Op._

trait Op {
  def forward(implicit context: BpContext): Mat

  def backward(dir: Mat)(implicit context: BpContext): ParamContext

  def *(p: Param): Op = Scale(this, p)

  def *(that: Op): Op = Dot(this, that)

  def +(that: Op): Op = Sum(this, that)

  @transient override lazy val hashCode: Int = super.hashCode
}


case class Placeholder(name: String) extends Op {
  override def forward(implicit context: BpContext): Mat =
    context.inputs(this)

  override def backward(dir: Mat)(implicit context: BpContext): ParamContext =
    Map.empty
}


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

case class Scale(m: Op, p: Param) extends Op {

  override def forward(implicit context: BpContext): Mat =
    m.forward * value

  override def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val diff = context.stepSize * (dir * m.forward)
    assert(diff.size == 1)

    val optOther = m.backward(dir * value)
    val optP = value - diff.data(0)

    optOther + (p -> optP)
  }

  private def value(implicit context: BpContext) = context.params(p)

}

case class Sum(l: Op, r: Op) extends Op {
  override def forward(implicit context: BpContext): Mat =
    l.forward + r.forward

  override def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir)
    val rOpt = r.backward(dir)

    lOpt |+| rOpt
  }
}

case class Dot(l: Op, r: Op) extends Op {
  override def forward(implicit context: BpContext): Mat =
    l.forward.t * r.forward

  override def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir * r.forward.t)
    val rOpt = r.backward(dir * l.forward.t)

    lOpt |+| rOpt
  }
}
