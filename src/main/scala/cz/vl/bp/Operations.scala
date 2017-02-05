package cz.vl.bp

import breeze.linalg.DenseMatrix
import cats.implicits._
import com.fasterxml.jackson.annotation.JsonTypeInfo

object Op {

  type Mat = DenseMatrix[Double]
  type ParamContext = Map[Param, Double]

  case class Param(name: String)
  case class BpContext(params: ParamContext, inputs: Map[Placeholder, Mat], stepSize: Double)

}

import cz.vl.bp.Op._

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY)
trait Op {

  def forward(implicit context: BpContext): Mat

  def backward(dir: Mat)(implicit context: BpContext): ParamContext

  def *(p: Param): Op = Scale(this, p)

  def *(that: Op): Op = Dot(this, that)

  def +(that: Op): Op = Plus(this, that)

  def -(that: Op): Op = Minus(this, that)

  @transient override lazy val hashCode: Int = super.hashCode

}

case class Const(mat: Mat) extends Op {

  def forward(implicit context: BpContext): Mat =
    mat

  def backward(dir: Mat)(implicit context: BpContext): ParamContext =
    Map.empty

}

case class Placeholder(name: String) extends Op {

  def forward(implicit context: BpContext): Mat =
    context.inputs(this)

  def backward(dir: Mat)(implicit context: BpContext): ParamContext =
    Map.empty

}


object L2Distance {
  def apply(expected: Mat, actual: Op): FrobeniusNorm = FrobeniusNorm(actual - Const(expected))
}

case class FrobeniusNorm(op: Op) extends Op {
  def forward(implicit context: BpContext): Mat = {
    val dist = op.forward.data.map(math.pow(_, 2)).sum * 2

    DenseMatrix.fill(1, 1)(dist)
  }

  def backward(dir: Mat)(implicit context: BpContext): ParamContext =
    op.backward(dir)

  def optimize(implicit context: BpContext): ParamContext = {
    val dir = op.forward.t
    this.backward(dir)
  }
}

case class Scale(m: Op, p: Param) extends Op {

  def forward(implicit context: BpContext): Mat =
    m.forward * value

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val diff = context.stepSize * (dir * m.forward)
    assert(diff.size == 1)

    val optOther = m.backward(dir * value)
    val optP = value - diff.data(0)

    optOther + (p -> optP)
  }

  private def value(implicit context: BpContext) = context.params(p)

}

case class Plus(l: Op, r: Op) extends Op {
  def forward(implicit context: BpContext): Mat =
    l.forward + r.forward

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir)
    val rOpt = r.backward(dir)

    lOpt |+| rOpt
  }
}

case class Minus(l: Op, r: Op) extends Op {
  def forward(implicit context: BpContext): Mat =
    l.forward - r.forward

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir)
    val rOpt = r.backward(-dir)

    lOpt |+| rOpt
  }
}

case class Dot(l: Op, r: Op) extends Op {
  def forward(implicit context: BpContext): Mat =
    l.forward.t * r.forward

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir * r.forward.t)
    val rOpt = r.backward(dir * l.forward.t)


    lOpt |+| rOpt
  }
}
