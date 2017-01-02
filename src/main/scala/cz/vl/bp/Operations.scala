package cz.vl.bp

import breeze.linalg.DenseMatrix
import cats.implicits._
import com.fasterxml.jackson.annotation.JsonTypeInfo

object Op {
  type Mat = DenseMatrix[Double]
  type ParamContext = Map[Param, Double]

  case class Param(name: String)
  case class BpContext(params: ParamContext, inputs: Map[Placeholder, Mat], stepSize: Double)

  implicit def matConst(mat: Mat) = new Const(mat)

  def l2Distance(expected: Mat, actual: Op): FrobeniusNorm = new FrobeniusNorm(actual - expected)

}

import cz.vl.bp.Op._

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY)
trait Op {

  def forward(implicit context: BpContext): Mat

  def backward(dir: Mat)(implicit context: BpContext): ParamContext

  def *(p: Param): Op = new Scale(this, p)

  def *(that: Op): Op = new Dot(this, that)

  def +(that: Op): Op = new Plus(this, that)

  def -(that: Op): Op = new Minus(this, that)

  @transient override lazy val hashCode: Int = super.hashCode

  @transient override lazy val toString: String = super.toString

}

class Const(val mat: Mat) extends Op {

  def forward(implicit context: BpContext): Mat =
    mat

  def backward(dir: Mat)(implicit context: BpContext): ParamContext =
    Map.empty

}

case class Placeholder(val name: String) extends Op {

  def forward(implicit context: BpContext): Mat =
    context.inputs(this)

  def backward(dir: Mat)(implicit context: BpContext): ParamContext =
    Map.empty

}


class FrobeniusNorm(val op: Op) extends Op {
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

case class Scale(val m: Op, val p: Param) extends Op {

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

class Plus(val l: Op, val r: Op) extends Op {
  def forward(implicit context: BpContext): Mat =
    l.forward + r.forward

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir)
    val rOpt = r.backward(dir)

    lOpt |+| rOpt
  }
}

class Minus(val l: Op, val r: Op) extends Op {
  def forward(implicit context: BpContext): Mat =
    l.forward - r.forward

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir)
    val rOpt = r.backward(-dir)

    lOpt |+| rOpt
  }
}

class Dot(val l: Op, val r: Op) extends Op {
  def forward(implicit context: BpContext): Mat =
    l.forward.t * r.forward

  def backward(dir: Mat)(implicit context: BpContext): ParamContext = {
    val lOpt = l.backward(dir * r.forward.t)
    val rOpt = r.backward(dir * l.forward.t)


    lOpt |+| rOpt
  }
}
