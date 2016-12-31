package cz.vl.bp

import breeze.linalg.{DenseMatrix, norm, _}

object Bp extends App {

  type Mat = DenseMatrix[Double]

  trait Op {
    def forward: Mat

    def backward(dir: Mat): Unit
  }

  case class L2Distance(expected: Mat, actual: Op) extends Op {
    override def forward: Mat = {
      val diff = (expected - actual.forward).flatten()
      DenseMatrix.create(1, 1, Array(norm(diff)))
    }

    override def backward(dir: Mat): Unit =
      actual.backward(dir)

    def optimize(): Unit = {
      //supports vectors only...
      assert(expected.rows == 1)

      val dir = (actual.forward - expected).t
      this.backward(dir)
    }
  }

  case class Source(in: Mat) extends Op {
    override def forward: Mat = in

    override def backward(dir: Mat): Unit = Unit
  }

  case class Scale(m: Op, var w: Double) extends Op {
    override def forward: Mat = m.forward * w

    override def backward(dir: Mat): Unit = {
      m.backward(dir * w)

      val diff = 0.3 * (dir * m.forward)
      assert(diff.size == 1)
      w -= diff.data(0)
    }
  }

  case class Sum(l: Op, r: Op) extends Op {
    override def forward: Mat = l.forward + r.forward

    override def backward(dir: Mat): Unit = {
      l.backward(dir)
      r.backward(dir)
    }
  }

  case class Dot(l: Op, r: Op) extends Op {
    override def forward: Mat = l.forward.t * r.forward

    override def backward(dir: Mat): Unit = {
      l.backward(dir * r.forward.t)
      r.backward(dir * l.forward.t)
    }
  }

  def mat(d: Double*): DenseMatrix[Double] = DenseMatrix.create(d.length, 1, d.toArray)

  val s1 = Source(mat(1, 2))
  val s2 = Source(mat(2, 3))
  val s3 = Source(mat(-1, 1))

  val s1Scaled = Scale(s1, 1)
  val s2Scaled = Scale(s2, -1)
  val s3Scaled = Scale(s3, 1)
  val s12Sum = Sum(s1Scaled, s2Scaled)
  val s123Dot = Dot(s12Sum, s3Scaled)

  println(s"s1:\n${s1Scaled.forward}\n")
  println(s"s2:\n${s2Scaled.forward}\n")
  println(s"s3:\n${s3Scaled.forward}\n")
  println(s"s12:\n${s12Sum.forward}\n")
  println(s"s123:\n${s123Dot.forward}\n")

  for (i <- 0 to 30) {
    L2Distance(mat(0.5), s123Dot).optimize()
    println(s123Dot.forward)
  }

}


