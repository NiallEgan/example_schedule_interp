package func_playground

import scala.lms.common._
import scala.collection.mutable.Map

trait Ast extends Dsl {
  type Dim = String
  type Env = Map[Dim, Rep[Int]]

  class Func(f: (Rep[Int], Rep[Int]) => Rep[Int], dom: (Int, Int), vars: (Dim, Dim),
             inlined: Boolean) {
    /*  f reperesents the atual computation to be done
        dom is the domain over which the function should be computed
        vars is the variables used in the function - these aren't automatically inferred
        from f at the moment
    */

    // This is a rep of the array which will store the computed values of f.
    // TODO: In reality, the size of this buffer should depend on where the storage node
    // for this function is and the range of values the consumer uses,
    // and we shouldn't generate it at all if inlined
    val buffer: Rep[Array[Array[Int]]] = New2DArray[Int](dom._2, dom._1)

    private def eval(x: Rep[Int], y: Rep[Int]): Rep[Int] = {
        // If f isn't inlined, then any valid schedules will have already
        // computed f at x, y
        if (inlined) f(x, y)
        else buffer(y, x)
    }

    def apply(x: Rep[Int], y: Rep[Int]): Rep[Int] = eval(x, y)

    def upper_bound(v: Dim): Int = {
      // This returns the upper bound for a given variable (used for the loop bounds)
      if (v == vars._1) dom._1
      else if (v == vars._2) dom._2
      else throw new Exception("Unknown var")
    }

    def compute(env: Env): Rep[Int] = {
      // Looks up the vars in the environment and then evaluates f there
      val x = env(vars._1)
      val y = env(vars._2)
      f(x, y)
    }

    def store_in_buffer(v: Rep[Int], env: Env): Unit = {
      val x = env(vars._1)
      val y = env(vars._2)
      buffer(y, x) = v
    }
  }

  sealed trait LoopType
  case object Sequential extends LoopType
  case object Unrolled extends LoopType
  // TODO: Vectorized, parallelized

  sealed trait ScheduleNode
  case class LoopNode(variable: Dim, func: Func, children: List[ScheduleNode],
                      loop_type: LoopType) extends ScheduleNode
  case class ComputeNode(func: Func, children: List[ScheduleNode])
    extends ScheduleNode
  case class RootNode(children: List[ScheduleNode]) extends ScheduleNode

}

trait StagedScheduleInterpreter extends Ast  {
  def evalSched(node: ScheduleNode, env: Env): Rep[Unit] =
    node match {
      case LoopNode(variable, func, children, loop_type) =>
        loop_type match {
          /* Here we generate a for loop for 'variable', finding its upper bound from func.
             The value of the variable (a rep of an int or an actual int)
             gets added to the env */
          case Sequential =>
            for (i <- (0 until func.upper_bound(variable)): Rep[Range]) {
              for (child <- children) evalSched(child, env + (variable -> i))
            }
          case Unrolled =>
            for (i <- 0 until func.upper_bound(variable)) {
              for (child <- children) {
                evalSched(child, env + (variable -> i))
              }
            }
        }

      case ComputeNode(func, children) => {
        /* At a compute node, we compute f.func and store it.
          TODO: if we computed f in a previous iteration, we need to skip over
          it */
        val v: Rep[Int] = func.compute(env)
        func.store_in_buffer(v, env)
        for (child <- children) {
          evalSched(child, env)
        }
      }

      case RootNode(children) => {
        for (child <- children) evalSched(child, env)
      }

    }
}

trait Prog extends Ast with StagedScheduleInterpreter {
  def eval_im(useless: Rep[Unit]): Rep[Unit] = {
    // No storage nodes at the moment. This means that basically the default is
    // store_at_root()
    val x: Dim = "x"
    val y: Dim = "y"

    def f_func(x: Rep[Int], y: Rep[Int]): Rep[Int] = x + y
    val f: Func = new Func(f_func, (5, 5), (x, y), true)

    def g_func(x: Rep[Int], y: Rep[Int]): Rep[Int] =
      (f(x, y) + f(x, y+1) + f(y+1, x) + f(x+1, y+1)) / 4
    val g: Func = new Func(g_func, (4, 4), (x, y), false)

    // Default Halide schedule: f gets inlined */

    val cn: ComputeNode = new ComputeNode(g, List())
    val x_loop: LoopNode = new LoopNode(x, g, List(cn), Sequential)
    val y_loop: LoopNode = new LoopNode(y, g, List(x_loop), Sequential)
    val root_node = new RootNode(List(y_loop))
    evalSched(root_node, Map())

    /*
    // the f.compute_root() schedule
    val cn_f = new ComputeNode(f, List())
    val fx_loop = new LoopNode(x, f, List(cn_f), Sequential)
    val fy_loop = new LoopNode(y, f, List(fx_loop), Sequential)

    val cn_g = new ComputeNode(g, List())
    val gx_loop = new LoopNode(x, g, List(cn_g), Sequential)
    val gy_loop = new LoopNode(y, g, List(gx_loop), Sequential)

    val root_node = new RootNode(List(fy_loop, gy_loop))

    evalSched(root_node, Map())
    */
  }
}

object Func {
  def main(args: Array[String]): Unit = {
    val prog = new Prog with DslExp {
      self =>
        val codegen = new DslGenC {
          val IR: self.type = self
        }

      codegen.emitSource(eval_im, "f", new java.io.PrintWriter(System.out))
    }
  }
}
