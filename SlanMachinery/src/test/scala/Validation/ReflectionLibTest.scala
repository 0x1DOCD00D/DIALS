package Validation

import GenericDefinitions.Keywords.{dispatch, received, resource}
import GenericDefinitions.PatternMatch4Messages.onEventRule
import Validation.ReflectionLib.ASTPrinter.printAST
import Validation.ReflectionLib.IdentInspector.inspect

object ReflectionLibTest extends App {

  // Function to test inspectExec
  def testInspectExec(): Unit = {
    def sampleFunction(): Int = {
      val x = 10
      val y = x * 2
      println(y)
      y + 5
    }

    println("---- Testing inspectExec ----")
    println(printAST(sampleFunction()))
    println("\n")
    println(inspect(sampleFunction()))
  }

  // Function to test inspectBlock
  def testInspectBlock(): Unit = {
    println("---- Testing inspectBlock ----")
    println (printAST {
      val a = 5
      val b = 10
      val c = a + b
      println(s"Sum: $c")
      val k = b==1 && a == 4
//      write and return a partial function
      val ab = (a: Any) => println(a)
      ab

    })
    println("\n")
  }

  def testPartialFunc(): Unit = {
    val kf: PartialFunction[Any, Unit] = {
        case 1 => println("One")
        case 2 => println("Two")
        case _ => println("Other")
    }

    println(inspect(kf.apply(1)))

    println(inspect(kf))
  }

  // Function to test inspectCondition
  def testInspectCondition(): Unit = {
    println("---- Testing inspectCondition ----")

    var a = 5
    var b = 10
    val condition: () => Boolean = () => 5 == 6 && 1 == 2

    println(inspect(condition))

    // Change values and test again
    a = 10
    b = 10
    println(inspect(condition))
    println("\n")
  }

  // Run all tests
  testInspectExec()
  testInspectBlock()
  testInspectCondition()
  testPartialFunc()
}


