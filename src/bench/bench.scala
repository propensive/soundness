package gossamer

import larceny.*
import probably.*

case class Group(persons: List[Person])
case class Person(name: String, age: Int)

object Benchmarks extends Suite(t"Gossamer Benchmarks"):
  def run(): Unit =
    suite(t"Compile performance"):
      test(t"Resolve a Show instance"):
        deferCompilation:
          import gossamer.*
          Group(List(Person("Jack", 30))).show
      .benchmark(warmup = 30000L, duration = 30000L)
      
      test(t"Resolve a Debug instance"):
        deferCompilation:
          import gossamer.*
          Group(List(Person("Jack", 30))).debug
      .benchmark(warmup = 30000L, duration = 30000L)
      
