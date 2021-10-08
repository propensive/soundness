package pyroclastic

// case class Env[-T](map: Map[Point[?, ?], Any])

// object Point:
//   def apply[T](value: T): Point[T, Nothing] = new Point(_ => value)

// case class Point[+T, D](gen: Env[?] => T):

//   def compute(env: Env[?]): Env[this.type] = Env(env.map.updated(this, gen(env)))

//   def gmap[S](fn: Env[T] ?=> S): Point[S, ?] = fn(using Env[T](values))

//   def apply()(using env: Env[T]): T = env.map(this).asInstanceOf[T]

//   def &(that: Point[?, ?]): Juncture = Juncture(this, that)

// case class Juncture(points: Point[?, ?]*):
//   def &(that: Point[?, ?]): Juncture = Juncture((points :+ that)*)



// val source: Point[String, Nothing] = Point("hello world")

// val getLength: Point[Int, source.type] = source.gmap { source().length }
// val addition: Point[Int, Nothing] = Point(18)

// val sum: Point[Int, getLength.type | addition.type] =
//   (getLength & addition).gmap { getLength() + addition() }


// case class Env[D](values: Map[D, Any]):
//   def apply[D <: Point[?], T](point: D): T = values(point)

// case class Point[+D <: Point[?, ?], +T](dependencies: Set[_ <: D], fn: Env[D] ?=> T):
//   def &[D2 <: Point[?], T2](point: Point[D2, T2]) = Point[D | D2, T | T2](dependencies ++ point.dependencies)
//   def apply(): T = ???

//   def gmap[S](fn: Env[D] ?=> S): Point[D | this.type] = dependencies.foldLeft(Env(map: 



// object Point:
//   def apply[T](fn: => T): Point[Nothing, T] = Point[Nothing, T](Set(), { (env: Env(Map())) ?=> fn })

// case class Person(name: String, age: String)
// case class Company(name: String)

// val person = Point(Person("Jon", 48))
// val company = Company(Company("Virtus Lab"))


import scala.concurrent.*

given ExecutionContext = ExecutionContext.global

// case class Env[-E <: Singleton](elements: Map[Point[?], Future[Any]])

// type Context = Map[Point[_], Future[Any]]

// case class Flow[In <: Singleton, Out <: Singleton](flow: Context => Context):
//   def >>>[In2 <: Singleton, Out2 <: Singleton](next: Flow[In2, Out2])(using Out <:< In2): Flow[In, In & Out & Out2] =
//     Flow({ in => next.flow(flow(in)) })

// case class Point[T, D <: Singleton]():
//   def apply(value: T): Flow[Singleton, this.type] = Flow(_.updated(this, Future(value)))
//   def apply()(using Env[this.type]) = summon[Env[this.type]].elements(this).asInstanceOf[T]

//   def &[S](that: Point[S]): Point[this.type | that.type] = Point()

//   def update[S <: Singleton](newValue: T)(using Env[S]): Env[S & this.type] = Env(summon[Env[S]].elements.updated(this, Future(newValue)))

//   def propagate[S](fn: Env[D] ?=> S): Flow[this.type, D & this.type] = ???


//BEST VERSION YET
// case class Points[T, P <: Point[?]](points: Set[P]):
//   def map[U](point: BaseKey)(fn: Env[T] ?=> U): Flow[T, T] =
//     Flow(key, 

trait BaseKey:
  type Type
  type This

case class Points[In <: BaseKey & Singleton](points: List[BaseKey]):
  def map(key: BaseKey & Singleton)(action: Env[In] ?=> key.Type): Flow[In, In with key.type] = ???

case class Point[T]() extends BaseKey:
  type Type = T
  def &[S <: Singleton](that: Point[S]): Point[T & S] = Point()
  def apply()(using env: Env[this.type]): T = env.values(this).asInstanceOf[T]
  def of(value: => T): Flow[BaseKey & Singleton, this.type] = Flow(this, _.updated(this, value))
  def from[X <: BaseKey & Singleton](points: Points[X]) = points.map(this)

//   def from2[Q <: BaseKey](tuple: Tuple)(using Tuple.Union[tuple.type])(action: Env[Q] => Type) =
//     Points[And[tuple.type]](tuple.toList.asInstanceOf[List[BaseKey]]).map(this)(action)

  //def assuming(points: Point[?]*): From[Any, Tuple] =
    

case class From[Ix, T <: Tuple]()

case class Flow[In <: BaseKey & Singleton, Out <: BaseKey & Singleton](key: BaseKey, flow: Map[BaseKey, Any] => Map[BaseKey, Any]):
  def >>>[In2 <: BaseKey & Singleton, Out2 <: BaseKey & Singleton](next: Flow[In2, Out2])(using Out <:< In2) =
    Flow(next.key, flow.andThen(next.flow))

  

case class Env[+T <: BaseKey & Singleton](values: Map[BaseKey, Any])

object Example:
  case class Water(hot: Boolean):
    def boil(): Water = Water(true)
 
  case class Kettle()
  case class Milk()
  case class Tea()
  case class Coffee()
  case class Teabag()
  
  case class Cup[T](water: Water, milk: Milk, other: T)
  object Cup:
    case class Spillage() extends Exception("whoops")
    def pour[T](water: Water, milk: Milk, other: T): Cup[T] =
      if math.random < 0.9 then Cup(water, milk, other) else throw Spillage()
  
  val milk = Point[Milk]()
  val tea = Point[Tea]()
  val water = Point[Water]()
  
  val coldWater = Point[Water]()
  val getWater = coldWater.of(Water(false))
  
  val hotWater = Point[Water]()
  val boil = Points[coldWater.type & milk.type](List(coldWater, milk)).map(hotWater) {
    val m = milk()
    coldWater().boil()
  }

  val boil2 = hotWater.from3(coldWater, milk) { env =>
    env: Int
    val m = milk()
    val n = hotWater()
    coldWater().boil()
  }



// import language.implicitConversions

// sealed trait BaseKey { type Type }

// object Need:
//   implicit def toNeed[T <: BaseKey](t: T): Need[t.type] = Need(t)

// case class Need[-T <: BaseKey](field: T)

// object Field:
//   def apply[T](): Field[T] = new Field()

// class Field[T]() extends BaseKey:
//   type Type = T
//   def apply()(implicit env: Env[this.type]): T = env.fields(this).asInstanceOf[T]
  
//   def of(mkField: => T): Flow[BaseKey, this.type] = Flow(this, _.updated(this, Future(mkField)))

// def assuming[In <: BaseKey](xs: Need[In]*): GivenFields[In] = new GivenFields(xs.map(_.field))

// class GivenFields[In <: BaseKey](keys: Seq[BaseKey]):
//   def propagate(key: BaseKey)(action: Env[? <: In] => Future[key.Type]): Flow[In, In with key.type] =
//     new Flow(key, { in =>
//       Future.sequence(in.updated(key, keys.map { k => in(k).map(k -> _) })).flatMap { seq =>
//         action(Env(seq.toMap)).asInstanceOf[Future[Any]]
//       }
//     })
  
//   def provide(key: BaseKey)(action: Env[? <: In] => key.Type): Flow[In, In with key.type] =
//     new Flow(key, { in =>
//       Future.sequence(in.updated(key, keys.map { key => in(key).map(key -> _) })).flatMap { seq =>
//         Future(action(Env(seq.toMap))).asInstanceOf[Future[Any]]
//       }
//     })

// case class Env[+In <: BaseKey](fields: Map[BaseKey, Any])

// case class Flow[In <: BaseKey, Out <: BaseKey](key: BaseKey, flow: Map[BaseKey, Future[Any]] => Map[BaseKey, Future[Any]]):
  
//   def >>>[In2 <: BaseKey, Out2 <: BaseKey](next: Flow[In2, Out2])(using Out <:< In2): Flow[In, In & Out & Out2] =
//     new Flow(next.key, { in => next.flow(flow(in)) })
  
//   def apply[T](key: Field[T])(implicit ev: Out <:< key.type, ev2: BaseKey =:= In): Future[T] =
//     flow(Map())(key).asInstanceOf[Future[T]]