// package gossamer

// trait Printer[ValueType]:
//   type WorkType
//   type ResultType
//   def append(current: WorkType, next: Text): WorkType
//   def result(value: WorkType): ResultType

// trait BufferPrinter[ValueType] extends Printer[ValueType]:
//   type WorkType = Unit
//   type ResultType = String
//   private val buffer: StringBuilder = StringBuilder()
//   def append(current: Unit, next: Text): Unit = buf.append(next.s)
//   def result(value: Unit): Unit