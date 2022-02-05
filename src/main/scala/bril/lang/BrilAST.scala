package bril.lang

/**
 * Define the classes that form the AST of Bril.
 */
case object BrilAST {

  /**
   * Type of identifiers.
   */
  type Ident = String

  /**
   * Program is a list of functions.
   */
  case class Program(functions: Seq[Function])

  /**
   * There are three types: int, bool, float, and ptr of types.
   */
  trait Type
  case object IntType extends Type
  case object BoolType extends Type
  case object FloatType extends Type
  case class PtrType(typ: Type) extends Type

  /**
   * Values possible in a program.
   */
  trait Value
  case class BoolValue(value: Boolean) extends Value
  case class NumericValue(value: BigDecimal) extends Value

  /**
   * A function has a name, a list of arguments,
   * a type (if it returns something), and a
   * list of instructions.
   */
  case class Argument(name: Ident, typ: Type)
  case class Function(name: Ident,
                      args: Seq[Argument],
                      typ: Option[Type],
                      instrs: Seq[Instruction])

  /**
   * Different types of primitive ops.
   */
  trait OpType
  trait IntOp extends OpType
  trait FloatOp extends OpType
  trait BoolOp extends OpType
  trait PtrOp extends OpType
  trait ComparisonOp extends OpType
  case object Add extends IntOp
  case object Sub extends IntOp
  case object Div extends IntOp
  case object Mul extends IntOp
  case object FAdd extends FloatOp
  case object FSub extends FloatOp
  case object FDiv extends FloatOp
  case object FMul extends FloatOp
  case object Or extends BoolOp
  case object And extends BoolOp
  case object Not extends BoolOp
  case object LT extends ComparisonOp
  case object GT extends ComparisonOp
  case object LE extends ComparisonOp
  case object GE extends ComparisonOp
  case object EQ extends ComparisonOp
  case object PtrAdd extends PtrOp

  /**
   * Different types of instructions have their own parameters.
   */
  trait Instruction

  // instructions in the core language and the floating point extension.
  case class Label(label: Ident) extends Instruction
  case class Const(dest: Option[Ident], typ: Option[Type], value: Value) extends Instruction
  case class UnOp(op: OpType, dest: Option[Ident], typ: Option[Type], x: Ident) extends Instruction
  case class BinOp(op: OpType, dest: Option[Ident], typ: Option[Type], x: Ident, y: Ident) extends Instruction
  case class Jmp(label: Ident) extends Instruction
  case class Br(source: Ident, trueLabel: Ident, falseLabel: Ident) extends Instruction
  case class Call(dest: Option[Ident], typ: Option[Type], function: Ident, args: Seq[Ident] = Seq()) extends Instruction
  case class Ret(source: Option[Ident] = None) extends Instruction
  case class Id(dest: Option[Ident], typ: Option[Type], source: Ident) extends Instruction
  case class Print(source: Ident) extends Instruction
  case object NoOp extends Instruction

  // instructions in the memory extension.
  case class Store(location: Ident, source: Ident) extends Instruction
  case class Alloc(dest: Option[Ident], typ: Option[Type], size: Ident) extends Instruction
  case class Free(source: Ident) extends Instruction
  case class Load(dest: Option[Ident], typ: Option[Type], source: Ident) extends Instruction

  // instructions in the SSA extension.
  case class Phi(dest: Option[Ident], typ: Option[Type], sources: Seq[Ident], labels: Seq[Ident]) extends Instruction

  // instructions in the speculative execution extension.
  case object Speculate extends Instruction
  case object Commit extends Instruction
  case class Guard(source: Ident, label: Ident) extends Instruction

}
