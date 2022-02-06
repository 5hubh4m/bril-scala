package bril.lang

/**
 * Define the classes that form the AST of Bril.
 */
case object BrilAst {

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
  trait Value { val asBool: Boolean; val asNum: BigDecimal }
  case class BoolValue(value: Boolean) extends Value {
    val asBool: Boolean = value
    override val asNum: BigDecimal = if (value) 1 else 0
  }
  case class NumericValue(value: BigDecimal) extends Value {
    val asBool: Boolean = value != 0
    val asNum: BigDecimal = value
  }

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
  trait IntOpType extends OpType
  trait FloatOpType extends OpType
  trait BoolOpType extends OpType
  trait PtrOpType extends OpType
  trait ComparisonOpType extends OpType
  trait CommutativeOpType extends OpType
  case object Add extends IntOpType with CommutativeOpType
  case object Mul extends IntOpType with CommutativeOpType
  case object Sub extends IntOpType
  case object Div extends IntOpType
  case object FAdd extends FloatOpType with CommutativeOpType
  case object FMul extends FloatOpType with CommutativeOpType
  case object FSub extends FloatOpType
  case object FDiv extends FloatOpType
  case object Or extends BoolOpType with CommutativeOpType
  case object And extends BoolOpType with CommutativeOpType
  case object Not extends BoolOpType
  case object LT extends ComparisonOpType with IntOpType
  case object GT extends ComparisonOpType with IntOpType
  case object LE extends ComparisonOpType with IntOpType
  case object GE extends ComparisonOpType with IntOpType
  case object EQ extends ComparisonOpType with IntOpType with CommutativeOpType
  case object FLT extends ComparisonOpType with FloatOpType
  case object FGT extends ComparisonOpType with FloatOpType
  case object FLE extends ComparisonOpType with FloatOpType
  case object FGE extends ComparisonOpType with FloatOpType
  case object FEQ extends ComparisonOpType with FloatOpType with CommutativeOpType
  case object PtrAdd extends PtrOpType

  /**
   * Represents an instruction. All instructions
   * have arguments, labels, and functions they
   * reference.
   */
  trait Instruction {
    val args: Seq[Ident] = Seq()
    val labels: Seq[Ident] = Seq()
    val funcs: Seq[Ident] = Seq()
  }
  object Instruction{
    def unapply(instr: Instruction): Option[(Seq[Ident], Seq[Ident], Seq[Ident])] =
      Some(instr.args, instr.labels, instr.funcs)
  }

  /**
   * Value operation instructions optionally
   * have a destination and a type.
   */
  trait ValueOp extends Instruction {
    val dest: Option[Ident] = None
    val typ: Option[Type] = None
  }
  object ValueOp{
    def unapply(instr: ValueOp): Option[(Option[Ident], Option[Type], Seq[Ident], Seq[Ident], Seq[Ident])] =
      Some(instr.dest, instr.typ, instr.args, instr.labels, instr.funcs)
  }

  /**
   * Effect operations are ones with some side-effect.
   */
  trait EffectOp extends Instruction

  /**
   * Control operations change the control flow of execution.
   */
  trait ControlOp extends EffectOp

  // instructions in the core language and the floating point extension.
  case class Label(label: Ident) extends Instruction

  case class Const(override val dest: Option[Ident], override val typ: Option[Type], value: Value) extends ValueOp

  case class UnOp(op: OpType, override val dest: Option[Ident], override val typ: Option[Type], x: Ident) extends ValueOp {
    override val args: Seq[Ident] = Seq(x)
  }

  case class BinOp(op: OpType, override val dest: Option[Ident], override val typ: Option[Type], x: Ident, y: Ident) extends ValueOp {
    override val args: Seq[Ident] = Seq(x, y)
  }

  case class Jmp(label: Ident) extends ControlOp {
    override val labels: Seq[Ident] = Seq(label)
  }

  case class Br(source: Ident, trueLabel: Ident, falseLabel: Ident) extends ControlOp {
    override val args: Seq[Ident] = Seq(source)
    override val labels: Seq[Ident] = Seq(trueLabel, falseLabel)
  }

  case class Call(override val dest: Option[Ident], override val typ: Option[Type], function: Ident, override val args: Seq[Ident]) extends ValueOp with ControlOp {
    override val funcs: Seq[Ident] = Seq(function)
  }

  case class Ret(source: Option[Ident]) extends ControlOp {
    override val args: Seq[Ident] = source.toSeq
  }

  case class Id(override val dest: Option[Ident], override val typ: Option[Type], source: Ident) extends ValueOp {
    override val args: Seq[Ident] = Seq(source)
  }

  case class Print(source: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(source)
  }

  case object NoOp extends EffectOp

  // instructions in the memory extension.
  case class Store(location: Ident, source: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(location, source)
  }

  case class Alloc(override val dest: Option[Ident], override val typ: Option[Type], size: Ident) extends ValueOp with EffectOp {
    override val args: Seq[Ident] = Seq(size)
  }

  case class Free(source: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(source)
  }

  case class Load(override val dest: Option[Ident], override val typ: Option[Type], source: Ident) extends ValueOp {
    override val args: Seq[Ident] = Seq(source)
  }

  // instructions in the SSA extension.
  case class Phi(override val dest: Option[Ident], override val typ: Option[Type], override val args: Seq[Ident], override val labels: Seq[Ident]) extends ValueOp

  // instructions in the speculative execution extension.
  case object Speculate extends EffectOp

  case object Commit extends EffectOp

  case class Guard(source: Ident, label: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(source)
    override val labels: Seq[Ident] = Seq(label)
  }

}
