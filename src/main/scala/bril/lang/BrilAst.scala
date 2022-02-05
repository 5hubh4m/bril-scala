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
  case object LT extends ComparisonOp with IntOp
  case object GT extends ComparisonOp with IntOp
  case object LE extends ComparisonOp with IntOp
  case object GE extends ComparisonOp with IntOp
  case object EQ extends ComparisonOp with IntOp
  case object FLT extends ComparisonOp with FloatOp
  case object FGT extends ComparisonOp with FloatOp
  case object FLE extends ComparisonOp with FloatOp
  case object FGE extends ComparisonOp with FloatOp
  case object FEQ extends ComparisonOp with FloatOp
  case object PtrAdd extends PtrOp

  /**
   * Different types of instructions have their own parameters.
   */
  trait Instruction {
    val dest: Option[Ident] = None
    val typ: Option[Type] = None
    val args: Seq[Ident] = Seq()
    val labels: Seq[Ident] = Seq()
    val funcs: Seq[Ident] = Seq()
  }
  trait ValueOp extends Instruction
  trait EffectOp extends Instruction
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
