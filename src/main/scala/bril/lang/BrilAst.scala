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
  sealed trait Type
  case object IntType extends Type
  case object BoolType extends Type
  case object FloatType extends Type
  case class PtrType(typ: Type) extends Type

  /**
   * Values possible in a program.
   */
  sealed trait Value { val asBool: Boolean; val asNum: BigDecimal }
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
  sealed trait OpType
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
  sealed trait Instruction {
    type InstrType <: Instruction

    val args: Seq[Ident] = Seq.empty
    val labels: Seq[Ident] = Seq.empty
    val funcs: Seq[Ident] = Seq.empty

    /**
     * Modify the arguments of this instruction
     * according to the given function.
     */
    def mapArgs(f: Ident => Ident): InstrType = this.asInstanceOf[InstrType]

    /**
     * Modify the labels of this instruction
     * according to the given function.
     */
    def mapLabels(f: Ident => Ident): InstrType = this.asInstanceOf[InstrType]

    /**
     * Modify the functions of this instruction
     * according to the given function.
     */
    def mapFuncs(f: Ident => Ident): InstrType = this.asInstanceOf[InstrType]
  }
  object Instruction{
    def unapply(instr: Instruction): Option[(Seq[Ident], Seq[Ident], Seq[Ident])] =
      Some(instr.args, instr.labels, instr.funcs)
  }

  /**
   * Value operation instructions optionally
   * have a destination and a type.
   */
  sealed trait ValueOp extends Instruction {
    type ValType <: ValueOp
    val dest: Option[Ident]
    val typ: Option[Type]

    /**
     * Modify the destination of this instruction
     * according to the given function.
     */
    def mapDest(f: Option[Ident] => Option[Ident]): ValType = this.asInstanceOf[ValType]

    /**
     * Modify the destination of this instruction
     * according to the given function.
     */
    def mapType(f: Option[Type] => Option[Type]): ValType = this.asInstanceOf[ValType]
  }
  object ValueOp {
    def unapply(instr: ValueOp): Option[(Option[Ident], Option[Type], Seq[Ident], Seq[Ident], Seq[Ident])] =
      Some(instr.dest, instr.typ, instr.args, instr.labels, instr.funcs)
  }

  /**
   * Effect operations are ones with some side-effect.
   */
  sealed trait EffectOp extends Instruction

  /**
   * Control operations change the control flow of execution.
   */
  sealed trait ControlOp extends EffectOp

  // instructions in the core language and the floating point extension.
  case class Label(label: Ident) extends Instruction { override type InstrType = Label }

  case class Const(value: Value, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override type InstrType = Const
    override type ValType = Const
    override def mapDest(f: Option[Ident] => Option[Ident]): Const = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Const = copy(typ = f(typ))
  }

  case class UnOp(op: OpType, x: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override type InstrType = UnOp
    override type ValType = UnOp
    override val args: Seq[Ident] = Seq(x)
    override def mapArgs(f: Ident => Ident): UnOp = copy(x = f(x))
    override def mapDest(f: Option[Ident] => Option[Ident]): UnOp = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): UnOp = copy(typ = f(typ))
  }

  case class BinOp(op: OpType, x: Ident, y: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override type InstrType = BinOp
    override type ValType = BinOp
    override val args: Seq[Ident] = Seq(x, y)
    override def mapArgs(f: Ident => Ident): BinOp = copy(x = f(x), y = f(y))
    override def mapDest(f: Option[Ident] => Option[Ident]): BinOp = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): BinOp = copy(typ = f(typ))
  }

  case class Jmp(label: Ident) extends ControlOp {
    override type InstrType = Jmp
    override val labels: Seq[Ident] = Seq(label)
    override def mapLabels(f: Ident => Ident): Jmp = copy(label = f(label))
  }

  case class Br(source: Ident, trueLabel: Ident, falseLabel: Ident) extends ControlOp {
    override type InstrType = Br
    override val args: Seq[Ident] = Seq(source)
    override val labels: Seq[Ident] = Seq(trueLabel, falseLabel)
    override def mapArgs(f: Ident => Ident): Br = copy(source = f(source))
    override def mapLabels(f: Ident => Ident): Br = copy(trueLabel = f(trueLabel), falseLabel = f(falseLabel))
  }

  case class Call(function: Ident, override val args: Seq[Ident] = Seq.empty, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp with ControlOp {
    override type InstrType = Call
    override type ValType = Call
    override val funcs: Seq[Ident] = Seq(function)
    override def mapArgs(f: Ident => Ident): Call = copy(args = args.map(f))
    override def mapFuncs(f: Ident => Ident): Call = copy(function = f(function))
    override def mapDest(f: Option[Ident] => Option[Ident]): Call = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Call = copy(typ = f(typ))
  }

  case class Ret() extends ControlOp  { override type InstrType = Ret }

  case class Return(source: Ident) extends ControlOp {
    override type InstrType = Return
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Return = copy(source = f(source))
  }

  case class Id(source: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override type InstrType = Id
    override type ValType = Id
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Id = copy(source = f(source))
    override def mapDest(f: Option[Ident] => Option[Ident]): Id = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Id = copy(typ = f(typ))
  }

  case class Print(source: Ident) extends EffectOp {
    override type InstrType = Print
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Print = copy(source = f(source))
  }

  case class NoOp() extends EffectOp  { override type InstrType = NoOp }

  // instructions in the memory extension.
  case class Store(location: Ident, source: Ident) extends EffectOp {
    override type InstrType = Store
    override val args: Seq[Ident] = Seq(location, source)
    override def mapArgs(f: Ident => Ident): Store = copy(source = f(source), location = f(location))
  }

  case class Alloc(size: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp with EffectOp {
    override type InstrType = Alloc
    override type ValType = Alloc
    override val args: Seq[Ident] = Seq(size)
    override def mapArgs(f: Ident => Ident): Alloc = copy(size = f(size))
    override def mapDest(f: Option[Ident] => Option[Ident]): Alloc = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Alloc = copy(typ = f(typ))
  }

  case class Free(source: Ident) extends EffectOp {
    override type InstrType = Free
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Free = copy(source = f(source))
  }

  case class Load(source: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override type InstrType = Load
    override type ValType = Load
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Load = copy(source = f(source))
    override def mapDest(f: Option[Ident] => Option[Ident]): Load = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Load = copy(typ = f(typ))
  }

  // instructions in the SSA extension.
  case class Phi(override val args: Seq[Ident] = Seq.empty, override val labels: Seq[Ident] = Seq.empty, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    assert(args.length == labels.length, "args and labels have different length for a phi instruction.")
    override type InstrType = Phi
    override type ValType = Phi
    override def mapArgs(f: Ident => Ident): Phi = copy(args = args.map(f))
    override def mapLabels(f: Ident => Ident): Phi = copy(labels = labels.map(f))
    override def mapDest(f: Option[Ident] => Option[Ident]): Phi = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Phi = copy(typ = f(typ))
  }

  // instructions in the speculative execution extension.
  case class Speculate() extends EffectOp { override type InstrType = Speculate }

  case class Commit() extends EffectOp  { override type InstrType = Commit }

  case class Guard(source: Ident, label: Ident) extends EffectOp {
    override type InstrType = Guard
    override val args: Seq[Ident] = Seq(source)
    override val labels: Seq[Ident] = Seq(label)
    override def mapArgs(f: Ident => Ident): Guard = copy(source = f(source))
    override def mapLabels(f: Ident => Ident): Guard = copy(label = f(label))
  }

}
