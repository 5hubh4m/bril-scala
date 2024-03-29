package bril.lang

/**
 * Define the classes that form the AST of Bril.
 */
object BrilAst {

  /**
   * Type of identifiers.
   */
  type Ident = String

  /**
   * Program is a list of functions.
   */
  case class Program(functions: Seq[Function]) {
    lazy val prettyPrint: String = functions.map(_.prettyPrint).mkString
  }

  /**
   * There are three types: int, bool, float, and ptr of types.
   */
  sealed trait Type { val prettyPrint: String }
  case object IntType extends Type { lazy val prettyPrint: String = "int" }
  case object BoolType extends Type { lazy val prettyPrint: String = "bool" }
  case object FloatType extends Type { lazy val prettyPrint: String = "float" }
  case class PtrType(typ: Type) extends Type { lazy val prettyPrint: String = f"ptr<${typ.prettyPrint}>" }

  /**
   * Values possible in a program.
   */
  sealed trait Value { val prettyPrint: String }
  case class BoolValue(value: Boolean) extends Value { lazy val prettyPrint: String = value.toString }
  case class NumericValue(value: BigDecimal) extends Value { lazy val prettyPrint: String = value.toString }

  /**
   * A function has a name, a list of arguments,
   * a type (if it returns something), and a
   * list of instructions.
   */
  case class Argument(name: Ident, typ: Type) { lazy val prettyPrint: String = f"$name: ${typ.prettyPrint}" }
  case class Function(name: Ident,
                      args: Seq[Argument],
                      typ: Option[Type],
                      instrs: Seq[Instruction]) {
    lazy val prettyPrint: Ident = {
      val as = if (args.isEmpty) "" else f"(${args.map(_.prettyPrint).mkString(", ")})"
      val ts = if (typ.isEmpty) "" else f": ${typ.get.prettyPrint}"
      val is = if (instrs.isEmpty) "" else instrs.map(_.prettyPrint + "\n").mkString
      f"@$name$as$ts {\n$is}\n"
    }
  }

  /**
   * Different types of primitive ops.
   */
  sealed trait OpType { lazy val prettyPrint: String = toString.toLowerCase }
  sealed trait BinOpType extends OpType
  sealed trait UnOpType extends OpType
  sealed trait IntOpType extends OpType
  sealed trait FloatOpType extends OpType
  sealed trait BoolOpType extends OpType
  sealed trait PtrOpType extends OpType
  sealed trait ComparisonOpType extends OpType with BinOpType
  sealed trait CommutativeOpType extends OpType with BinOpType
  case object Add extends IntOpType with CommutativeOpType
  case object Mul extends IntOpType with CommutativeOpType
  case object Sub extends IntOpType with BinOpType
  case object Div extends IntOpType with BinOpType
  case object FAdd extends FloatOpType with CommutativeOpType
  case object FMul extends FloatOpType with CommutativeOpType
  case object FSub extends FloatOpType with BinOpType
  case object FDiv extends FloatOpType with BinOpType
  case object Or extends BoolOpType with CommutativeOpType
  case object And extends BoolOpType with CommutativeOpType
  case object Not extends BoolOpType with UnOpType
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
  case object PtrAdd extends PtrOpType with BinOpType

  /**
   * Represents an instruction. All instructions
   * have arguments, labels, and functions they
   * reference.
   */
  sealed trait Instruction {
    val args: Seq[Ident] = Seq.empty
    val labels: Seq[Ident] = Seq.empty
    val funcs: Seq[Ident] = Seq.empty

    /**
     * Modify the arguments of this instruction
     * according to the given function.
     */
    def mapArgs(f: Ident => Ident): Instruction = this

    /**
     * Modify the labels of this instruction
     * according to the given function.
     */
    def mapLabels(f: Ident => Ident): Instruction = this

    /**
     * Modify the functions of this instruction
     * according to the given function.
     */
    def mapFuncs(f: Ident => Ident): Instruction = this

    /**
     * Pretty print an instruction.
     */
    lazy val prettyPrint: Ident = {
      // get the labels, functions, and args
      lazy val as = if (args.isEmpty) "" else " " + args.mkString(" ")
      lazy val ls = if (labels.isEmpty) "" else " " + labels.map("." + _).mkString(" ")
      lazy val fs = if (funcs.isEmpty) "" else " " + funcs.map("@" + _).mkString(" ")

      // get the op
      lazy val op = this match {
        case UnOp (op, _, _, _) => op.prettyPrint
        case BinOp (op, _, _, _, _) => op.prettyPrint
        case _ => this.getClass.getSimpleName.filter(_!= '$').toLowerCase
      }

      // get the assignment
      lazy val assign = Some(this)
        .collect({ case ValueOp(_, _, _, Some(dest), Some(typ)) => f"$dest: ${typ.prettyPrint} = " })
        .getOrElse("")

      // return the string
      this match {
        case Label(l) => f".$l:"
        case Const(v, _, _) => f"  ${assign}const ${v.prettyPrint};"
        case _ => f"  $assign$op$fs$as$ls;"
      }
    }
  }

  /**
   * Value operation instructions optionally
   * have a destination and a type.
   */
  sealed trait ValueOp extends Instruction {
    val dest: Option[Ident]
    val typ: Option[Type]

    /**
     * Modify the destination of this instruction
     * according to the given function.
     */
    def mapDest(f: Option[Ident] => Option[Ident]): ValueOp = this

    /**
     * Modify the destination of this instruction
     * according to the given function.
     */
    def mapType(f: Option[Type] => Option[Type]): ValueOp = this
  }
  object ValueOp {
    def unapply(instr: ValueOp): Some[(Seq[Ident], Seq[Ident], Seq[Ident], Option[Ident], Option[Type])] =
      Some(instr.args, instr.labels, instr.funcs, instr.dest, instr.typ)
  }

  /**
   * Effect operations are ones with some side-effect.
   */
  sealed trait EffectOp extends Instruction
  object EffectOp {
    def unapply(instr: EffectOp): Some[(Seq[Ident], Seq[Ident], Seq[Ident])] =
      Some(instr.args, instr.labels, instr.funcs)
  }

  /**
   * Control operations change the control flow of execution.
   */
  sealed trait ControlOp extends EffectOp

  // instructions in the core language and the floating point extension.
  case class Label(label: Ident) extends Instruction

  case class Const(value: Value, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override def mapDest(f: Option[Ident] => Option[Ident]): Const = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Const = copy(typ = f(typ))
  }

  case class UnOp(op: UnOpType, x: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override val args: Seq[Ident] = Seq(x)
    override def mapArgs(f: Ident => Ident): UnOp = copy(x = f(x))
    override def mapDest(f: Option[Ident] => Option[Ident]): UnOp = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): UnOp = copy(typ = f(typ))
  }

  case class BinOp(op: BinOpType, x: Ident, y: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override val args: Seq[Ident] = Seq(x, y)
    override def mapArgs(f: Ident => Ident): BinOp = copy(x = f(x), y = f(y))
    override def mapDest(f: Option[Ident] => Option[Ident]): BinOp = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): BinOp = copy(typ = f(typ))
  }

  case class Jmp(label: Ident) extends ControlOp {
    override val labels: Seq[Ident] = Seq(label)
    override def mapLabels(f: Ident => Ident): Jmp = copy(label = f(label))
  }

  case class Br(source: Ident, trueLabel: Ident, falseLabel: Ident) extends ControlOp {
    override val args: Seq[Ident] = Seq(source)
    override val labels: Seq[Ident] = Seq(trueLabel, falseLabel)
    override def mapArgs(f: Ident => Ident): Br = copy(source = f(source))
    override def mapLabels(f: Ident => Ident): Br = copy(trueLabel = f(trueLabel), falseLabel = f(falseLabel))
  }

  case class Call(function: Ident, override val args: Seq[Ident] = Seq.empty, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp with ControlOp {
    override val funcs: Seq[Ident] = Seq(function)
    override def mapArgs(f: Ident => Ident): Call = copy(args = args.map(f))
    override def mapFuncs(f: Ident => Ident): Call = copy(function = f(function))
    override def mapDest(f: Option[Ident] => Option[Ident]): Call = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Call = copy(typ = f(typ))
  }

  case object Ret extends ControlOp

  case class Ret(source: Ident) extends ControlOp {
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Ret = copy(source = f(source))
  }

  case class Id(source: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Id = copy(source = f(source))
    override def mapDest(f: Option[Ident] => Option[Ident]): Id = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Id = copy(typ = f(typ))
  }

  case class Print(source: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Print = copy(source = f(source))
  }

  case object Nop extends EffectOp

  // instructions in the memory extension.
  case class Store(location: Ident, source: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(location, source)
    override def mapArgs(f: Ident => Ident): Store = copy(source = f(source), location = f(location))
  }

  case class Alloc(size: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp with EffectOp {
    override val args: Seq[Ident] = Seq(size)
    override def mapArgs(f: Ident => Ident): Alloc = copy(size = f(size))
    override def mapDest(f: Option[Ident] => Option[Ident]): Alloc = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Alloc = copy(typ = f(typ))
  }

  case class Free(source: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Free = copy(source = f(source))
  }

  case class Load(source: Ident, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    override val args: Seq[Ident] = Seq(source)
    override def mapArgs(f: Ident => Ident): Load = copy(source = f(source))
    override def mapDest(f: Option[Ident] => Option[Ident]): Load = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Load = copy(typ = f(typ))
  }

  // instructions in the SSA extension.
  case class Phi(override val args: Seq[Ident] = Seq.empty, override val labels: Seq[Ident] = Seq.empty, dest: Option[Ident] = None, typ: Option[Type] = None) extends ValueOp {
    assert(args.length == labels.length, "args and labels have different length for a phi instruction.")
    override def mapArgs(f: Ident => Ident): Phi = copy(args = args.map(f))
    override def mapLabels(f: Ident => Ident): Phi = copy(labels = labels.map(f))
    override def mapDest(f: Option[Ident] => Option[Ident]): Phi = copy(dest = f(dest))
    override def mapType(f: Option[Type] => Option[Type]): Phi = copy(typ = f(typ))
  }

  // instructions in the speculative execution extension.
  case object Speculate extends EffectOp

  case object Commit extends EffectOp

  case class Guard(source: Ident, label: Ident) extends EffectOp {
    override val args: Seq[Ident] = Seq(source)
    override val labels: Seq[Ident] = Seq(label)
    override def mapArgs(f: Ident => Ident): Guard = copy(source = f(source))
    override def mapLabels(f: Ident => Ident): Guard = copy(label = f(label))
  }

}
