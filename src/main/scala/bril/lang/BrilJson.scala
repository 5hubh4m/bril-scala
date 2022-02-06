package bril.lang

import bril.lang.BrilAst._
import spray.json._

import scala.language.postfixOps

/**
 * Define the functions that can convert a
 * Bril AST back into JSON.
 */
case object BrilJson extends DefaultJsonProtocol {

  /**
   * Convert [[OpType]] to JSON.
   */
  implicit object OpJsonWriter extends JsonWriter[OpType] {
    def write(op: OpType): JsValue = JsString(op.toString.toLowerCase)
  }

  /**
   * Convert [[Type]] to and from a JSON value.
   */
  implicit object TypeJsonFormat extends JsonFormat[Type] {
    def read(json: JsValue): Type = json match {
      case JsString("int") => IntType
      case JsString("bool") => BoolType
      case JsString("float") => FloatType
      case JsObject(m) if m.contains("ptr") => PtrType(read(m("ptr")))
      case v => throw DeserializationException(f"Failed to parse type in $v.")
    }

    def write(typ: Type): JsValue = typ match {
      case IntType => JsString("int")
      case BoolType => JsString("bool")
      case FloatType => JsString("float")
      case PtrType(t) => JsObject("ptr" -> write(t))
    }
  }

  /**
   * Convert [[Argument]] to and from a JSON value.
   */
  implicit object ArgumentJsonFormat extends JsonFormat[Argument] {
    def read(json: JsValue): Argument = json.asJsObject.getFields("name", "type") match {
      case Seq(JsString(name), typ) => Argument(name, typ.convertTo[Type])
      case v => throw DeserializationException(f"Failed to parse argument in $v.")
    }

    def write(arg: Argument): JsValue = JsObject("name" -> JsString(arg.name), "type" -> arg.typ.toJson)
  }

  /**
   * Convert [[Value]] to and from a JSON value.
   */
  implicit object ValueJsonFormat extends JsonFormat[Value] {
    def read(json: JsValue): Value = json match {
      case JsNumber(value) => NumericValue(value)
      case JsBoolean(bool) => BoolValue(bool)
      case v => throw DeserializationException(f"Failed to parse value in $v.")
    }

    def write(value: Value): JsValue = value match {
      case NumericValue(n) => JsNumber(n)
      case BoolValue(b) => JsBoolean(b)
    }
  }

  /**
   * Convert [[Instruction]] to and from a JSON value.
   */
  implicit object InstructionJsonFormat extends JsonFormat[Instruction] {
    def read(json: JsValue): Instruction = json match {
      // try to parse a label
      case JsObject(m) if m.contains("label") => m("label") match {
        case JsString(l) => Label(l)
        case v => throw DeserializationException(f"Failed to parse label in $v.")
      }

      // try to parse an instruction
      case JsObject(m) if m.contains("op") =>
        // parse the various arguments and labels from the object
        val args = m.get("args").map(_.convertTo[Seq[Ident]]).getOrElse(Seq())
        val labels = m.get("labels").map(_.convertTo[Seq[Ident]]).getOrElse(Seq())
        val funcs = m.get("funcs").map(_.convertTo[Seq[Ident]]).getOrElse(Seq())
        val typ = m.get("type").map(_.convertTo[Type])
        val dest = m.get("dest").map(_.convertTo[Ident])

        // parse according to the op
        m("op") -> args -> labels -> funcs match {
          // basic ops
          case JsString("nop") -> _ -> _ -> _ => NoOp
          case JsString("print") -> Seq(s) -> _ -> _ => Print(s)
          case JsString("id") -> Seq(s) ->  _  -> _ => Id(dest, typ, s)
          case JsString("const") -> _ -> _ -> _ if m.contains("value") => Const(dest, typ, m("value").convertTo[Value])

          // control flow ops
          case JsString("jmp") -> _ -> Seq(l) -> _ => Jmp(l)
          case JsString("ret") -> a -> _ -> _ => Ret(a.headOption)
          case JsString("call") -> a -> _ -> Seq(f) => Call(dest, typ, f, a)
          case JsString("br") -> Seq(a) -> Seq(t, f) -> _ => Br(a, t, f)

          // memory ops
          case JsString("free") -> Seq(a) -> _ -> _ => Free(a)
          case JsString("load") -> Seq(a) -> _ -> _ => Load(dest, typ, a)
          case JsString("store") -> Seq(a, b) -> _ -> _ => Store(a, b)
          case JsString("alloc") -> Seq(a) -> _ -> _ => Alloc(dest, typ, a)
          case JsString("ptradd") -> Seq(a, b) -> _ -> _ => BinOp(PtrAdd, dest, typ, a, b)

          // arithmetic operations
          case JsString("add") -> Seq(x, y) -> _ -> _ => BinOp(Add, dest, typ, x, y)
          case JsString("sub") -> Seq(x, y) -> _ -> _ => BinOp(Sub, dest, typ, x, y)
          case JsString("mul") -> Seq(x, y) -> _ -> _ => BinOp(Mul, dest, typ, x, y)
          case JsString("div") -> Seq(x, y) -> _ -> _ => BinOp(Div, dest, typ, x, y)

          // floating point operations
          case JsString("fadd") -> Seq(x, y) -> _ -> _ => BinOp(FAdd, dest, typ, x, y)
          case JsString("fdiv") -> Seq(x, y) -> _ -> _ => BinOp(FDiv, dest, typ, x, y)
          case JsString("fmul") -> Seq(x, y) -> _ -> _ => BinOp(FMul, dest, typ, x, y)
          case JsString("fsub") -> Seq(x, y) -> _ -> _ => BinOp(FSub, dest, typ, x, y)

          // comparison ops
          case JsString("lt") -> Seq(x, y) -> _ -> _ => BinOp(LT, dest, typ, x, y)
          case JsString("gt") -> Seq(x, y) -> _ -> _ => BinOp(GT, dest, typ, x, y)
          case JsString("ge") -> Seq(x, y) -> _ -> _ => BinOp(GE, dest, typ, x, y)
          case JsString("le") -> Seq(x, y) -> _ -> _ => BinOp(LE, dest, typ, x, y)
          case JsString("eq") -> Seq(x, y) -> _ -> _ => BinOp(EQ, dest, typ, x, y)
          case JsString("ne") -> Seq(x, y) -> _ -> _ => BinOp(NE, dest, typ, x, y)

          // floating point comparison ops
          case JsString("flt") -> Seq(x, y) -> _ -> _ => BinOp(FLT, dest, typ, x, y)
          case JsString("fgt") -> Seq(x, y) -> _ -> _ => BinOp(FGT, dest, typ, x, y)
          case JsString("fge") -> Seq(x, y) -> _ -> _ => BinOp(FGE, dest, typ, x, y)
          case JsString("fle") -> Seq(x, y) -> _ -> _ => BinOp(FLE, dest, typ, x, y)
          case JsString("feq") -> Seq(x, y) -> _ -> _ => BinOp(FEQ, dest, typ, x, y)
          case JsString("fne") -> Seq(x, y) -> _ -> _ => BinOp(FNE, dest, typ, x, y)

          // boolean ops
          case JsString("or") -> Seq(x, y) -> _ -> _ => BinOp(Or, dest, typ, x, y)
          case JsString("and") -> Seq(x, y) -> _ -> _ => BinOp(And, dest, typ, x, y)
          case JsString("not") -> Seq(x) -> _ -> _ => UnOp(Not, dest, typ, x)

          // phi instruction
          case JsString("phi") -> as -> ls -> _ if as.size == ls.size => Phi(dest, typ, as, ls)

          // speculative execution instructions
          case JsString("guard") -> Seq(a) -> Seq(l) -> _ => Guard(a, l)
          case JsString("speculate") -> _ -> _ -> _ => Speculate
          case JsString("commit") -> _ -> _ -> _ => Commit

          case v -> _ -> _ -> _ => throw DeserializationException(f"Failed to parse instruction in $v.")
        }

      case v => throw DeserializationException(f"Failed to parse instruction in $v.")
    }

    def write(instr: Instruction): JsValue = {
      // create the values from the available fields
      val funcs = if (instr.funcs.nonEmpty) Seq("funcs" -> instr.funcs.toJson) else Seq()
      val args = if (instr.args.nonEmpty) Seq("args" -> instr.args.toJson) else Seq()
      val labels = if (instr.labels.nonEmpty) Seq("labels" -> instr.labels.toJson) else Seq()
      val dest = Seq(instr).collect({ case v: ValueOp if v.dest.isDefined => "dest" -> v.dest.get.toJson })
      val typ = Seq(instr).collect({ case v: ValueOp if v.typ.isDefined => "type" -> v.typ.get.toJson })

      // get the fields concatenated according to per-op values
      val fields = funcs ++ args ++ labels ++ dest ++ typ ++ (instr match {
        // core language ops
        case Label(l) => Seq("label" -> JsString(l))
        case Const(_, _, v) => Seq("op" -> JsString("const"), "value" -> v.toJson)
        case UnOp(op, _, _, _) => Seq("op" -> op.toJson)
        case BinOp(op, _, _, _, _) => Seq("op" -> op.toJson)
        case _: Jmp => Seq("op" -> JsString("jmp"))
        case _: Br => Seq("op" -> JsString("br"))
        case _: Call => Seq("op" -> JsString("call"))
        case _: Ret => Seq("op" -> JsString("ret"))
        case _: Id => Seq("op" -> JsString("id"))
        case _: Print => Seq("op" -> JsString("print"))
        case NoOp => Seq("op" -> JsString("nop"))

        // memory extension ops
        case _: Store => Seq("op" -> JsString("store"))
        case _: Alloc => Seq("op" -> JsString("alloc"))
        case _: Free => Seq("op" -> JsString("free"))
        case _: Load => Seq("op" -> JsString("load"))

        // SSA extension ops
        case _: Phi => Seq("op" -> JsString("phi"))

        // speculative execution ops
        case Speculate => Seq("op" -> JsString("speculate"))
        case Commit => Seq("op" -> JsString("commit"))
        case _: Guard => Seq("op" -> JsString("guard"))
      })

      // create the JSON object
      JsObject(fields: _*)
    }
  }

  /**
   * Convert [[Function]] to and from a JSON value.
   */
  implicit object FunctionJsonFormat extends JsonFormat[Function] {
    def read(json: JsValue): Function = json match {
      case obj@JsObject(m) => obj.getFields("name", "instrs") match {
        case Seq(JsString(name), instrs) =>
          // try to parse the args and type of the function if it exists
          val args = m.get("args").map(_.convertTo[Seq[Argument]]).getOrElse(Seq())
          val typ = m.get("type").map(_.convertTo[Type])

          // create the function
          Function(name, args, typ, instrs.convertTo[Seq[Instruction]])

        case v => throw DeserializationException(f"Failed to parse function in $v.")
      }

      case v => throw DeserializationException(f"Failed to parse function in $v.")
    }

    def write(func: Function): JsValue = {
      // populate all the fields
      val fields: Seq[(String, JsValue)] =
        Seq("name" -> func.name.toJson, "instrs" -> func.instrs.toJson) ++
        (if (func.args.nonEmpty) Seq("args" -> func.args.toJson) else Seq()) ++
        func.typ.map("type" -> _.toJson)

      // create the object
      JsObject(fields: _*)
    }

  }

  /**
   * Convert [[Program]] to and from a JSON value.
   */
  implicit object ProgramJsonFormat extends JsonFormat[Program] {
    def read(json: JsValue): Program = json.asJsObject.getFields("functions") match {
      case Seq(fs) => Program(fs.convertTo[Seq[Function]])
      case v => throw DeserializationException(f"Failed to parse program in $v.")
    }

    def write(program: Program): JsValue = JsObject("functions" -> program.functions.toJson)
  }

}
