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
      case JsString(name) :: typ :: _ => Argument(name, typ.convertTo[Type])
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
        val args = m.get("args").map(_.convertTo[Seq[Ident]]).getOrElse(Seq.empty)
        val labels = m.get("labels").map(_.convertTo[Seq[Ident]]).getOrElse(Seq.empty)
        val funcs = m.get("funcs").map(_.convertTo[Seq[Ident]]).getOrElse(Seq.empty)

        // parse according to the op
        val instr = m("op") -> args -> labels -> funcs match {
          // basic ops
          case JsString("nop") -> _ -> _ -> _ => NoOp()
          case JsString("print") -> (s :: _) -> _ -> _ => Print(s)
          case JsString("id") -> (s :: _) ->  _  -> _ => Id(s)
          case JsString("const") -> _ -> _ -> _ if m.contains("value") => Const(m("value").convertTo[Value])

          // control flow ops
          case JsString("jmp") -> _ -> (l :: _) -> _ => Jmp(l)
          case JsString("ret") -> (a :: _) -> _ -> _ => Return(a)
          case JsString("ret") -> _ -> _ -> _ => Ret()
          case JsString("call") -> a -> _ -> (f :: _) => Call(f, a)
          case JsString("br") -> (a :: _) -> (t :: f :: _) -> _ => Br(a, t, f)

          // memory ops
          case JsString("free") -> (a :: _) -> _ -> _ => Free(a)
          case JsString("load") -> (a :: _) -> _ -> _ => Load(a)
          case JsString("store") -> (a :: b :: _) -> _ -> _ => Store(a, b)
          case JsString("alloc") -> (a :: _) -> _ -> _ => Alloc(a)
          case JsString("ptradd") -> (a :: b :: _) -> _ -> _ => BinOp(PtrAdd, a, b)

          // arithmetic operations
          case JsString("add") -> (x :: y :: _) -> _ -> _ => BinOp(Add, x, y)
          case JsString("sub") -> (x :: y :: _) -> _ -> _ => BinOp(Sub, x, y)
          case JsString("mul") -> (x :: y :: _) -> _ -> _ => BinOp(Mul, x, y)
          case JsString("div") -> (x :: y :: _) -> _ -> _ => BinOp(Div, x, y)

          // floating point operations
          case JsString("fadd") -> (x :: y :: _) -> _ -> _ => BinOp(FAdd, x, y)
          case JsString("fdiv") -> (x :: y :: _) -> _ -> _ => BinOp(FDiv, x, y)
          case JsString("fmul") -> (x :: y :: _) -> _ -> _ => BinOp(FMul, x, y)
          case JsString("fsub") -> (x :: y :: _) -> _ -> _ => BinOp(FSub, x, y)

          // comparison ops
          case JsString("lt") -> (x :: y :: _) -> _ -> _ => BinOp(LT, x, y)
          case JsString("gt") -> (x :: y :: _) -> _ -> _ => BinOp(GT, x, y)
          case JsString("ge") -> (x :: y :: _) -> _ -> _ => BinOp(GE, x, y)
          case JsString("le") -> (x :: y :: _) -> _ -> _ => BinOp(LE, x, y)
          case JsString("eq") -> (x :: y :: _) -> _ -> _ => BinOp(EQ, x, y)

          // floating point comparison ops
          case JsString("flt") -> (x :: y :: _) -> _ -> _ => BinOp(FLT, x, y)
          case JsString("fgt") -> (x :: y :: _) -> _ -> _ => BinOp(FGT, x, y)
          case JsString("fge") -> (x :: y :: _) -> _ -> _ => BinOp(FGE, x, y)
          case JsString("fle") -> (x :: y :: _) -> _ -> _ => BinOp(FLE, x, y)
          case JsString("feq") -> (x :: y :: _) -> _ -> _ => BinOp(FEQ, x, y)

          // boolean ops
          case JsString("or") -> (x :: y :: _) -> _ -> _ => BinOp(Or, x, y)
          case JsString("and") -> (x :: y :: _) -> _ -> _ => BinOp(And, x, y)
          case JsString("not") -> (x :: _) -> _ -> _ => UnOp(Not, x)

          // phi instruction
          case JsString("phi") -> as -> ls -> _ if as.size == ls.size => Phi(as, ls)

          // speculative execution instructions
          case JsString("guard") -> (a :: _) -> (l :: _) -> _ => Guard(a, l)
          case JsString("speculate") -> _ -> _ -> _ => Speculate()
          case JsString("commit") -> _ -> _ -> _ => Commit()

          case v -> _ -> _ -> _ => throw DeserializationException(f"Failed to parse instruction in $v.")
        }

        // set the destination and type if the instruction is a value operation
        val typ = m.get("type").map(_.convertTo[Type])
        val dest = m.get("dest").map(_.convertTo[Ident])
        Some(instr).collect({ case v: ValueOp => v.mapDest(_ => dest).mapType(_ => typ) }).getOrElse(instr)

      case v => throw DeserializationException(f"Failed to parse instruction in $v.")
    }

    def write(instr: Instruction): JsValue = {
      // create the values from the available fields
      val funcs = if (instr.funcs.nonEmpty) Seq("funcs" -> instr.funcs.toJson) else Seq.empty
      val args = if (instr.args.nonEmpty) Seq("args" -> instr.args.toJson) else Seq.empty
      val labels = if (instr.labels.nonEmpty) Seq("labels" -> instr.labels.toJson) else Seq.empty
      val dest = Seq(instr).collect({ case v: ValueOp if v.dest.isDefined => "dest" -> v.dest.get.toJson })
      val typ = Seq(instr).collect({ case v: ValueOp if v.typ.isDefined => "type" -> v.typ.get.toJson })

      // get the fields concatenated according to per-op values
      val fields = funcs ++ args ++ labels ++ dest ++ typ ++ (instr match {
        // core language ops
        case Label(l) => Seq("label" -> JsString(l))
        case Const(v, _, _) => Seq("op" -> JsString("const"), "value" -> v.toJson)
        case UnOp(op,  _, _, _) => Seq("op" -> op.toJson)
        case BinOp(op, _, _, _, _) => Seq("op" -> op.toJson)
        case _: Jmp => Seq("op" -> JsString("jmp"))
        case _: Br => Seq("op" -> JsString("br"))
        case _: Call => Seq("op" -> JsString("call"))
        case _: Ret => Seq("op" -> JsString("ret"))
        case _: Return => Seq("op" -> JsString("ret"))
        case _: Id => Seq("op" -> JsString("id"))
        case _: Print => Seq("op" -> JsString("print"))
        case _: NoOp => Seq("op" -> JsString("nop"))

        // memory extension ops
        case _: Store => Seq("op" -> JsString("store"))
        case _: Alloc => Seq("op" -> JsString("alloc"))
        case _: Free => Seq("op" -> JsString("free"))
        case _: Load => Seq("op" -> JsString("load"))

        // SSA extension ops
        case _: Phi => Seq("op" -> JsString("phi"))

        // speculative execution ops
        case _: Speculate => Seq("op" -> JsString("speculate"))
        case _: Commit => Seq("op" -> JsString("commit"))
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
        case JsString(name) :: instrs :: _ =>
          // try to parse the args and type of the function if it exists
          val args = m.get("args").map(_.convertTo[Seq[Argument]]).getOrElse(Seq.empty)
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
        (if (func.args.nonEmpty) Seq("args" -> func.args.toJson) else Seq.empty) ++
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
      case fs :: _ => Program(fs.convertTo[Seq[Function]])
      case v => throw DeserializationException(f"Failed to parse program in $v.")
    }

    def write(program: Program): JsValue = JsObject("functions" -> program.functions.toJson)
  }

}
