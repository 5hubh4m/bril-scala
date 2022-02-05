package bril.lang

import bril.lang.BrilAST._
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
      case obj@JsObject(m) if m.contains("label") => obj.getFields("label") match {
        case Seq(JsString(l)) => Label(l)
        case v => throw DeserializationException(f"Failed to parse label in $v.")
      }

      // try to parse an instruction
      case obj@JsObject(m) if m.contains("op") =>
        val args = m.get("args").map(_.convertTo[Seq[Ident]]).getOrElse(Seq())
        val labels = m.get("labels").map(_.convertTo[Seq[Ident]]).getOrElse(Seq())
        val funcs = m.get("funcs").map(_.convertTo[Seq[Ident]]).getOrElse(Seq())
        val typ = m.get("type").map(_.convertTo[Type])
        val dest = m.get("dest").map(_.convertTo[Ident])

        // either both type and dest are defined or both are left blank
        if (typ.isDefined != dest.isDefined)
          throw DeserializationException(f"Failed to parse instruction type and destination in $obj.")

        // make sure value is defined in a const instruction
        if (m("op") == JsString("const") && !m.contains("value"))
          throw DeserializationException(f"Failed to parse const instruction in $obj.")

        m("op") -> args -> labels -> funcs match {
          // basic ops
          case JsString("nop") -> _ -> _ -> _ => NoOp
          case JsString("print") -> Seq(s) -> _ -> _ => Print(s)
          case JsString("id") -> Seq(s) ->  _  -> _ => Id(dest, typ, s)
          case JsString("const") -> _ -> _ -> _ => Const(dest, typ, m("value").convertTo[Value])

          // control flow ops
          case JsString("jmp") -> _ -> Seq(l) -> _ => Jmp(l)
          case JsString("ret") -> a -> _ -> _ => Ret(a.headOption)
          case JsString("call") -> a -> _ -> Seq(f) => Call(dest, typ, f, a)
          case JsString("br") -> Seq(a) -> Seq(t, f) -> _ => Br(a, t, f)

          // memory ops
          case JsString("free") -> Seq(a) -> _ -> _ => Free(a)
          case JsString("load") -> Seq(a) -> _ -> _ => Load(dest, typ, a)
          case JsString("store") -> Seq(a, b) -> _ -> _ => Store(a, b)
          case JsString("alloc") -> Seq(a) -> _ -> _ if typ.forall(_.isInstanceOf[PtrType]) => Alloc(dest, typ, a)
          case JsString("ptradd") -> Seq(a, b) -> _ -> _ if typ.forall(_.isInstanceOf[PtrType]) => BinOp(PtrAdd, dest, typ, a, b)

          // arithmetic operations
          case JsString("add") -> Seq(x, y) -> _ -> _ if typ.forall(_ == IntType) => BinOp(Add, dest, typ, x, y)
          case JsString("sub") -> Seq(x, y) -> _ -> _ if typ.forall(_ == IntType) => BinOp(Sub, dest, typ, x, y)
          case JsString("mul") -> Seq(x, y) -> _ -> _ if typ.forall(_ == IntType) => BinOp(Mul, dest, typ, x, y)
          case JsString("div") -> Seq(x, y) -> _ -> _ if typ.forall(_ == IntType) => BinOp(Div, dest, typ, x, y)
          case JsString("fadd") -> Seq(x, y) -> _ -> _ if typ.forall(_ == FloatType) => BinOp(FAdd, dest, typ, x, y)
          case JsString("fdiv") -> Seq(x, y) -> _ -> _ if typ.forall(_ == FloatType) => BinOp(FDiv, dest, typ, x, y)
          case JsString("fmul") -> Seq(x, y) -> _ -> _ if typ.forall(_ == FloatType) => BinOp(FMul, dest, typ, x, y)
          case JsString("fsub") -> Seq(x, y) -> _ -> _ if typ.forall(_ == FloatType) => BinOp(FSub, dest, typ, x, y)

          // comparison ops
          case JsString("lt") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(LT, dest, typ, x, y)
          case JsString("gt") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(GT, dest, typ, x, y)
          case JsString("ge") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(GE, dest, typ, x, y)
          case JsString("le") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(LE, dest, typ, x, y)
          case JsString("eq") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(EQ, dest, typ, x, y)

          // boolean ops
          case JsString("or") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(Or, dest, typ, x, y)
          case JsString("and") -> Seq(x, y) -> _ -> _ if typ.forall(_ == BoolType) => BinOp(And, dest, typ, x, y)
          case JsString("not") -> Seq(x) -> _ -> _ if typ.forall(_ == BoolType) => UnOp(Not, dest, typ, x)

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

    def write(instr: Instruction): JsValue = instr match {
      // core language ops
      case Label(l) => JsObject("label" -> JsString(l))
      case Const(None, None, v) => JsObject("op" -> JsString("const"), "value" -> v.toJson)
      case Const(Some(d), Some(t), v) => JsObject("op" -> JsString("const"), "value" -> v.toJson, "dest" -> d.toJson, "type" -> t.toJson)
      case UnOp(op, None, None, x) => JsObject("op" -> op.toJson, "args" -> JsArray(x.toJson))
      case UnOp(op, Some(d), Some(t), x) => JsObject("op" -> op.toJson, "args" -> JsArray(x.toJson), "dest" -> d.toJson, "type" -> t.toJson)
      case BinOp(op, None, None, x, y) => JsObject("op" -> op.toJson, "args" -> JsArray(x.toJson, y.toJson))
      case BinOp(op, Some(d), Some(t), x, y) => JsObject("op" -> op.toJson, "dest" -> d.toJson, "type" -> t.toJson, "args" -> JsArray(x.toJson, y.toJson))
      case Jmp(l) => JsObject("op" -> JsString("jmp"), "labels" -> JsArray(l.toJson))
      case Br(c, t, f) => JsObject("op" -> JsString("br"), "args" -> JsArray(c.toJson), "labels" -> JsArray(t.toJson, f.toJson))
      case Call(None, None, f, Seq()) => JsObject("op" -> JsString("call"), "funcs" -> JsArray(f.toJson))
      case Call(None, None, f, as) => JsObject("op" -> JsString("call"), "funcs" -> JsArray(f.toJson), "args" -> as.toJson)
      case Call(Some(d), Some(t), f, as) => JsObject("op" -> JsString("call"), "dest" -> d.toJson, "type" -> t.toJson, "funcs" -> JsArray(f.toJson), "args" -> as.toJson)
      case Ret(None) => JsObject("op" -> JsString("ret"))
      case Ret(Some(s)) => JsObject("op" -> JsString("ret"), "args" -> JsArray(s.toJson))
      case Id(None, None, s) => JsObject("op" -> JsString("id"), "args" -> JsArray(s.toJson))
      case Id(Some(d), Some(t), s) => JsObject("op" -> JsString("id"), "args" -> JsArray(s.toJson), "dest" -> d.toJson, "type" -> t.toJson)
      case Print(s) => JsObject("op" -> JsString("print"), "args" -> JsArray(s.toJson))
      case NoOp => JsObject("op" -> JsString("nop"))
        
      // memory extension ops
      case Store(l, s) => JsObject("op" -> JsString("store"), "args" -> JsArray(l.toJson, s.toJson))
      case Alloc(None, None, s) => JsObject("op" -> JsString("alloc"), "args" -> JsArray(s.toJson))
      case Alloc(Some(d), Some(t), s) => JsObject("op" -> JsString("alloc"), "args" -> JsArray(s.toJson), "dest" -> d.toJson, "type" -> t.toJson)
      case Free(s) => JsObject("op" -> JsString("free"), "args" -> JsArray(s.toJson))
      case Load(None, None, s) => JsObject("op" -> JsString("load"), "args" -> JsArray(s.toJson))
      case Load(Some(d), Some(t), s) => JsObject("op" -> JsString("load"), "dest" -> d.toJson, "type" -> t.toJson, "args" -> JsArray(s.toJson))

      // SSA extension ops
      case Phi(Some(d), Some(t), ss, ls) => JsObject("op" -> JsString("phi"), "dest" -> d.toJson, "type" -> t.toJson, "args" -> ss.toJson, "labels" -> ls.toJson)
      case Phi(None, None, ss, ls) => JsObject("op" -> JsString("phi"), "args" -> ss.toJson, "labels" -> ls.toJson)

      // speculative execution ops
      case Speculate => JsObject("op" -> JsString("speculate"))
      case Commit => JsObject("op" -> JsString("commit"))
      case Guard(s, l) => JsObject("op" -> JsString("guard"), "labels" -> JsArray(l.toJson), "args" -> JsArray(s.toJson))

      // if we have received an ill-formed object then we throw
      case _ => throw new RuntimeException("Invalid object passed to serialise.")
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
        func.typ.map("type" -> _.toJson) ++
        (if (func.args.nonEmpty) Seq("args" -> func.args.toJson) else Seq())

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
