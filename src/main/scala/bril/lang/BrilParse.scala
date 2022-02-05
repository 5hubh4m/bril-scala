package bril.lang

import bril.lang.BrilAST._
import spray.json._

import scala.io.Source
import scala.util.{Random, Try}

/**
 * Define the classes that form the AST of Bril.
 */
case object BrilParse {

  /**
   * Parse a Bril program from stdin.
   */
  def readProgramFromStdin: Try[Program] =
    Try(jsonToBril(Source.stdin.iter.foldLeft("")({ case str -> c => str :+ c }).parseJson).get)

  /**
   * Convert a Bril JSON to an object of the
   * [[Program]] class.
   */
  def jsonToBril(json: JsValue): Option[Program] = json match {
    case JsObject(m) if m.contains("functions") => m("functions") match {
      case JsArray(fs) => Some(Program(fs.flatMap(parseFunction)))
      case _ => None
    }
    case _ => None
  }

  /**
   * Parse a function.
   */
  private def parseFunction(json: JsValue): Option[Function] = json match {
    case JsObject(m) if m.contains("name") && m.contains("instrs") =>
      val name = m("name") match { case JsString(str) => Some(str) case _ => None }
      val instrs = m("instrs") match { case JsArray(instrs) => instrs.flatMap(parseInstruction) case _ => Seq() }
      val args = m.getOrElse("args", JsNull) match { case JsArray(args) => args.flatMap(parseArg) case _ => Seq() }
      val typ = parseType(m.getOrElse("type", JsNull))

      // create the function from the obtained params
      name.map(Function(_, args, typ, instrs))

    case _ => None
  }

  /**
   * Parse a list of strings.
   */
  private def parseListString(json: JsValue): Seq[Ident] = json match {
    case JsArray(vs) => vs.flatMap({ case JsString(str) => Some(str) case _ => None })
    case _ => Seq()
  }

  /**
   * Parse a single instruction.
   */
  private def parseInstruction(json: JsValue): Option[Instruction] = json match {
    case JsObject(m) if m.contains("label") => m("label") match { case JsString(str) => Some(Label(str)) case _ => None }

    case JsObject(m) if m.contains("op") =>
      val args = parseListString(m.getOrElse("args", JsNull))
      val labels = parseListString(m.getOrElse("labels", JsNull))
      val funcs = parseListString(m.getOrElse("funcs", JsNull))
      val typ = parseType(m.getOrElse("type", JsNull))
      val dest = m.getOrElse("dest", JsNull) match { case JsString(str) => Some(str) case _ => None }

      // either both type and dest are defined
      // or both are left blank
      if (typ.isDefined != dest.isDefined) None else
      m("op") -> args -> labels match {
        // basic ops
        case JsString("nop") -> _ -> _ => Some(NoOp)
        case JsString("print") -> Seq(s) -> _ => Some(Print(s))
        case JsString("id") -> Seq(s) ->  _ => Some(Id(dest, typ, s))
        case JsString("const") -> _ -> _ => parseValue(m.getOrElse("value", JsNull)).flatMap({
          v => if (typ.forall(v.types.contains)) Some(Const(dest, typ, v)) else None
        })

        // control flow ops
        case JsString("jmp") -> _ -> Seq(l) => Some(Jmp(l))
        case JsString("ret") -> a -> _ => Some(Ret(a.headOption))
        case JsString("call") -> a -> _ => funcs.headOption.map(Call(dest, typ, _, a))
        case JsString("br") -> Seq(a) -> Seq(t, f) => Some(Br(a, t, f))

        // memory ops
        case JsString("free") -> Seq(a) -> Seq() => Some(Free(a))
        case JsString("load") -> Seq(a) -> _ => Some(Load(dest, typ, a))
        case JsString("store") -> Seq(a, b) -> _=> Some(Store(a, b))
        case JsString("alloc") -> Seq(a) -> _ if typ.forall(_.isInstanceOf[PtrType]) => Some(Alloc(dest, typ, a))
        case JsString("ptradd") -> Seq(a, b) -> _ if typ.forall(_.isInstanceOf[PtrType]) => Some(BinOp(PtrAdd, dest, typ, a, b))

        // arithmetic operations
        case JsString("add") -> Seq(x, y) -> _ if typ.forall(_ == IntType) => Some(BinOp(Add, dest, typ, x, y))
        case JsString("sub") -> Seq(x, y) -> _ if typ.forall(_ == IntType) => Some(BinOp(Sub, dest, typ, x, y))
        case JsString("mul") -> Seq(x, y) -> _ if typ.forall(_ == IntType) => Some(BinOp(Mul, dest, typ, x, y))
        case JsString("div") -> Seq(x, y) -> _ if typ.forall(_ == IntType) => Some(BinOp(Div, dest, typ, x, y))
        case JsString("fadd") -> Seq(x, y) -> _ if typ.forall(_ == FloatType) => Some(BinOp(FAdd, dest, typ, x, y))
        case JsString("fdiv") -> Seq(x, y) -> _ if typ.forall(_ == FloatType) => Some(BinOp(FDiv, dest, typ, x, y))
        case JsString("fmul") -> Seq(x, y) -> _ if typ.forall(_ == FloatType) => Some(BinOp(FMul, dest, typ, x, y))
        case JsString("fsub") -> Seq(x, y) -> _ if typ.forall(_ == FloatType) => Some(BinOp(FSub, dest, typ, x, y))

        // comparison ops
        case JsString("lt") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(LT, dest, typ, x, y))
        case JsString("gt") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(GT, dest, typ, x, y))
        case JsString("ge") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(GE, dest, typ, x, y))
        case JsString("le") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(LE, dest, typ, x, y))
        case JsString("eq") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(EQ, dest, typ, x, y))

        // boolean ops
        case JsString("or") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(Or, dest, typ, x, y))
        case JsString("and") -> Seq(x, y) -> _ if typ.forall(_ == BoolType) => Some(BinOp(And, dest, typ, x, y))
        case JsString("not") -> Seq(x) -> _ if typ.forall(_ == BoolType) => Some(UnOp(Not, dest, typ, x))

        // phi instruction
        case JsString("phi") -> as -> ls if as.size == ls.size => Some(Phi(dest, typ, as, ls))

        // speculative execution instructions
        case JsString("guard") -> Seq(a) -> Seq(l) => Some(Guard(a, l))
        case JsString("speculate") -> _ -> _ => Some(Speculate)
        case JsString("commit") -> _ -> _ => Some(Commit)

        case _ => None
      }

    case _ => None
  }

  /**
   * Parse a value for const op.
   */
  private def parseValue(json: JsValue): Option[Value] = json match {
    case JsNumber(value) if value.isWhole => Some(NumericValue(value.intValue))
    case JsBoolean(bool) => Some(BoolValue(bool))
    case _ => None
  }

  /**
   * Parse function arguments.
   */
  private def parseArg(json: JsValue): Option[Argument] = json match {
    case JsObject(m) =>
      val name = m("name") match { case JsString(str) => Some(str) case _ => None }
      parseType(m("type")).flatMap(typ => name.map(Argument(_, typ)))

    case _ => None
  }

  /**
   * Parse a type.
   */
  private def parseType(json: JsValue): Option[Type] = json match {
    case JsString("int") => Some(IntType)
    case JsString("bool") => Some(BoolType)
    case JsString("float") => Some(FloatType)
    case JsObject(m) if m.contains("ptr") => parseType(m("ptr")).map(PtrType)
    case _ => None
  }

}
