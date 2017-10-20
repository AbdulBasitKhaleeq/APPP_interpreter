package mypackage

sealed trait TokenType
//Tokenz will have type and their values
case class TokenObject(Type: TokenType, Value: String)

case object DataTypes extends TokenType
case object Int_values extends TokenType
case object Alpha_values extends TokenType
case object Bool_values extends TokenType
case object Identifires extends TokenType
case object Identifires_types extends TokenType
case object Colon extends TokenType
case object Print extends TokenType
case object Terminater extends TokenType
case object Equals extends TokenType
case object Binary extends TokenType
case object Unary extends TokenType
case object LParen extends TokenType
case object RParen extends TokenType

case class tokenPatterns(pattern:scala.util.matching.Regex,t_type: TokenType)

object grammar{
   def var_name=tokenPatterns("""[a-zA-Z]{1}[a-zA-Z_$#0-9]*""".r,Identifires)
   def var_types=tokenPatterns("""var|const""".r,Identifires_types)
   def data_type=tokenPatterns("""int|bool|alpha""".r,DataTypes)
   def booleans= tokenPatterns("""[tt|ff]{1}""".r,Bool_values)
   def ints=tokenPatterns("""[+-]?[0-9]{1,5}""".r,Int_values)
   def alpha=tokenPatterns(""""{1}[\p{Alnum}\p{Punct}\p{Space}]+"{1}""".r,Alpha_values)
   def colon=tokenPatterns(""":{1}""".r,Colon)
   def print=tokenPatterns("""print{1}""".r,Print)
   def terminater=tokenPatterns("""[;|\n]{1}""".r,Terminater)
   def equal=tokenPatterns("""={1}""".r,Equals)
   def unary=tokenPatterns("""[-|not]{1}""".r,Unary)
   def binary=tokenPatterns("""[+|*|\/|\^]""".r,Binary)
   def left_paren=tokenPatterns("""\(""".r,LParen)
   def right_paren=tokenPatterns("""\)""".r,LParen)
   
   

   def operation_list= List(left_paren,right_paren,colon,terminater,var_types,data_type,equal,binary,unary,print,ints,booleans,var_name)
   
   def match_pattern(Line:String,parts:List[tokenPatterns]):(String,TokenObject,String)={
     if (parts.isEmpty) {
      throw new Exception("no match")
    }

    val regex = parts.head.pattern
    val tokenType = parts.head.t_type

    val matcher=regex.pattern.matcher(Line)
    if (matcher.find()){
      (
        Line.substring(0,matcher.start()),
        TokenObject(tokenType,Line.substring(matcher.start(),matcher.end())),
        Line.substring(matcher.end())
      )
    }else
      match_pattern(Line,parts.tail)
  }

  def tokenize(line: String): List[TokenObject] = {
    if (line.trim.isEmpty)
      List()
    else{
      val (before,token,after) = match_pattern(line,operation_list)
      println("Token found: ",token)
      tokenize(before) ::: List(token) ::: tokenize(after)
    }
   }

}

/*   val dataTypes="""(int|bool|alpha)""".r

// boolvalues
val bool_values="""(t{2}|f{2})""".r

// int values
val int_values="""([+-]?[0-9]{1,5})""".r

//alpha's
val alpha_values="""("{1}[\p{Alnum}\p{Punct}\p{Space}]+"{1})""".r


val LT="""\n|\r|""".r  //new line or cariage return

val line_end=""";{1}"""
* 

val Variable_pattern="""( ?var| ?const) +([a-zA-Z][a-zA-Z_$#0-9]+) *:{1} *(int|bool|alpha) *(= *([+-]?[0-9]+|t{2}|f{2}))?""".r

val Variable_initials=s" *(var|const){1} +${var_name} *:{1} *${dataTypes} *(=? *(${int_values}|${bool_values}|${alpha_values})?)".r

val Print_pattern=s" ?(print{1}) +${alpha_values}|${var_name}".r

* */
