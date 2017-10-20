package mypackage
package scala.io.Source
import scala.io.Source
import mypackage.grammar.tokenize




object APPP{
  
val consts:Map[String,Any]=Map()

  
  def main(args: Array[String]){
//    for (line <- Source.fromFile(args(0)).getLines){
    val myTokens = tokenize("var var const")
  //  }      
  } 

  def split[T](array : Array[T])(where : T=>Boolean) : List[Array[T]] = {
    if (array.isEmpty) Nil
    else {
        val (head, tail) = array span {!where(_)}
        head :: split(tail drop 1)(where)
    }
  }
  
}