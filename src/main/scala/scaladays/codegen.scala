package scaladays

import java.io.File
import scala.collection.immutable.Seq
import scala.meta._
import scala.annotation.StaticAnnotation
import org.json4s._
import org.json4s.native.JsonMethods._

class codegen extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    implicit val formats = DefaultFormats

    val dir = System.getenv("user.dir")
    println("user.dir is " + dir)
    val json = parse(FileInput(new File("domain.json")))
    def classes: Seq[(Defn.Class, Defn.Val)] = json.extract[Map[String, List[(String, String)]]].map {
      case (name, members) => 
        val membersTerms: Seq[Term.Param] = members.map {
          case (name, domainType) =>
            val paramName = Term.Name(name)

            val seq = """seq\[(\w+)\]""".r
            val opt = """option\[(\w+)\]""".r
            val map = """map\[(\w+)\]""".r

            val tpe = domainType match {
              case "int" => t"Int"
              case "string" => t"String"
              case seq(inner) => Type.Name(s"Seq[${inner.capitalize}]")
              case map(inner) => {
                val vType = Type.Name(inner.capitalize)
                t"Map[String,${vType}]"
              }
              case opt(inner) => {
                println("inner is " + inner)
                Type.Name(s"Option[${inner.capitalize}]")
              }
              case t => Type.Name(t.capitalize)
            }

            param"$paramName: $tpe"
        }
        val tName = Type.Name(name.capitalize)
        val implicitName =  Pat.Var.Term(Term.Name(name.capitalize + "Format"))

        val vName = Pat.Var.Term(Term.Name(name.capitalize + "Format"))

        val n = members.size

        val aName = Term.Name(name.capitalize)

        val jsonFormatN = Term.Name("jsonFormat" + n)

        // Term.Name(name.capitalize + "Format")
        (q"""
            case class $tName(..$membersTerms)
         """,
          q"""implicit val $vName = $jsonFormatN($aName)"""
        )
    }.toList


    def toScalaType(domainType: String): Type = domainType match {
      case "int" => t"Int"
      case "string" => t"String"
      case t => Type.Name(t.capitalize)


    }

    defn match {
      case obj: Defn.Object =>
        val k = classes;
        val c = classes.map(_._1)
        val v = classes.map(_._2)
        val ret =
          q"""
             object ${obj.name} {
             import spray.json.DefaultJsonProtocol._
             ..${c}
             ..${v}
             }
            """
        println(ret)
        ret
    }
  }
}
