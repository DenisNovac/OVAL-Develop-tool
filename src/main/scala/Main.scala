import validation._
import composer._

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new ArgumentsConfig(args)
    val path = conf.xml()
    //OvalValidator.validate(path)
    new OvalDecomposer().decompose(path)
    val (definitionIndex, elementIndex) = OvalIndexer.createIndex()

    OvalDefinitionDepencenciesBuilder.buildGraph(definitionIndex, elementIndex, definitionIndex.filter(_.id=="oval:org.mitre.oval:def:18972").head)


    /*for {
      e <- elementIndex("variables")
    } yield {
      println(e)
    }*/

    /*for {
      i <- definitionIndex
    } yield {
      OvalDefinitionDepencenciesBuilder.buildGraph(definitionIndex, elementIndex, i)
    }*/


  }

}

