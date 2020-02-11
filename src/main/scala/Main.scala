import validation._
import composer._

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new ArgumentsConfig(args)
    val path = conf.xml()
    //OvalValidator.validate(path)
    new OvalDecomposer().decompose(path)
    val index = OvalIndexer.createIndex()

    OvalDefinitionDepencenciesBuilder.buildGraph(index.filter(_.id=="oval:org.mitre.oval:def:18972").head)
  }

}

