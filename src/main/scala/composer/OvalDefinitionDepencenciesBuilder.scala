package composer

import scala.xml.{Elem, Node, XML}
import entities.{OvalDefinition, OvalDefinitionDependenciesGraph, OvalTest}

object OvalDefinitionDepencenciesBuilder {

  def buildGraph(definition: OvalDefinition) = {
    val extendDefinitionsIds, testsIds = parseDefinitionDependencies(definition)

  }


  private def parseDefinitionDependencies(definition: OvalDefinition): Tuple2[Vector[String], Vector[String]] = {

    def getExtendDefinitionsDependenciesIds(definition: Elem): Vector[String] = {
      val extendDefinitions = definition \\ "criteria" \ "extend_definition"
      val definitionsIdList = for {
        d <- extendDefinitions
      } yield d.attribute("definition_ref") match {
        case Some(x) => x
        case None => throw new Error(s"No definition_ref in one of extend definitions in $definition")
      }
      definitionsIdList.flatten.map(_.text).toVector
    }

    def getDefinitionTestsDepencenciesIds(definition: Elem): Vector[String] = {
      val tests = definition \\ "criteria" \ "criterion"
      val testsIdList = for {
        d <- tests
      } yield d.attribute("test_ref") match {
        case Some(x) => x
        case None => throw new Error(s"One of the extend definitions in ${definition} does not have definition_ref")
      }
      testsIdList.flatten.map(_.text).toVector
    }

    val rootDefinitionXml = XML.load(definition.path)
    val extendDefinitionsIds = getExtendDefinitionsDependenciesIds(rootDefinitionXml)
    val testsIds = getDefinitionTestsDepencenciesIds(rootDefinitionXml)

    (extendDefinitionsIds, testsIds)
  }
}
