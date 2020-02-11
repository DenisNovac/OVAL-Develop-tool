package composer

import composer.OvalIndexer.OvalIndex

import scala.xml.{Elem, Node, XML}
import entities.{OvalDefinition, OvalDefinitionDependenciesGraph, OvalTest}
import FolderUtils.getThousandFolderName

object OvalDefinitionDepencenciesBuilder {

  /** Method for building Dependencies Graph for given definition on given index
    * @param index is used to get dependent definitions out of it by ID
    * @param definition definition for which one wants to build a graph
    */
  def buildGraph(index: OvalIndex, definition: OvalDefinition) = {
    val extendDefinitionsIds, testsIds = parseDefinitionDependencies(definition)

    parseTestDependencies(testsIds[0], definition.family)
  }

  /** Parse one test's dependencies
    * @param testId id of the test
    * @param firstFamily from test's id there is no way to predict from which family or type it is. But we still need to
    *                    find it from structure: repository/tests/$ovalFamily/$ovalType/$idFolder
    */
  private def parseTestDependencies(testId: String, firstFamily: String): Unit = {
    val numericId = testId.split(":").last.toLong
    val numericFolderName = getThousandFolderName(numericId)
    val path = s"repository/tests/$firstFamily/"
  }


  /** Method for parsing definitions for getting it's dependencies.
    * There are two inner parts (separate methods): one for searching extend definitions and one for searching tests.
    * @param definition OvalDefinition object
    * @return Required extend definitions list, Required tests list
    */
  private def parseDefinitionDependencies(definition: OvalDefinition): (Vector[String], Vector[String]) = {

    /** This method search extend definitions refs in given definition */
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

    /** This method search tests refs in given  definition */
    def getDefinitionTestsDepencenciesIds(definition: Elem): Vector[String] = {
      val tests = definition \\ "criteria" \ "criterion"
      val testsIdList = for {
        d <- tests
      } yield d.attribute("test_ref") match {
        case Some(x) => x
        case None => throw new Error(s"One of the extend definitions in $definition does not have definition_ref")
      }
      testsIdList.flatten.map(_.text).toVector
    }

    /** Load definition file from path found in index */
    val rootDefinitionXml = XML.load(definition.path)

    val extendDefinitionsIds = getExtendDefinitionsDependenciesIds(rootDefinitionXml)
    val testsIds = getDefinitionTestsDepencenciesIds(rootDefinitionXml)
    (extendDefinitionsIds, testsIds)
  }
}
