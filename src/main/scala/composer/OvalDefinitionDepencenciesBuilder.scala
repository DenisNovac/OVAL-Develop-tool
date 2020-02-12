package composer

import composer.OvalIndexer.OvalIndex

import scala.xml.{Elem, Node, XML}
import com.typesafe.scalalogging.Logger
import entities.{OvalDefinition, OvalDefinitionDependenciesGraph, OvalTest}
import RepositoryUtils.getFilePathForComplexOvalObjects




object OvalDefinitionDepencenciesBuilder {
  private val logger = Logger("OVAL Dependencies Builder")

  /** Method for building Dependencies Graph for given definition on given index
    * @param index is used to get dependent definitions out of it by ID
    * @param definition definition for which one wants to build a graph
    */
  def buildGraph(index: OvalIndex, definition: OvalDefinition) = {
    logger.info(s"Building graph for definition ${definition.id}")
    val (extendDefinitionsIds, testsIds) = parseDefinitionDependencies(definition)
    logger.info(s"Done parsing inner dependencies of definition")

    val (objectIds, stateIds): (Vector[String], Vector[String]) = { for {
      t <- testsIds
    } yield {
      parseTestDependencies(t, definition.family)
    } }.reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))  // Vector[(Vector[String], Vector[String])] => (Vector[String], Vector[String])
    // other options to do the same:
    //testDependencies.foldLeft(Vector(""), Vector("")){case (a,(o,s)) => (a._1 ++ o, a._2 ++ s)}  // BAD: creates empty lines
    //testDependencies.fold(Vector.empty[String], Vector.empty[String]){case (a,(o,s)) => (a._1 ++ o, a._2 ++ s)}

    println(objectIds.mkString(","))
    println(stateIds.mkString(","))
    ()
  }


  /** Parse one test's dependencies
    * @param testId id of the test
    * @param firstFamily from test's id there is no way to predict from which family or type it is. But we still need to
    *                    find it from structure: repository/tests/$ovalFamily/$ovalType/$idFolder
    */
  private def parseTestDependencies(testId: String, firstFamily: String): (Vector[String], Vector[String]) = {
    logger.debug(s"Parsing test $testId dependencies")

    /** Finding the file of test and loading it */
    val path = getFilePathForComplexOvalObjects("tests", firstFamily, testId)
    logger.debug(s"Found file path $path")
    val xml = XML.load(path)

    /** Parsing the XML */
    val objects = xml \ "object"
    val states = xml \ "state"

    val objectIds ={ for {
      o <- objects
    } yield {
      o.attribute("object_ref") match {
        case Some(x) => x
        case None =>
          val message = s"Object in test $testId does not contain object_ref attrubute"
          logger.error(message)
          throw new Error(message)
      }
    } }.flatten.toVector.map(_.text)

    val stateIds = { for {
      s <- states
    } yield {
      s.attribute("state_ref") match {
        case Some(x) => x
        case None =>
          val message = s"State in test $testId does not contain state_ref attrubute"
          logger.error(message)
          throw new Error(message)
      }
    } }.flatten.toVector.map(_.text)

    (objectIds, stateIds)
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
