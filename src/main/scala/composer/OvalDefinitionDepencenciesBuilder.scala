package composer

import composer.OvalIndexer.{DefinitionIndex, ElementIndex}

import scala.xml.{Elem, XML}
import com.typesafe.scalalogging.Logger
import entities._




object OvalDefinitionDepencenciesBuilder {
  private val logger = Logger("OVAL Dependencies Builder")


  /** Method for building Dependencies Graph for given definition on given index
    * @param definitionIndex is used to get dependent definitions out of it by ID
    * @param definition definition for which one wants to build a graph
    */
  def buildGraph(definitionIndex: DefinitionIndex, elementIndex: ElementIndex, definition: OvalDefinition) = {
    logger.info(s"Building graph for definition ${definition.id}")
    val (extendDefinitionsIds, testsIds) = parseDefinitionDependencies(definition)

    val testsDependencies: Vector[(Vector[String], Vector[String])] =  for {
      id <- testsIds
    } yield {
      parseTestDependencies(elementIndex._1.filter(_.id == id).head, definition.family)
    }

    if (testsDependencies.nonEmpty) {
      val (objectIds, stateIds): (Vector[String], Vector[String]) =
        testsDependencies.reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))

      /** Objects dependencies parsing */
      for {
        id <- objectIds
      } yield {
        val p = elementIndex._2.filter(_.id == id)  // getting the object from index
        parseObjectDependencies(p.head)
        //println(p)
      }

      /** States dependencies parsing */
      for {
        id <- stateIds
      } yield {
        val p = elementIndex._3.filter(_.id == id)
        //println(p)
      }


    } else logger.warn(s"${definition.id} does not contain tests with dependencies.")
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


  /** Parse one test's dependencies
    * @param ovalTest test object
    * @param firstFamily from test's id there is no way to predict from which family or type it is. But we still need to
    *                    find it from structure: repository/tests/$ovalFamily/$ovalType/$idFolder
    * @return Tuple of dependencies: (Objects, States)
    */
  private def parseTestDependencies(ovalTest: OvalTest, firstFamily: String): (Vector[String], Vector[String]) = {
    val xml = XML.load(ovalTest.path)

    /** Parsing the XML */
    val objects = xml \ "object"
    val states = xml \ "state"

    val objectIds ={ for {
      o <- objects
    } yield {
      o.attribute("object_ref") match {
        case Some(x) => x
        case None =>
          val message = s"Object in test ${ovalTest.path} does not contain object_ref attrubute"
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
          val message = s"State in test ${ovalTest.path} does not contain state_ref attrubute"
          logger.error(message)
          throw new Error(message)
      }
    } }.flatten.toVector.map(_.text)

    (objectIds, stateIds)
  }


  /** Method for parsing objects
    * We'll need to look through all tags of object and search for var_ref and object_reference
    * @param ovalObject contains the object
    * @return Tuple of dependencies: (Objects, Variables)
    */
  private def parseObjectDependencies(ovalObject: OvalObject): (Vector[String], Vector[String]) = {
    val xml = XML.load(ovalObject.path)
    println("Im here")
    (Vector(), Vector())
  }


}
