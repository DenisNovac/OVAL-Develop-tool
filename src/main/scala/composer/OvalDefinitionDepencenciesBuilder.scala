package composer

import composer.OvalIndexer.{DefinitionIndex, ElementIndex, OvalIndex}

import scala.xml.{Elem, XML}
import com.typesafe.scalalogging.Logger
import entities._

import scala.collection.mutable



class OvalDefinitionDepencenciesBuilder(index: OvalIndex) {
  private val logger = Logger("OVAL Dependencies Builder")
  private val collectedObjectsIdsSet: mutable.HashSet[String] = mutable.HashSet()  // registry of collected objects to prevent endless loop

  /** Method for building Dependencies Graph for given definition on given index
    *
    * @param definition      definition for which one wants to build a graph
    */
  def buildGraphForDefinition(definition: OvalDefinition) = {

    val (extendDefinitionsIds, testsIds) = parseDefinitionDependencies(definition)

    val testsDependencies: Vector[(Vector[OvalObject], Vector[OvalState])] = for {
      t <- testsIds
    } yield {
      parseTestDependencies(index._2._1.filter(_.id == t.id).head, definition.family)
    }

    if (testsDependencies.nonEmpty) {
      val (objectIds, stateIds): (Vector[OvalObject], Vector[OvalState]) =
        testsDependencies.reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))

      /** Objects dependencies parsing */
      for {
        obj <- objectIds
      } yield {
        val p = index._2._2.filter(_.id == obj.id) // getting the object from index
        parseObjectDependencies(p.head)
      }

      /** States dependencies parsing */
      for {
        ste <- stateIds
      } yield {
        val p = index._2._3.filter(_.id == ste.id)
      }
    } else logger.warn(s"${definition.id} does not contain tests with dependencies.")
  }


  /** Method for parsing definitions for getting it's dependencies.
    * There are two inner parts (separate methods): one for searching extend definitions and one for searching tests.
    *
    * @param definition OvalDefinition object
    * @return tuple (OvalDefinitions, OvalTests)
    */
  private def parseDefinitionDependencies(definition: OvalDefinition): (Vector[OvalDefinition], Vector[OvalTest]) = {

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

    val extendDefinitions = for {
      id <- extendDefinitionsIds
    } yield {
      index._1.filter(_.id == id).head
    }

    val tests = for {
      id <- testsIds
    } yield {
      index._2._1.filter(_.id == id).head
    }
    (extendDefinitions, tests)
  }


  /** Parse one test's dependencies
    *
    * @param ovalTest    test object
    * @param firstFamily from test's id there is no way to predict from which family or type it is. But we still need to
    *                    find it from structure: repository/tests/$ovalFamily/$ovalType/$idFolder
    * @return Tuple of dependencies: (Objects, States)
    */
  private def parseTestDependencies(ovalTest: OvalTest, firstFamily: String): (Vector[OvalObject], Vector[OvalState]) = {
    val xml = XML.load(ovalTest.path)

    /** Parsing the XML */
    val objectsXml = xml \ "object"
    val statesXml = xml \ "state"

    val objectIds = {
      for {
        o <- objectsXml
      } yield {
        o.attribute("object_ref") match {
          case Some(x) => x
          case None =>
            val message = s"Object in test ${ovalTest.path} does not contain object_ref attrubute"
            logger.error(message)
            throw new Error(message)
        }
      }
      }.flatten.toVector.map(_.text)

    val stateIds = {
      for {
        s <- statesXml
      } yield {
        s.attribute("state_ref") match {
          case Some(x) => x
          case None =>
            val message = s"State in test ${ovalTest.path} does not contain state_ref attrubute"
            logger.error(message)
            throw new Error(message)
        }
      }
      }.flatten.toVector.map(_.text)


    val objects = for {
      id <- objectIds
    } yield {
      index._2._2.filter(_.id == id).head
    }

    val states = for {
      id <- stateIds
    } yield {
      index._2._3.filter(_.id == id).head
    }

    (objects, states)
  }


  /** Method for parsing objects
    * We'll need to look through all tags of object and search for var_ref and object_reference
    *
    * @param ovalObject   contains the object
    * @return Tuple of dependencies: (Objects, Variables)
    */
  private def parseObjectDependencies(ovalObject: OvalObject):
  (Vector[OvalObject], Vector[OvalVariable]) = {

    val xml = XML.load(ovalObject.path)
    val decompose = xml.descendant

    val objectIds: Vector[String] = {
      for {
        n <- decompose // all childs
        if n.label == "object_reference"
      } yield {
        n
      }
      }.toVector.flatten.map(_.text)

    val variableIds: Vector[String] = {
      for {
        n <- decompose
        if n.attribute("var_ref").orNull != null
      } yield {
        n.attribute("var_ref") match {
          case Some(x) => x
          case None =>
            val message = s"VarRef in object ${ovalObject.path} does not contain id reference"
            logger.error(message)
            throw new Error(message)
        }
      }
      }.toVector.flatten.map(_.text)

    /** Getting actual objects and variables from index */
    val objectsDependencies = for {
      id <- objectIds
    } yield {
      val obj = index._2._2.filter(_.id == id).head
      collectedObjectsIdsSet.add(obj.id)
      obj
    }

    val variablesDependencies = for {
      id <- variableIds
    } yield {
      index._2._4.filter(_.id == id).head
    }

    /** Nested objects references check */
    val nested: Vector[(Vector[OvalObject], Vector[OvalVariable])] = for {
      o <- objectsDependencies
      if !collectedObjectsIdsSet.contains(o.id)
    } yield {
      logger.debug(s"Found nested uncollected reference for object ${ovalObject.id} ->  ${o.id}, processing")
      parseObjectDependencies(o)
    }

    if (nested.nonEmpty) {
      val (nestedObjects, nestedVariables): (Vector[OvalObject], Vector[OvalVariable]) =
        nested.reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
      (objectsDependencies ++ nestedObjects, variablesDependencies ++ nestedVariables)
    } else
      (objectsDependencies, variablesDependencies)
  }
}