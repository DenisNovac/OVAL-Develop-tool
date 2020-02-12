package composer

import java.io.File
import java.nio.file.Paths

import composer.OvalIndexer.OvalIndex

import scala.xml.{Elem, Node, XML}
import com.typesafe.scalalogging.Logger
import entities.{OvalDefinition, OvalDefinitionDependenciesGraph, OvalTest}
import FolderUtils.getThousandFolderName




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

    parseTestDependencies(testsIds.head, definition.family)
  }

  /** Parse one test's dependencies
    * @param testId id of the test
    * @param firstFamily from test's id there is no way to predict from which family or type it is. But we still need to
    *                    find it from structure: repository/tests/$ovalFamily/$ovalType/$idFolder
    */
  private def parseTestDependencies(testId: String, firstFamily: String): Unit = {
    logger.debug(s"Parsing test $testId dependencies")

    getFilePathForComplexOvalObjects("tests", firstFamily, testId)
  }

  /** Complex Oval objects - tests, objects and states (have structure like family/thousand_id/file) */
  private def getFilePathForComplexOvalObjects(ovalType: String, ovalFamily: String, id: String): String = {

    /** Check only given family and independent existence. For example, Windows definition won't use Linux object */
    def getExistingFamilyFolderPaths(typePath: String, ovalFamily: String): Array[String] =
      for {
        fam <- Array(ovalFamily, "independent")
        f = new File(Paths.get(typePath, fam).toString)
        if f.exists
      } yield f.getPath

    /** There is types folders (like file_test, registry_test) inside family folder so we'll walk through each */
    def getThousandIdFolderPaths(familyPath: String, thousandId: String): Array[String] = {
      val familyFolder = new File(familyPath)
      for {
        p <- familyFolder.listFiles.map(_.getPath)
        f = new File(Paths.get(p).toString)
        if f.listFiles.map(_.getPath).contains(Paths.get(p, thousandId).toString)
      } yield Paths.get(f.getPath, thousandId).toString
    }

    // Checking exitence of Family folder or Independent folder
    val familyFolderPaths = getExistingFamilyFolderPaths(Paths.get("repository", "tests").toString, ovalFamily)
    if (familyFolderPaths.isEmpty) {
      val message = s"Can't find file of $id: there is no folder suitable for $id"
      logger.error(message)
      throw new Error(message)
    }
    logger.debug(s"Found suitable family folders: ${familyFolderPaths.mkString(",")}")

    // Searching wanted thousand ID in those folders
    val thousandId = getThousandFolderName(id.split(":").last.toLong)
    val familyFolderPathsWithThousand: Array[String] = { for {
      f <- familyFolderPaths
    } yield getThousandIdFolderPaths(f, thousandId) }.flatten
    if (familyFolderPathsWithThousand.isEmpty) {
      val message = s"Can't find wanted thousand ID of $id: there is no folder $thousandId in family folders"
      logger.error(message)
      throw new Error(message)
    }
    logger.debug(s"Found suitable thousand id folders: ${familyFolderPathsWithThousand.mkString(",")}")

    // Searching wanted ID file
    val fileById = { for {
      f <- familyFolderPathsWithThousand
      possible = new File(Paths.get(f, id.replace(":", "_")+".xml").toString)
      if possible.exists()
    } yield possible }.map(_.getPath)

    if (fileById.length == 1) {
      logger.debug(s"Found file: ${fileById.head}")
      fileById.head
    } else {
      val filesString = fileById.mkString(",")
      val message = s"There is none or too much (${fileById.length}) of suitable OVAL files: $filesString"
      logger.error(message)
      throw new Error(message)
    }
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
