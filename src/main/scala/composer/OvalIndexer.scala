package composer

import java.io.{File, FileNotFoundException}

import scala.xml.XML
import com.typesafe.scalalogging.Logger
import entities._
import RepositoryUtils.recursiveListFiles

/** Class for search through definitions before building.
  * It reads whole list of definitions and make collection of OvalDefinitions objects.
  */
object OvalIndexer {
  private val logger = Logger("OVAL Indexer")

  type DefinitionIndex = Vector[OvalDefinition]
  type ElementIndex = Map[String, Vector[OvalEntity]]

  /** Indexing method
    * @throws FileNotFoundException if repository does not exists
    */
  def createIndex(): (DefinitionIndex, ElementIndex) = {

    logger.info("Repository found, reading definitions")

    val definitionIndex = indexDefinitions()
    logger.info(s"Parsed ${definitionIndex.length} definitions")

    logger.info("Reading elements")
    val testsIndex = indexElements("tests")
    val objectsIndex = indexElements("objects")
    val statesIndex = indexElements("states")
    val variablesIndex = indexElements("variables")

    val elementIndex = Map("tests" -> testsIndex, "objects" -> objectsIndex, "states" -> statesIndex, "variables" -> variablesIndex)


    logger.info(s"Parsed ${testsIndex.length} tests, ${objectsIndex.length} objects, ${statesIndex.length} states, " +
      s"${variablesIndex.length} variables (${elementIndex.values.flatten.size})")


    (definitionIndex, elementIndex)
  }



  /** Definition parsing and collecting metadata */
  private def indexDefinitions(): Vector[OvalDefinition] = {
    val definitionsFolder = new File("repository/definitions")

    if (!definitionsFolder.exists()) {
      val message = "Repository does not exists or does not contains 'definitions' directory"
      logger.error(message)
      throw new Error(message)
    }

    val definitions = recursiveListFiles(definitionsFolder).filter(_.isFile)
    logger.info(s"Found ${definitions.length} definitions")
    for {
      d <- definitions
    } yield {
      val definition = XML.loadFile(d)
      val meta = definition \\ "metadata"

      val id = definition.attribute("id") match {
        case Some(x) => x
        case None =>
          val message = s"Definition ${d.getPath} does not contain ID"
          logger.error(message)
          throw new Error(message)
      }

      val ovalClass = definition.attribute("class") match {
        case Some(x) => x
        case None =>
          val message = s"Definition ${d.getPath} does not contain class"
          logger.error(message)
          throw new Error(message)
      }

      val title = meta \ "title"
      val description = meta \ "description"
      val affected = meta \ "affected"

      val family = affected.head.attribute("family") match {
        case Some(x) => x
        case None =>
          val message = s"Definition ${d.getPath} does not contain affected family"
          logger.error(message)
          throw new Error(message)
      }

      val platforms = { for {
        p <- affected \ "platform"
      } yield {
        p.text
      } }.toArray

      val products = { for {
        p <- affected \ "product"
      } yield {
        p.text
      } }.toArray

      val references = { for {
        r <- meta \ "reference"
      } yield {
        r.attribute("ref_id") match {
          case Some(x) => x.text
          case None =>
            val message = s"Definition ${d.getPath} does not contain ref_id in reference $r"
            logger.error(message)
            throw new Error(message)
        }
      } }.toArray

      OvalDefinition(id.text, d.getPath, ovalClass.text, title.text, description.text, family.text, platforms, products, references)
    }}





  private def indexElements(ovalType: String): Vector[OvalEntity] = {
    val elementsFolder = new File(s"repository/$ovalType")

    if (!elementsFolder.exists()) {
      val message = s"Repository does not exists or does not contains $ovalType directory"
      logger.error(message)
      throw new Error(message)
    }

    val elements = recursiveListFiles(elementsFolder).filter(_.isFile)

    for {
      e <- elements
    } yield {
      val elem = XML.loadFile(e)

      val id = elem.attribute("id") match {
        case Some(x) => x.text
        case None =>
          val message = s"Element from $ovalType does not contain ID: \n $elem"
          logger.error(message)
          throw new Error(message)
      }

      ovalType match {
        case "tests" => OvalTest(id, e.getPath)
        case "objects" => OvalObject(id, e.getPath)
        case "states" => OvalState(id, e.getPath)
        case "variables" => OvalVariable(id, e.getPath)
      }
    }
  }
}
