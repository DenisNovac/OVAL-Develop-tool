package composer

import java.io.{File, FileNotFoundException}

import scala.xml.XML
import com.typesafe.scalalogging.Logger
import entities.OvalDefinition
import RepositoryUtils.recursiveListFiles

/** Class for search through definitions before building.
  * It reads whole list of definitions and make collection of OvalDefinitions objects.
  */
object OvalIndexer {
  private val logger = Logger("OVAL Indexer")

  type OvalIndex = Vector[OvalDefinition]


  /** Indexing method
    * @throws FileNotFoundException if repository does not exists
    */
  def createIndex(): OvalIndex = {
    val definitions = new File("repository/definitions")

    if (!definitions.exists()) {
      val message = "Repository does not exists or does not contains 'definitions' directory"
      logger.error(message)
      throw new Error(message)
    }


    logger.info("Repository found, reading definitions")
    val definitionsList = recursiveListFiles(definitions).filter(_.isFile)
    logger.info(s"Found ${definitionsList.length} definitions")

    val index = indexDefinitions(definitionsList)
    logger.info(s"Parsed ${index.length} definitions")

    index
  }



  /** Definition parsing and collecting metadata */
  private def indexDefinitions(definitions: Vector[File]): Vector[OvalDefinition] = for {
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
    }
}
