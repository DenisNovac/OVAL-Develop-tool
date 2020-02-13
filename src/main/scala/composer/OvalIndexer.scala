package composer

import java.io.{File, FileNotFoundException}
import scala.reflect.runtime.universe._
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
  type ElementIndex = Tuple4[Vector[OvalTest], Vector[OvalObject], Vector[OvalState], Vector[OvalVariable]]

  /** Indexing method
    * @throws FileNotFoundException if repository does not exists
    */
  def createIndex(): (DefinitionIndex, ElementIndex) = {

    logger.info("Repository found, reading definitions")

    val definitionIndex = indexDefinitions()
    logger.info(s"Parsed ${definitionIndex.length} definitions")

    logger.info("Reading elements")

    /** Factories with methods for creation of some T
      * TODO: Replace it somewhere
      * */

    implicit object FactoryT extends Factory[OvalTest] {
      override def createT(id: String, path: String): OvalTest = new OvalTest(id, path)
    }

    implicit object FactoryOb extends Factory[OvalObject] {
      override def createT(id: String, path: String): OvalObject = new OvalObject(id, path)
    }

    implicit object FactorySt extends Factory[OvalState] {
      override def createT(id: String, path: String): OvalState = new OvalState(id, path)
    }

    implicit object FactoryVar extends Factory[OvalVariable] {
      override def createT(id: String, path: String): OvalVariable = new OvalVariable(id, path)
    }


    val testsIndex = new ElementsIndexer[OvalTest].indexElements("tests")
    val objectsIndex =  new ElementsIndexer[OvalObject].indexElements("objects")
    val statesIndex =  new ElementsIndexer[OvalState].indexElements("states")
    val variablesIndex =  new ElementsIndexer[OvalVariable].indexElements("variables")

    val elementIndex = (testsIndex, objectsIndex, statesIndex, variablesIndex)

    val sumLength = testsIndex.length + objectsIndex.length + statesIndex.length + variablesIndex.length
    logger.info(s"Parsed ${testsIndex.length} tests, ${objectsIndex.length} objects, ${statesIndex.length} states, " +
      s"${variablesIndex.length} variables ($sumLength)")

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


  class ElementsIndexer[T <: OvalEntity] {  // T is a subtype of OvalEntity;

    /** TODO: Process folder from given type through reflection */
    /*implicit private val tag: TypeTag[T] = typeTag[T]
    private def getTypeString(implicit tag: TypeTag[T]): String = typeOf[T] match {
      case t if t =:= typeOf[OvalTest] => "tests"
      case t if t =:= typeOf[OvalObject]=> "objects"
      case t if t =:= typeOf[OvalState]=> "states"
      case t if t =:= typeOf[OvalVariable] => "variables"
    }
    private val ovalType = getTypeString*/

    /** Factory is a object which contains methods for creating correct T */
    def indexElements(ovalType: String)(implicit factory: Factory[T]): Vector[T] = {
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
      factory.createT(id, e.getPath)
      }
    }
  }
}
