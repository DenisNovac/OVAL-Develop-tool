package composer
import com.typesafe.scalalogging.Logger
import java.io.File
import java.nio.file.Paths

import scala.xml.{Node, NodeSeq, XML}

object OvalDecomposer {
  private val logger = Logger("OVAL Decomposer")

  def decompose(path: String): Unit = {
    val xml_parse = XML.loadFile(path)
    if (xml_parse.label != "oval_definitions")
      throw new Error(s"$path is not a definitions file")

    val xml = xml_parse \\ "oval_definitions"
    logger.info(s"Loaded XML: $path")

    /** Decomposing OVAL entities */
    val definitions = xml \\ "definitions" \ "_"
    val tests = xml \\ "tests" \ "_"
    val objects = xml \\ "objects" \ "_"
    val states = xml \\ "states" \ "_"
    val variables =  xml \\ "variables" \ "_"
    logger.info("OVAL entries decomposed")

    logger.info(s"Decomposing ${definitions.length} definitions")
    decomposeDefinition(definitions)

    logger.info(s"Decomposing ${tests.length} tests")
    decomposeOvalClass("tests", tests)

    logger.info(s"Decomposing ${objects.length} objects")
    decomposeOvalClass("objects", objects)

    logger.info(s"Decomposing ${states.length} states")
    decomposeOvalClass("states", states)

    logger.info(s"Decomposing ${variables.length} variables")
    decomposeVariables(variables)

    logger.info("Decomposition complete")
  }


  /** Decomposing definitions */
  private def decomposeDefinition(xmlDefinitions: NodeSeq) = for {
    d <- xmlDefinitions
  } yield {
    val id = getId(d)
    val definitionType = d.attribute("class") match {
      case Some(x) => x
      case None => throw new Error(s"There is no class in some definition: \n$d")
    }
    saveXmlToRepository(s"repository/definitions/$definitionType", s"$id.xml", d)
  }


  /** Decomposing variables */
  private def decomposeVariables(xmlVariables: NodeSeq) = for {
    v <- xmlVariables
  } yield {
    val id = getId(v)
    saveXmlToRepository(s"repository/variables/", s"$id.xml", v)
  }


  /** Decomposing of OVAL entities except for Definitions and Variables*/
  private def decomposeOvalClass(className: String, xmlClass: NodeSeq) = for {
    c <- xmlClass
  } yield {
    val id = getId(c)
    val ovalFamily = c.namespace.split("#").last
    val ovalType = c.label
    val idFolder = getThousandFolderName(id.split("_").last.toLong)
    saveXmlToRepository(s"repository/$className/$ovalFamily/$ovalType/$idFolder", s"$id.xml", c)
  }


  /** This method will get folder name for ID */
  private def getThousandFolderName(id: Long): String = (id/1000)*1000 match {
    case 0 => "0000"
    case x => x.toString
  }


  /** Method for getting ID  */
  private def getId(xml: Node): String = xml.attribute("id") match {
    case Some(x) => x.mkString("").replace(":", "_")
    case None => throw new Error(s"There is no ID in some definition: \n$xml")
  }

  /** Method for saving into non-existing directories */
  private def saveXmlToRepository(path: String, name: String, xml: Node): Unit = {
    val f = new File(path)
    if (!f.exists())
      f.mkdirs()
    XML.save(Paths.get(path, name).toString, xml)
  }
}
