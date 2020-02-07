package validation
import com.typesafe.scalalogging.Logger
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory

import scala.xml.XML


object OvalValidator {
  private val logger = Logger("OVAL Validator")

  /** A simple validation method.
    *
    * TODO: Referential integrity validation
    * @param path: Path to OVAL xml to validate
    * @return validation result
    */
  def validate(path: String): Boolean = {
    // XML decomposition
    val xml = XML.loadFile(path)
    logger.info(s"Loaded XML: $path")

    val ovalType = xml.label  // oval_definitions, oval_results or oval_system_characteristics
    logger.info(s"OVAL file type: $ovalType")

    val types = xml.head ++ (xml \\ ovalType).head ++ (xml \\ ovalType \\ "_") ++ (xml \\ ovalType \\ "_" \\ "_")
    val groups = types.groupBy(_.namespace).keys.filterNot(_ == null)

    val ovalVersionMajor = "5"
    val ovalVersion = "5.11.1"

    val namespaces: Array[Source] = { for {
      ns <- groups
    } yield {
      (ns split "#").toList match {

        case Nil =>
          throw new Error(s"Invalid empty OVAL xmlns namespace: $ns")

        /** Misc schemas like "oval-definitions-schema" */
        case x :: Nil =>
          // example: http://oval.mitre.org/XMLSchema/oval-definitions-5  => "oval-definitions"
          val miscOvalType = x.split("/").last.split(s"-$ovalVersionMajor").head
          logger.info(s"Misc namespace found: $miscOvalType")
          new StreamSource(s"$ovalVersion/$miscOvalType-schema.xsd")

        /** Real namespaces like "windows" */
        case x :: y :: Nil =>
          // example:  http://oval.mitre.org/XMLSchema/oval-system-characteristics-5#windows => windows-system-characteristics-schema.xsd
          // there is no "<family>-results" namespaces so anything except definitions is system-characteristics
          val localType = x match {
            case t if t.contains("oval-definitions") => "definitions"
            case _ => "system-characteristics"
          }
          logger.info(s"Family namespace found: $y-$localType")
          new StreamSource(s"$ovalVersion/$y-$localType-schema.xsd")

        /** Invalid namespace with more than one # */
        case x :: y :: xs =>
          throw new Error(s"Invalid OVAL namespace with two sharps ('#'): $ns")
      }
    } }.toArray

    /** XML Validation through javax.xml packages */

    val schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val schema = schemaFactory.newSchema(namespaces)

    val validator = schema.newValidator()

    validator.validate(new StreamSource(path))

    logger.info("Validation complete")
    true
  }
}
