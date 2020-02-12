package composer

import java.io.File
import java.lang.System.Logger
import java.nio.file.Paths

import com.typesafe.scalalogging.Logger
import scala.xml.Node

/** Util class for methods, dealing with filesystem and repository folder
  */
object RepositoryUtils {
  private val logger = Logger("Repository Utils")

  /** This method will get folder name for ID */
  def getThousandFolderName(id: Long): String = (id/1000)*1000 match {
    case 0 => "0000"
    case x => x.toString
  }

  /** Recursive folders reading */
  def recursiveListFiles(f: File): Vector[File] = {
    val files = f.listFiles
    files ++: files.filter(_.isDirectory).flatMap(recursiveListFiles).toVector
  }

  /** Complex Oval Objects searcher
    * Complex Oval Objects (from the repository perspective) - is a tests, objects and states. The structure of their
    * storage is complicated: /repository/[OVAL TYPE]/[OVAL FAMILY]/[ENTITY TYPE]/[ID FOLDERS BY THOUSANDS]/file.
    * We know OVAL TYPE by default and OVAL FAMILY from definition (it also may be "independent"). By we have to
    * walk through all ENTITY TYPES to search for ID FOLDERS.
    * @param ovalType - tests, objects, states
    * @param ovalFamily - windows, unix, etc
    * @param id - id of the object
    * @throws Error if repository or file is not found
    * @return path to the file
    */
  def getFilePathForComplexOvalObjects(ovalType: String, ovalFamily: String, id: String): String = {

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


    if (fileById.length != 1) {
      val filesString = fileById.mkString(",")
      val message = s"There is none or too much (${fileById.length}) of suitable OVAL files: $filesString"
      logger.error(message)
      throw new Error(message)
    } else
      fileById.head

  }
}
