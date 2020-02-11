package composer

import java.io.File

import scala.xml.Node

/** Util class for shared folders methods
  */
object FolderUtils {

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


}
