package composer

import java.io.File
import com.typesafe.scalalogging.Logger

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
}
