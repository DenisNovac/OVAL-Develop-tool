import validation._
import composer._

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new ArgumentsConfig(args)
    val path = conf.xml()
    //OvalValidator.validate(path)
    //OvalDecomposer.decompose(path)
    OvalIndexer.createIndex()
  }

}

