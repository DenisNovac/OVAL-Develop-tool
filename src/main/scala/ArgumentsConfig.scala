import org.rogach.scallop._

class ArgumentsConfig(args: Seq[String]) extends ScallopConf(args) {
  val xml = opt[String]( required = true, descr = "Path to OVAL xml file")
  verify()
}
