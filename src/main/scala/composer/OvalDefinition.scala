package composer

/** Model for OVAL definition
  *
  * It is handy to store definitions as JSON index.
  * @param id OVAL definition id (with ":" in it)
  * @param path Path to file on disk
  * @param ovalClass OVAL Class (Vulnerability, Inventory, etc)
  * @param title Definition's title
  * @param description Definition's description
  * @param family Definition's family (such as Windows)
  * @param platform Definition's platform (such as Windows 7 Ultimate)
  * @param product Definition's product (Such as Java 8)
  * @param referenceId Definition's reference (Such as CVE-2013-5777)
  *
  */

case class OvalDefinition(id: String,
                         path: String,
                         ovalClass: String,
                         title: String,
                         description: String,
                         family: String,
                         platform: Vector[String],
                         product: Vector[String],
                         referenceId: String)

