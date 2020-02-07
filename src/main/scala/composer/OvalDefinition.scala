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
  * @param platforms Definition's platform (such as Windows 7 Ultimate)
  * @param products Definition's product (Such as Java 8)
  * @param references Definition's references (Such as CVE-2013-5777)
  *
  */

case class OvalDefinition(id: String,
                         path: String,
                         ovalClass: String,
                         title: String,
                         description: String,
                         family: String,
                         platforms: Array[String],
                         products: Array[String],
                         references: Array[String]) {

  override def toString: String =
    s"""
       |$id
       |$path
       |$ovalClass
       |$title
       |$description
       |$family
       |${platforms.mkString(",")}
       |${products.mkString(",")}
       |${references.mkString(",")}
       |""".stripMargin
}

