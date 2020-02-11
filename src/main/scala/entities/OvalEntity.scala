package entities

trait OvalEntity {
  val id: String
  val path: String
}

case class OvalDefinition (id: String,
                           path: String,
                           ovalClass: String,
                           title: String,
                           description: String,
                           family: String,
                           platforms: Array[String],
                           products: Array[String],
                           references: Array[String]) extends OvalEntity

case class OvalTest(id: String, path: String) extends OvalEntity
case class OvalObject(id: String, path: String) extends OvalEntity
case class OvalState(id: String, path: String) extends OvalEntity
case class OvalVariable(id: String, path: String) extends OvalEntity