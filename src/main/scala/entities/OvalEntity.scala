package entities
import scala.reflect.runtime.universe._

trait OvalEntity {
  def id: String
  def path: String
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



abstract class Factory[T <: OvalEntity] {
  def createT(id: String, path: String): T
}

