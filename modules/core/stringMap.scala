/* -
 * Case Classy [classy-core]
 */

package classy
package read

import java.io.InputStream
import java.util.Properties

object stringMap {

  type StringMap     = scala.collection.Map[String, String]
  type JavaStringMap = java.util.Map[String, String]

  implicit val stringMapReadString: Read[StringMap, String] =
    Read.instance(decoders.stringMapToString)

  implicit val stringMapReadNested: Read[StringMap, StringMap] =
    Read.instance(decoders.stringMapToStringMap)

  val readStringMap = Read.from[StringMap]

  implicit class StringMapDecoderOps[A](
    private val decoder: Decoder[StringMap, A]
  ) extends AnyVal {

    import scala.collection.convert.{ Wrappers => wrap }

    /** Converts this decoder to a decoder that parses a
      * `java.util.Map[String, String]` instead of a `Map[String, String]`
      *
      * @group stringMap
      */
    def fromJavaStringMap: Decoder[JavaStringMap, A] =
      decoder.mapInput(javaMap => wrap.JMapWrapper(javaMap))

    /** Converts this decoder to a decoder that parses
      * `java.util.Properties` instead of a `Map[String, String]`
      *
      * @group stringMap
      */
    def fromProperties: Decoder[Properties, A] =
      decoder.mapInput(properties => wrap.JPropertiesWrapper(properties))
  }

  implicit class PropertiesDecoderOps[A](
    private val decoder: Decoder[Properties, A]
  ) extends AnyVal {

    def fromInputStream: Decoder[InputStream, A] =
      decoder <<< decoders.inputStreamToProperties
  }
}

import scala.Predef._
object DOOFUS extends App {

  import java.util.UUID

  val data = Map(
    "a" -> "2000.0",
    "b" -> "true",
    "c" -> "123e4567-e89b-12d3-a456-426655440000"
  )

  import stringMap.{ readStringMap => read, _ }

  case class Doof(a: Float, b: Boolean, c: UUID)

  val decoder = (
    read[Float]("a") join
    read[Boolean]("b") join
    read[UUID]("c")
  ).map(Doof.tupled)


  decoders.fileToInputStream       >>>
  decoders.inputStreamToProperties >>>
  decoder.fromProperties


  val d = decoder.fromProperties.fromInputStream

  println("> " + decoder(data))

}
