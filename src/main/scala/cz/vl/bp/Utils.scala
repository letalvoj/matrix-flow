package cz.vl.bp

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object Utils {
  private val mapper = new ObjectMapper()
    .registerModule(DefaultScalaModule)
    .writerWithDefaultPrettyPrinter()

  implicit class JsonSerializer(any: Any) {
    def toJson: String = mapper.writeValueAsString(any)
  }

}
