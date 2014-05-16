package org.jff.network

import java.net.URL
import scala.collection.immutable
import java.io.InputStream

object JURLConnectionParams {

  type ValueType  = Either[String, Array[String]]
  type EntityType = immutable.Map[String, ValueType]

  class UploadData(dataStreamArg: InputStream, dataLengthArg: Option[Long]) {

    val dataStream: InputStream  = dataStreamArg
    val dataLength: Option[Long] = dataLengthArg
  }
}

class JURLConnectionParams(urlArg: URL) {

  import JURLConnectionParams._

  val url = urlArg
  var entity    : Option[EntityType] = None
  var httpMethod: String = "GET"

  var headers   : Option[immutable.Map[String, String]]   = None
  def appendHeaders(headers: immutable.Map[String, String]) {
    this.headers.fold(this.headers = Some(headers))(originalHeaders => {
      this.headers = Some(originalHeaders ++ headers)
    })
  }

  var uploadData: Option[UploadData] = None//TODO change to Factory
}
