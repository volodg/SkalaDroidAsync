package org.jff.network.query

import android.net.Uri
import org.jff.network.JURLConnectionParams

object XQueryComponents {

  private val queryComponentSeparator = "&"

  def stringFromQueryComponentAndKey(queryComponent: String, key: String): String = {

    val value = Uri.encode(queryComponent)
    f"$key%s=$value%s"
  }

  def stringFromQueryComponents(components: JURLConnectionParams.EntityType): String = {

    val resultArr = components.keys.map(key => {

      val encodedKey = Uri.encode(key)

      components.get(key).get.fold(value => {
        Array(stringFromQueryComponentAndKey(value, encodedKey))
      }, values => {
        values.map(value => stringFromQueryComponentAndKey(value, encodedKey))
      })
    }).flatten
    val result = resultArr.mkString(queryComponentSeparator)
    result
  }

  def dataFromQueryComponents(components: JURLConnectionParams.EntityType): Array[Byte] = {

    stringFromQueryComponents(components).getBytes
  }
}
