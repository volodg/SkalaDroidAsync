package org.jff.json

import org.json.JSONArray
import scala.collection.mutable.ArrayBuffer

object JSONTools {

  def JSONArrayToArrayOfObjects[T](value: JSONArray, f: (Int) => T): ArrayBuffer[T] = {

    val result = new ArrayBuffer[T]

    for (i <- 0 to value.length() - 1) {

      result += f(i)
    }

    result
  }
}
