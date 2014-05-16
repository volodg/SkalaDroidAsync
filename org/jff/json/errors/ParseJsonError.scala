package org.jff.json.errors

import android.content.Context

class ParseJsonError extends JsonToolsError {

  var exception: Exception = null
  var data     : String    = null
  var context  : Any       = null

  override def errorLogDescription(context: Option[Context]): String = {

    val excDescription = if (exception != null) exception.toString else ""
    val dataStr        = if (data      != null) new String(data)   else ""
    val contextStr     = if (context   != null) context.toString   else ""
    s"$toString, exception: $excDescription, data: $dataStr, context: $contextStr"
  }
}