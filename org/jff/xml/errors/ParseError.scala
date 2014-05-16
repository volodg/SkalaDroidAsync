package org.jff.xml.errors

import android.content.Context
import org.jff.utils.errors.JError

class ParseError(ex: Throwable) extends JError {
  override def errorLogDescription(context: Option[Context]): String = {
    val excDescription = ex.toString
    s"$toString : exception : $excDescription"
  }
}
