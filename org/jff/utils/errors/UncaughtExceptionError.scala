package org.jff.utils.errors

import android.content.Context

class UncaughtExceptionError(exception: Throwable) extends JError {

  override def errorLogDescription(context: Option[Context]): String = {

    val excDescription = exception.toString
    s"$toString : exception : $excDescription"
  }
}
