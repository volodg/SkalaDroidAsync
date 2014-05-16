package org.jff.network.errors

import org.jff.utils.errors.JError
import android.content.Context

class NetworkError(ioException: Throwable) extends JError {

  override def errorLogDescription(context: Option[Context]): String = {

    val excDescription = ioException.toString
    s"$toString : exception: $excDescription"
  }
}
