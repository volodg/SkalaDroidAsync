package org.jff.cache.errors

import android.content.Context
import scala.Predef.String

class CacheLoadImageError(val ex: Throwable) extends CacheError {

  override def errorLogDescription(context: Option[Context]): String = {

    val excDescription = ex.toString
    s"$toString, exception: $excDescription"
  }
}
