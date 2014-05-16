package org.jff.utils.errors

import android.content.Context

object ErrorLogDescription {

  def apply(error: Error, context: Option[Context]): Option[String] = {
    error match {
      case error: JSilentError => None
      case error: JError       => Some(error.errorLogDescription(context))
      case error: Error        => Some(error.toString)
    }
  }
}
