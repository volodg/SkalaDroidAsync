package org.jff.utils.errors

import android.content.Context

case class JError(errorDescription: String) extends java.lang.Error(errorDescription) {
  def this() = this(null)
  def errorLogDescription(context: Option[Context]): String = toString
}