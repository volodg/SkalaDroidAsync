package org.jff.social.facebook.errors

import android.content.Context

class FacebookAuthorizeError extends FBError

class FacebookAuthorizeErrorWithException(val exc: Exception) extends FacebookAuthorizeError {

  override def errorLogDescription(context: Option[Context]): String = {

    val result = super.errorLogDescription(context)
    val excDescription = exc.toString
    s"$result, exception: $excDescription"
  }
}