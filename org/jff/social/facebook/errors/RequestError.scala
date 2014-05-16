package org.jff.social.facebook.errors

import com.facebook.FacebookRequestError
import android.content.Context

class RequestError(val error: FacebookRequestError) extends FBError {

  override def errorLogDescription(context: Option[Context]): String = {

    val fbError = error.toString
    s"$toString : fbError : $fbError"
  }
}
