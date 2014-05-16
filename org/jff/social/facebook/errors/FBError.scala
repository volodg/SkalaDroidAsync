package org.jff.social.facebook.errors

import org.jff.utils.errors.JError
import android.content.Context
import android.content.pm.PackageManager
import java.security.MessageDigest
import android.util.Base64

abstract class FBError extends JError {

  private def appSignature(context: Context): String = {

    try {
      val info = context.getPackageManager.getPackageInfo(context.getPackageName, PackageManager.GET_SIGNATURES)
      var result = ""
      info.signatures.foreach(signature => {
        val md = MessageDigest.getInstance("SHA")
        md.update(signature.toByteArray)
        result = Base64.encodeToString(md.digest(), Base64.DEFAULT)
      })
      result
    } catch {
      case _: Throwable => ""
    }
  }

  override def errorLogDescription(context: Option[Context]): String = {

    val result = toString
    context.fold(result)(context => {
      val signature = appSignature(context)
      s"$result, MyAppSignature: $signature"
    })
  }

}