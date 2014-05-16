package org.jff.utils

import java.util.UUID
import java.io.File
import android.content.Context

object FileUtil {

  def tmpFileName(appContext: Context): String = {

    val appPath = appContext.getFilesDir.getAbsolutePath
    val path = s"$appPath/tmpfiles"

    synchronized {

      val folder = new File(path)
      if (!folder.isDirectory) {
        val ok = folder.mkdirs()
        if (!ok)
          throw new IllegalStateException("make dir error: " + folder)
      }
    }

    val uuid = UUID.randomUUID().toString
    s"$path/$uuid"
  }
}
