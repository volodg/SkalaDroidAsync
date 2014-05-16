package org.jff.ui

import android.app.Activity
import android.content.Intent
import org.jff.utils.WeakReferencedObserver

trait JActivityOnResult {
  def onJActivityResult(requestCode: Int, resultCode: Int, data: Intent): Boolean
}

trait JActivity extends Activity {

  private lazy val visitor = new Object with WeakReferencedObserver[JActivityOnResult]

  def addOnActivityResultObserver(observer: JActivityOnResult, strongReference: Boolean = false) {
    visitor.addObserver(observer, strongReference)
  }

  def removeOnActivityResultObserver(observer: JActivityOnResult) {
    visitor.removeObserver(observer)
  }

  override protected def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) {

    val visited = visitor.visitAll((observer: JActivityOnResult) => {
      observer.onJActivityResult(requestCode, resultCode, data)
    })

    if (!visited) {
      super.onActivityResult(requestCode, resultCode, data)
    }
  }
}
