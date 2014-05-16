package org.jff.ui

import org.jff.async.{AsyncOpHandlerTask, AsyncOperationsOwner}
import org.jff.async.AsyncOps.{AsyncFinishCallback, AsyncProgressCallback, Async}
import android.support.v4.app.Fragment

trait AsyncFragment extends Fragment {

  lazy protected val allOperationsToCancelOnPause = new AsyncOperationsOwner

  protected def performAutoCancelLoader[T](nativeLoader: Async[T], progress: AsyncProgressCallback, finish: AsyncFinishCallback[T]) {
    val loader = allOperationsToCancelOnPause.ownedAsyncOperation(nativeLoader)
    loader(progress, finish)
  }

  protected override def onPause() {
    super.onPause()
    allOperationsToCancelOnPause.handleAllWithTask(AsyncOpHandlerTask.AsyncOpHandlerTaskUnSubscribe)
  }
}
