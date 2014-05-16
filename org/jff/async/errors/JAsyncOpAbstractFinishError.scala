package org.jff.async.errors

import org.jff.utils.errors.JError
import org.jff.async.AsyncOpHandlerTask
import org.jff.async.AsyncOpHandlerTask.AsyncOpHandlerTask

object JAsyncOpAbstractFinishError extends Error {

  def createErrorWithAsyncHandlerTask(task: AsyncOpHandlerTask) : JAsyncOpAbstractFinishError = {
    
    val result = task match {
      case AsyncOpHandlerTask.AsyncOpHandlerTaskUnSubscribe => new JAsyncOpFinishedByUnSubscribeError ()
      case AsyncOpHandlerTask.AsyncOpHandlerTaskCancel      => new JAsyncOpFinishedByCancellationError()
      case _ => throw new IllegalArgumentException("illegal arg: " + task)
    }
    result
  }
}

abstract class JAsyncOpAbstractFinishError extends JError

class JAsyncOpFinishedByUnSubscribeError extends JAsyncOpAbstractFinishError

class JAsyncOpFinishedByCancellationError extends JAsyncOpAbstractFinishError
