package org.jff.async.builder

import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpHandlerTask._

trait AsyncOpInterface[T] {
  
  def asyncOpWithCallbacks(progress: AsyncProgressCallback, finish: AsyncFinishCallback[T]): Unit
  
  def doTask(task: AsyncOpHandlerTask): Unit
}
