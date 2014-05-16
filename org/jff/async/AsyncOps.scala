package org.jff.async

import org.jff.async.AsyncOpHandlerTask.AsyncOpHandlerTask
import org.jff.utils.blocks.jUtilsBlockDefinitions

object AsyncOps {

  type SyncOp[R] = (AsyncProgressCallback) => jUtilsBlockDefinitions.OptionResult[R]
  
  type AsyncHandler = (AsyncOpHandlerTask) => Unit
  
  type AsyncProgressCallback = (Any) => Unit
  
  type AsyncFinishCallback[T] = jUtilsBlockDefinitions.OptionResult[T] => Unit
  
  type Async[R] = (AsyncProgressCallback, AsyncFinishCallback[R]) => AsyncHandler
  /*trait Async extends ((AsyncProgressCallback, AsyncFinishCallback) => AsyncHandler) {

  }*/
  
  type AsyncBinder[P, R] = (P) => Async[R]

  type FinishAsyncOpHook[R1, R2] = (jUtilsBlockDefinitions.OptionResult[R1], AsyncFinishCallback[R2]) => Unit

  val jStubHandlerAsyncOp: (AsyncOpHandlerTask) => Unit = (task: AsyncOpHandlerTask) => {
    //do nothing
  }
}
