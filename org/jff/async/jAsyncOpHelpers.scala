package org.jff.async

import org.jff.async.AsyncOps._

import org.jff.utils.blocks.jUtilsBlockDefinitions._

import android.os.Looper
import android.os.Handler
import org.jff.utils.blocks.jUtilsBlockDefinitions

object jAsyncOpHelpers {
  
  type ActionBlockExecutor = (ActionBlock) => Unit

  def asyncOperationWithResult[R](result: R): Async[R] = {
    asyncOperationWithOptionResult(Right(result))
  }
  
  def asyncOperationWithError[T](error: Error): Async[T] = {
    asyncOperationWithOptionResult(Left(error))
  }

  def asyncOperationWithOptionResult[T](optionResult: jUtilsBlockDefinitions.OptionResult[T]): Async[T] = {
    assert(optionResult != null)
    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[T]) : AsyncHandler = {
      if (finish != null)
        finish(optionResult)
      jStubHandlerAsyncOp
    }
    async
  }

  def asyncOperationWithFinishHookBlock[R1, R2](loader: Async[R1], finishCallbackHook: FinishAsyncOpHook[R1, R2]): Async[R2] = {

    if (loader == null)
      throw new NullPointerException

    if (finishCallbackHook == null)
      throw new NullPointerException

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R2]): AsyncHandler = {
      loader(progress, (result: jUtilsBlockDefinitions.OptionResult[R1]) => {
        finishCallbackHook(result, finish)
      })
    }
    async
  }

  def asyncOperationWithDoneBlock[R](loader: Async[R], doneCallbackHook: jUtilsBlockDefinitions.ActionBlock): Async[R] = {

    if (doneCallbackHook == null)
      return loader

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R]): AsyncHandler = {
      def wrappedDoneCallback(result: jUtilsBlockDefinitions.OptionResult[R]) {
        doneCallbackHook()
        if (finish != null)
          finish(result)
      }
      loader(progress, wrappedDoneCallback)
    }
    async
  }

  def ignoreProgressLoader[R](loader: Async[R]): Async[R] = {

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R]): AsyncHandler = {
      loader(null, finish)
    }
    async
  }

  def asyncOperationWithAnalyzer[T1, T2](data: T1, analyzer: Analyzer[T1, T2]): Async[T2] = {
    
    if (data == null || analyzer == null)
      throw new IllegalArgumentException()
    
    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[T2]) : AsyncHandler = {
      val resultOrError = analyzer(data)
      if (finish != null) {
        finish(resultOrError)
      }
      jStubHandlerAsyncOp
    }
    async
  }

  def asyncOperationBinderWithAnalyzer[T1, T2](analyzer: Analyzer[T1, T2]): AsyncBinder[T1, T2] = {
    def binder(result: T1): Async[T2] = {
      asyncOperationWithAnalyzer(result, analyzer)
    }
    binder
  }

  def sameThreadActionExecutor(): ActionBlockExecutor = {
    
    val currentLooper = Looper.myLooper()
    assert(currentLooper != null)//TODO implement for not main thread
    
    def result(action: ActionBlock): Unit = {
      
      val currLooper = Looper.myLooper()
      val isForeignThread = currentLooper != currLooper
      
      if (!isForeignThread) {
        action()
      } else {
        val mainHandler = new Handler(currentLooper)
        mainHandler.post(new Runnable {
          override def run() {
            action()
          }
        })
      }
    }
    result
  }
}