package org.jff.async

import org.jff.async.AsyncOps._
import org.jff.async.builder.AsyncOpInterface
import org.jff.async.AsyncOpHandlerTask._
import org.jff.async.builder.jAsyncOpBuilder._
import org.jff.async.JAsyncOpContinuity._
import org.jff.utils.errors.UncaughtExceptionError
import android.util.Log
import org.jff.utils.blocks.jUtilsBlockDefinitions
import org.jff.async.errors.JAsyncOpFinishedByCancellationError
import org.jff.utils.blocks.jUtilsBlockDefinitions.ActionBlock
import org.jff.timer.SameThreadTimer

object AsyncOpUtils {

  private class AsyncOperationAdapter[T] extends AsyncOpInterface[T] {
    
    var syncOp: SyncOp[T] = null
    
    private var finish  : AsyncFinishCallback[T] = null
    private var progress: AsyncProgressCallback  = null
  
    private var thread: Thread = null
  
    def asyncOpWithCallbacks(progress: AsyncProgressCallback, finish: AsyncFinishCallback[T]): Unit = {
      
      this.progress = progress
      this.finish   = finish

      val self = this

      thread = new Thread() {

        override def run() {

          def throwable: jUtilsBlockDefinitions.OptionResult[T] = {

            try {
              syncOp(progress)
            } catch {
              case ex: Throwable =>
                Log.e("jAsync", ex.toString)
                Left(new UncaughtExceptionError(ex))
            }
          }

          val finish = self.finish

          val res = throwable

          if (finish != null)
            finish(res)
        }
      }
      thread.start()
    }
    
    def doTask(task: AsyncOpHandlerTask): Unit = {
      
    val thread = this.thread
    
    this.finish   = null
    this.progress = null
    this.thread   = null
    
    if (task == AsyncOpHandlerTaskCancel)
      thread.interrupt()
    }
  }
  
  def asyncOperationWithSyncOperation[T](syncOp: SyncOp[T]): Async[T] = {
    
    if (syncOp == null) {
      
      throw new IllegalArgumentException("syncOp should not be nul")
    }
    
    def factory() : AsyncOpInterface[T] = {
      
      val asyncObj = new AsyncOperationAdapter[T]()
      asyncObj.syncOp = syncOp
      asyncObj
    }
    
    buildAsyncOpWithInstanceBuilder(factory)
  }

  type ContinueLoaderWithResult[T] = jUtilsBlockDefinitions.OptionResult[T] => Option[Async[T]]

  def repeatAsyncOperation[R](nativeLoader: Async[R],
                              continueLoaderBuilder: ContinueLoaderWithResult[R],
                              maxRepeatCount: Int): Async[R] = {

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R]): AsyncHandler = {

      var currentLoaderHandlerHolder: AsyncHandler = null
      var finishHookHolder: FinishAsyncOpHook[R, R] = null

      var progressCallbackHolder = progress
      var doneCallbackHolder     = finish

      def progressCallbackWrapper(progressInfo: Any) {

        if (progressCallbackHolder != null)
          progressCallbackHolder(progressInfo)
      }
      def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[R]) {

        if (doneCallbackHolder != null) {
          doneCallbackHolder(result)
          doneCallbackHolder = null
        }
      }

      var currentLeftCount = maxRepeatCount

      def finishCallbackHook(result: jUtilsBlockDefinitions.OptionResult[R], doneCallback: AsyncFinishCallback[R]) {

        def finish() {

          finishHookHolder = null
          doneCallbackWrapper(result)

          progressCallbackHolder = null
          doneCallbackHolder     = null
        }

        def processResult() {

          val newLoader = continueLoaderBuilder(result)

          newLoader.fold({

            finish()
          })(newLoader => {

            if (currentLeftCount == 0) {

              finish()
            } else {

              currentLeftCount = if (currentLeftCount > 0) currentLeftCount - 1 else currentLeftCount
              val loader = jAsyncOpHelpers.asyncOperationWithFinishHookBlock(newLoader, finishHookHolder)
              currentLoaderHandlerHolder = loader(progressCallbackWrapper, doneCallbackWrapper)
            }
          })
        }

        result.fold({

          case error: JAsyncOpFinishedByCancellationError =>
            finishHookHolder = null
            doneCallbackWrapper(result)

            progressCallbackHolder = null
            doneCallbackHolder     = null
          case _ =>
            processResult()
        }, result => {
          processResult()
        })
      }

      finishHookHolder = finishCallbackHook

      val loader = jAsyncOpHelpers.asyncOperationWithFinishHookBlock(nativeLoader, finishHookHolder)
      currentLoaderHandlerHolder = loader(progress, doneCallbackWrapper)

      var unsubscribed = false

      def handler(task: AsyncOpHandlerTask) {

        if (currentLoaderHandlerHolder != null && !unsubscribed) {

          task match {

            case AsyncOpHandlerTask.AsyncOpHandlerTaskCancel =>
              finishHookHolder = null
              currentLoaderHandlerHolder(task)
              currentLoaderHandlerHolder = null
            case AsyncOpHandlerTask.AsyncOpHandlerTaskUnSubscribe =>
              progressCallbackHolder = null
              doneCallbackHolder     = null
              unsubscribed = true
          }
        }
      }
      handler
    }
    async
  }

  def repeatAsyncOperation[R](nativeLoader: Async[R],
                              continueLoaderBuilder: ContinueLoaderWithResult[R],
                              delay: Long,
                              maxRepeatCount: Int): Async[R] = {

    def continueLoaderBuilderWrapper(result: jUtilsBlockDefinitions.OptionResult[R]): Option[Async[R]] = {
      val loader = continueLoaderBuilder(result)
      loader.map(loader => sequenceOfAsyncOperations(asyncOperationWithDelay(delay), loader))
    }

    repeatAsyncOperation(nativeLoader, continueLoaderBuilderWrapper, maxRepeatCount)
  }

  def asyncOperationWithDelay(delay: Long): Async[Unit] = {

    class AsyncOperationScheduler extends AsyncOpInterface[Unit] {

      var cancelTimer: Option[ActionBlock] = None

      def doTask(task: AsyncOpHandlerTask.AsyncOpHandlerTask): Unit = {

        cancelTimer.foreach(cancel => {
          cancel()
          this.cancelTimer = None
        })
      }

      def asyncOpWithCallbacks(progress: AsyncOps.AsyncProgressCallback, finish: (jUtilsBlockDefinitions.OptionResult[Unit]) => Unit): Unit = {

        cancelTimer = Some(SameThreadTimer.schedule((cancel) => {
          cancel()
          finish(Right(()))
        }, delay))
      }
    }

    buildAsyncOpWithInstanceBuilder(() => new AsyncOperationScheduler)
  }
}