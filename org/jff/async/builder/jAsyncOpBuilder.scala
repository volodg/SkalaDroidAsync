package org.jff.async.builder

import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpHandlerTask._
import org.jff.async.jAsyncOpHelpers
import org.jff.async.errors.JAsyncOpAbstractFinishError
import org.jff.utils.blocks.jUtilsBlockDefinitions

object jAsyncOpBuilder {

  type AsyncOpInstanceBuilder[T] = () => AsyncOpInterface[T]

  def buildAsyncOpWithInstanceBuilder[T](objectFactory: AsyncOpInstanceBuilder[T]): Async[T] = {

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[T]): AsyncHandler = {

      var asyncObject = objectFactory()

      var progressCallbackHolder = progress
      var finishCallbackHolder = finish

      def onceFinishCallback(result: jUtilsBlockDefinitions.OptionResult[T]): Unit = {

        if (asyncObject == null)
          return

        val finish = finishCallbackHolder
        finishCallbackHolder = null

        if (finish != null) {

          finish(result)
        }

        asyncObject = null
      }
      
      val executer = jAsyncOpHelpers.sameThreadActionExecutor()

      def completionHandler(result: jUtilsBlockDefinitions.OptionResult[T]): Unit = {

        if (asyncObject == null)
          return

        progressCallbackHolder = null
        
        executer(() => {
          
          onceFinishCallback(result)
        })
      }
      
      def progressHandler(progressInfo: Any): Unit = {
        
        if (asyncObject == null) {
          
          return
        }

        val progress = progressCallbackHolder

        executer(() => {
          
          if (progress != null)
            progress(progressInfo)
        })
      }
      
      asyncObject.asyncOpWithCallbacks(progressHandler, completionHandler)
      
      def handler(task: AsyncOpHandlerTask) : Unit = {
        
        if (finishCallbackHolder == null)
          return
        
        asyncObject.doTask(task)
        
        val error = JAsyncOpAbstractFinishError.createErrorWithAsyncHandlerTask(task)
        completionHandler(Left(error))
      }

      handler
    }
    async
  }
}