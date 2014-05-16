package org.jff.async

import org.jff.async.AsyncOps._
import org.jff.async.jAsyncOpHelpers._
import org.jff.async.AsyncOpHandlerTask._
import org.jff.utils.blocks.jUtilsBlockDefinitions
import org.jff.async.helpers.AsyncOpHandlerBlockHolder
import org.jff.async.errors.JAsyncOpAbstractFinishError
import org.jff.async.errors.JAsyncOpFinishedByCancellationError
import scala.collection.mutable.ArrayBuffer

object JAsyncOpContinuity {

  def sequenceOfAsyncOperations[R](loader1: Async[Any], loader2: Async[R]): Async[R] = {

    def firstBinder(anyVal: Any): Async[Any] = {
      loader1
    }
    def secondBinder(anyVal: Any): Async[R] = {
      loader2
    }

    val binder:AsyncBinder[Any, R] = bindSequenceOfBindersPair(firstBinder, secondBinder)
    binder(null)
  }

  def sequenceOfAsyncOperations[R](loader1: Async[Any], loader2: Async[Any], loader3: Async[R]): Async[R] = {
    val loader12 = sequenceOfAsyncOperations(loader1, loader2)
    sequenceOfAsyncOperations(loader12, loader3)
  }

  def trySequenceOfAsyncOperations[R](loader1: Async[R], loader2: Async[R]): Async[R] = {

    def firstBinder(anyVal: Any): Async[R] = {
      loader1
    }
    def secondBinder(anyVal: Any): Async[R] = {
      loader2
    }

    val binder:AsyncBinder[Any, R] = bindTrySequenceOfBindersPair(firstBinder, secondBinder)
    binder(null)
  }

  def trySequenceOfAsyncOperationsArray[R](loaders: Array[Async[R]]): Async[R] = {

    def asyncToBinder(loader: Async[R]): AsyncBinder[Error, R] = {
      def binder(result: Error): Async[R] = {
        loader
      }
      binder
    }
    val binders = loaders.map(asyncToBinder)

    val binder = bindTrySequenceOfAsyncOperationsArray(binders.toArray)
    binder(null)
  }

  def bindSequenceOfAsyncOperations[R1, R2](firstLoader: Async[R1], firstBinder: AsyncBinder[R1, R2]): Async[R2] = {
    
    val firstBinderLoader = (result: Any) => firstLoader
    val binder: AsyncBinder[Any, R2] = bindSequenceOfBindersPair(firstBinderLoader, firstBinder)
    binder(null)
  }

  /*TODO implement
  def bindAsyncOperations[R1, R2](firstLoader: Async[R1],
                                  firstBinder: AsyncBinder[jUtilsBlockDefinitions.OptionResult[R1], R2]): Async[R2] = {

    val firstBinderLoader = (result: Any) => firstLoader

    val binder: AsyncBinder[Any, R2] = bindSequenceOfBindersPair(firstBinderLoader, firstBinder)
    binder(null)
  }*/

  def binderAsSequenceOfBindersArray[T](binders: Array[AsyncBinder[T, T]]): AsyncBinder[T, T] = {

    if (binders.size == 0) {

      throw new IllegalArgumentException
    }

    mergeBinders(bindSequenceOfBindersPair[T, T, T], binders)
  }
  
  def bindTrySequenceOfAsyncOperationsArray[R](binders: Array[AsyncBinder[Error, R]]): AsyncBinder[Error, R] = {

    var firstBinder = binders(0)
    if (binders.length > 0) {
      for (index <- 1 to binders.length - 1) {
        val secondBinder = binders(index)
        firstBinder = bindTrySequenceOfBindersPair(firstBinder, secondBinder)
      }
    }

    firstBinder
  }
  
  def bindTrySequenceOfAsyncOperations[R](loader: Async[R], binder: AsyncBinder[Error, R]): Async[R] = {
    
    val binderLoader: AsyncBinder[Any, R] = (result: Any) => loader
    
    val resBinder = bindTrySequenceOfBindersPair[Any, R](binderLoader, binder)
    resBinder(null)
  }

  def groupOfAsyncOperations[R1, R2](firstLoader: Async[R1], secondLoader: Async[R2]): Async[(R1, R2)] = {
    groupOfAsyncOperationsPair(firstLoader, secondLoader)
  }

  def groupOfAsyncOperationsArray[R](asyncs: Array[Async[R]]): Async[ArrayBuffer[R]] = {

    if (asyncs.length == 0) {
      throw new IllegalArgumentException("asyncs should not be empty")
      return null
    }

    def resultToMutableArrayForLoader(async: Async[R]): Async[ArrayBuffer[R]] = {
      bindSequenceOfAsyncOperations(async, (value: R) => {
        val result = new ArrayBuffer[R]
        result += value
        asyncOperationWithResult(result)
      })
    }

    def pairToMutableArrayForLoader(async: Async[(ArrayBuffer[R], R)]): Async[ArrayBuffer[R]] = {
      bindSequenceOfAsyncOperations(async, (value: (ArrayBuffer[R], R)) => {
        value._1 += value._2
        asyncOperationWithResult(value._1)
      })
    }

    /*def mutArrayToArrayLoader(async: Async[ArrayBuffer[R]]): Async[Array[R]] = {
      bindSequenceOfAsyncOperations(async, (value: ArrayBuffer[R]) => {
        asyncOperationWithResult(value.toArray[R])
      })
    }*/

    val firstBlock = asyncs(0)
    var arrayFirstBlock = resultToMutableArrayForLoader(firstBlock)

    for (index <- 1 to asyncs.length - 1) {
      val loader = groupOfAsyncOperations(arrayFirstBlock, asyncs(index))
      arrayFirstBlock = pairToMutableArrayForLoader(loader)
    }

    //mutArrayToArrayLoader(arrayFirstBlock)
    arrayFirstBlock
  }

  private type TwoBindersMerger[P1, R1, R2] = (AsyncBinder[P1, R1], AsyncBinder[R1, R2]) => AsyncBinder[P1, R2]

  private def groupOfAsyncOperationsPair[R1, R2](firstLoader: Async[R1], secondLoader: Async[R2]): Async[(R1, R2)] = {
    
    if (firstLoader == null) {
      throw new IllegalArgumentException("firstLoader is empty")
    }
    
    if (secondLoader == null) {
      throw new IllegalArgumentException("secondLoader is empty")
    }

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[(R1, R2)]): AsyncHandler = {
      
      var loaded = false
      var errorHolder: Error = null

      var result1: Option[R1] = None
      var result2: Option[R2] = None

      var blockCanceledOrUnsubscribed = false
      
      var finishTask = AsyncOpHandlerTaskUndefined

      var handlerHolder1 = new AsyncOpHandlerBlockHolder
      var handlerHolder2 = new AsyncOpHandlerBlockHolder

      type ResultSetter[T] = (T) => Unit
      def resultSetter1(value: R1) {
        result1 = Some(value)
      }
      def resultSetter2(value: R2) {
        result2 = Some(value)
      }

      def makeResultHandler[RT](index: Int, resultSetter: ResultSetter[RT]): AsyncFinishCallback[RT] = {
        
        def resFinishCallback(result: jUtilsBlockDefinitions.OptionResult[RT]) {

          if (!blockCanceledOrUnsubscribed) {
            result.fold({
              case error: JAsyncOpAbstractFinishError =>

                val otherHandlerHolder = if (index == 0) handlerHolder2 else handlerHolder1

                blockCanceledOrUnsubscribed = true

                if (otherHandlerHolder != null) {

                  finishTask = if (error.isInstanceOf[JAsyncOpFinishedByCancellationError])
                    AsyncOpHandlerTaskCancel
                  else
                    AsyncOpHandlerTaskUnSubscribe
                  otherHandlerHolder.performCancelBlockOnceWithArgument(finishTask)
                }

                handlerHolder1 = null
                handlerHolder2 = null

                if (finish != null) {
                  finish(Left(error))
                }
              case error: Error =>
                if (loaded) {
                  handlerHolder1 = null
                  handlerHolder2 = null
                  if (finish != null)
                    finish(Left(error))
                }
                else
                  errorHolder = error
            }, result => {

              resultSetter(result)

              if (loaded) {
                handlerHolder1 = null
                handlerHolder2 = null

                if (errorHolder != null)
                  finish(Left(errorHolder))
                else {

                  val res = for {
                    res1 <- result1
                    res2 <- result2
                  } yield (res1, res2)

                  finish(Right(res.getOrElse({
                    throw new RuntimeException("should never happen")
                    null
                  })))
                }
              }
            })

            if (index == 0)
              handlerHolder1 = null
            else
              handlerHolder2 = null

            loaded = true
          }
        }
        resFinishCallback
      }
      
      val loaderHandler1 = firstLoader(progress, makeResultHandler[R1](0, resultSetter1))
      
      if (blockCanceledOrUnsubscribed) {
            
        if (finishTask == AsyncOpHandlerTaskUnSubscribe) {

          secondLoader(null, null)
        }
        jStubHandlerAsyncOp
      } else {
        
        if (handlerHolder1 != null) {
          
          handlerHolder1.loaderHandler = loaderHandler1
        }

        val loaderHandler2 = secondLoader(progress, makeResultHandler[R2](1, resultSetter2))
      
        if (blockCanceledOrUnsubscribed) {
            
          jStubHandlerAsyncOp
        } else {
      
          if (handlerHolder2 != null) {
            handlerHolder2.loaderHandler = loaderHandler2
          }
      
          def handler(task: AsyncOpHandlerTask) {
            
            if (handlerHolder1 != null)
              handlerHolder1.performCancelBlockOnceWithArgument(task)
            if (handlerHolder2 != null)
              handlerHolder2.performCancelBlockOnceWithArgument(task)
          }
          handler
        }
      }
    }
    async
  }
  
  private def bindSequenceOfBindersPair[P1, R1, R2](firstBinder: AsyncBinder[P1, R1],
                                                    secondBinder: AsyncBinder[R1, R2]): AsyncBinder[P1, R2] = {
    
    if (firstBinder == null) {
      throw new IllegalArgumentException("firstBinder is empty")
    }

    if (secondBinder == null) {
      throw new IllegalArgumentException("secondBinder is empty")
    }

    (bindResult: P1) => {
      
      val firstLoader = firstBinder(bindResult)
      if (firstLoader == null) {
        throw new IllegalArgumentException("firstLoader is empty")
      }
      
      def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R2]): AsyncHandler = {
        
        var handlerBlockHolder: AsyncHandler = null
        
        var progressCallbackHolder: AsyncProgressCallback   = progress
        var doneCallbackHolder    : AsyncFinishCallback[R2] = finish
        
        def progressCallbackWrapper(progressInfo: Any) {
          
          if (progressCallbackHolder != null)
            progressCallbackHolder(progressInfo)
        }
        
        def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[R2]) {
          
          if (doneCallbackHolder != null) {
                    
            doneCallbackHolder(result)
            doneCallbackHolder = null
          }
                
          progressCallbackHolder = null
          handlerBlockHolder     = null
        }
        
        var finished = false
        
        def fistLoaderDoneCallback(result: jUtilsBlockDefinitions.OptionResult[R1]) {

          result.fold(error => {

            finished = true
            doneCallbackWrapper(Left(error))
          }, result => {

            val secondLoader = secondBinder(result)
            if (secondLoader == null) {
              throw new IllegalArgumentException("secondLoader should not be nil")
            }
            handlerBlockHolder = secondLoader(progressCallbackWrapper, doneCallbackWrapper)
          })
        }
        
        val firstHandler = firstLoader(progressCallbackWrapper, fistLoaderDoneCallback)
        
        if (finished) {
          
          jStubHandlerAsyncOp
        } else {
        
          if (handlerBlockHolder == null) {
          
            handlerBlockHolder = firstHandler
          }

          def handler(task: AsyncOpHandlerTask) {
            val currentHandler = handlerBlockHolder
          
            if (currentHandler != null) {
            
              handlerBlockHolder = null
          
              if (task != AsyncOpHandlerTaskUnSubscribe)
                currentHandler(task)
          
              progressCallbackHolder = null
              doneCallbackHolder     = null
            }
          }
          handler
        }
      }
      async
    }
  }
  
  private def bindTrySequenceOfBindersPair[P, R](firstBinder: AsyncBinder[P, R], secondBinder: AsyncBinder[Error, R]): AsyncBinder[P, R] = {
    
    if (firstBinder == null) {
      throw new IllegalArgumentException("firstBinder is empty")
    }
    
    if (secondBinder == null) {
      throw new IllegalArgumentException("firstBinder is empty")
    }
    
    (binderResult: P) => {
      
      val firstLoader = firstBinder(binderResult)
      if (firstLoader == null) {
        throw new IllegalArgumentException("firstLoader is empty")
      }
      
      var handlerBlockHolder: AsyncHandler = null
      
      def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R]): AsyncHandler = {
        
        var progressCallbackHolder: AsyncProgressCallback  = progress
        var doneCallbackHolder    : AsyncFinishCallback[R] = finish
        
        def progressCallbackWrapper(progressInfo: Any) {
          
          if (progressCallbackHolder != null)
            progressCallbackHolder(progressInfo)
        }
        
        def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[R]) {
          
          if (doneCallbackHolder != null) {
                    
            doneCallbackHolder(result)
            doneCallbackHolder = null
          }

          progressCallbackHolder = null
          handlerBlockHolder     = null
        }
        
        val firstHandler = firstLoader(progressCallbackWrapper, (result: jUtilsBlockDefinitions.OptionResult[R]) => {

          result.fold({

            case error: JAsyncOpFinishedByCancellationError =>
              doneCallbackWrapper(Left(error))
            case error: Error =>
              val secondLoader = secondBinder(error)
              handlerBlockHolder = secondLoader(progressCallbackWrapper, doneCallbackWrapper)
          }, result => {

            doneCallbackWrapper(Right(result))
          })
        })
        
        if (handlerBlockHolder == null)
          handlerBlockHolder = firstHandler
        
        (task: AsyncOpHandlerTask) => {
          
          if (handlerBlockHolder != null) {
            
            val currentHandler = handlerBlockHolder
            
            handlerBlockHolder = null
                
            if (task != AsyncOpHandlerTaskUnSubscribe) {
              currentHandler(task)
            }

            progressCallbackHolder = null
            doneCallbackHolder     = null
          }
        }
      }
      async
    }
  }
  
  private def mergeBinders[T](merger: TwoBindersMerger[T, T, T], binders: Array[AsyncBinder[T, T]]): AsyncBinder[T, T] = {
    
    var firstBinder = binders(0)
    
    if (binders.length > 0) {
      
      for( index <- 1 to binders.length - 1 ) {
      
        val secondBinder = binders(index)
        firstBinder = merger(firstBinder, secondBinder)
      }
    }
    
    firstBinder
  }
}
