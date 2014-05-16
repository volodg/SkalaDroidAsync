package org.jff.async.cached

import org.jff.async.AsyncOps.{AsyncProgressCallback, AsyncFinishCallback, AsyncHandler, Async, jStubHandlerAsyncOp}
import scala.collection.mutable
import org.jff.async.AsyncOpHandlerTask._
import org.jff.async.errors.JAsyncOpFinishedByUnSubscribeError
import org.jff.utils.blocks.jUtilsBlockDefinitions

private class ObjectRelatedPropertyData[T] {

  var delegates    : mutable.ArrayBuffer[CallbacksBlocksHolder[T]] = null
  var loaderHandler: AsyncHandler = null
  var asyncLoader  : Async[T] = null
  var finish       : AsyncFinishCallback[T] = null
}

private class CallbacksBlocksHolder[CT](progressParam: AsyncProgressCallback, finishParam: AsyncFinishCallback[CT]) {

  var progress = progressParam
  var finish   = finishParam

  def clearCallbacks() {

    progress = null
    finish   = null
  }
}

object CachedAsyncOpTypes {
  type ResultSetter[T] = (T) => Unit
  type ResultGetter[T] = ( ) => Option[T]
}

class CachedAsyncOp[Key, Value] {

  private val delegatesByKey = new mutable.HashMap[Key, ObjectRelatedPropertyData[Value]]()

  private type PropertyExtractorType = PropertyExtractor[Key, Value]

  private def clearDelegates(delegates: mutable.ArrayBuffer[CallbacksBlocksHolder[Value]]) {
    delegates.foreach((callbacks: CallbacksBlocksHolder[Value]) => {
      callbacks.clearCallbacks()
    })
  }

  private def clearDataForPropertyExtractor(propertyExtractor: PropertyExtractorType) {
    if (propertyExtractor.cacheObject == null)
      return
    clearDelegates(propertyExtractor.getDelegates)
    propertyExtractor.setDelegates(null)
    propertyExtractor.setLoaderHandler(null)
    propertyExtractor.setFinishBlock(null)
    propertyExtractor.setAsyncLoader(null)
    propertyExtractor.clear()
  }

  private def cancelBlock(propertyExtractor: PropertyExtractorType, callbacks: CallbacksBlocksHolder[Value]): AsyncHandler = {

    def result(task: AsyncOpHandlerTask) {

      val handler = propertyExtractor.getLoaderHandler
      if (handler == null)
        return

      task match {

        case AsyncOpHandlerTaskUnSubscribe =>

          val didLoadDataBlock = callbacks.finish
          propertyExtractor.getDelegates -= callbacks
          callbacks.clearCallbacks()

          if (didLoadDataBlock != null)
            didLoadDataBlock(Left(new JAsyncOpFinishedByUnSubscribeError))

        case AsyncOpHandlerTaskCancel =>

          handler(AsyncOpHandlerTaskCancel)
          clearDataForPropertyExtractor(propertyExtractor)//TODO should be already cleared here in finish callback

        case _ => throw new IllegalArgumentException()
      }
    }
    result
  }

  private def doneCallbackBlock(propertyExtractor: PropertyExtractorType): AsyncFinishCallback[Value] = {

    def result(result: jUtilsBlockDefinitions.OptionResult[Value]) {

      val delegates = propertyExtractor.getDelegates
      if (delegates != null) {

        var asyncResult = result

        val copyDelegates = delegates.map((callbacks: CallbacksBlocksHolder[Value]) => {//

          new CallbacksBlocksHolder(callbacks.progress, callbacks.finish)
        })

        val finishBlock = propertyExtractor.getFinishBlock

        result.fold(error => {

          propertyExtractor.setAsyncResult(None)
        }, result => {

          propertyExtractor.setAsyncResult(Some(result))
        })

        if (finishBlock != null) {

          finishBlock(asyncResult)
          propertyExtractor.getAsyncResult.foreach(value => asyncResult = Right(value))
        }

        clearDataForPropertyExtractor(propertyExtractor)

        copyDelegates.foreach((delegate: CallbacksBlocksHolder[Value]) => {

          if (delegate.finish != null) {
            delegate.finish(asyncResult)
          }
        })

        clearDelegates(copyDelegates)
      }
    }
    result
  }

  private def performNativeLoader(propertyExtractor: PropertyExtractorType, callbacks: CallbacksBlocksHolder[Value]): AsyncHandler = {

    def progressCallback(progressInfo: Any) {
      propertyExtractor.getDelegates.foreach((delegate: CallbacksBlocksHolder[Value]) => {
        if (delegate.progress != null) {
          delegate.progress(progressInfo)
        }
      })
    }

    val doneCallback = doneCallbackBlock(propertyExtractor)

    val handler = propertyExtractor.getAsyncLoader(progressCallback, doneCallback)

    if (propertyExtractor.cacheObject == null)
      return jStubHandlerAsyncOp

    propertyExtractor.setLoaderHandler(handler)

    cancelBlock(propertyExtractor, callbacks)
  }

  protected def isLoadingDataForUniqueKey(uniqueKey: Key): Boolean = {

    delegatesByKey.get(uniqueKey).fold(false)(_ => true)
  }

  def asyncOpWithPropertySetterGetterKeyAndLoader(setter: CachedAsyncOpTypes.ResultSetter[Value],
                                                  getter: CachedAsyncOpTypes.ResultGetter[Value],
                                                  uniqueKey: Key,
                                                  loader: Async[Value]): Async[Value] = {

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Value]) : AsyncHandler = {

      val propertyExtractor = new PropertyExtractorType(setter, getter, this, uniqueKey, loader)
      val result = propertyExtractor.getAsyncResult

      result.fold({

        val callbacks = new CallbacksBlocksHolder[Value](progress, finish)

        if (null == propertyExtractor.getDelegates) {
          propertyExtractor.setDelegates(mutable.ArrayBuffer[CallbacksBlocksHolder[Value]](callbacks))
          performNativeLoader(propertyExtractor, callbacks)
        } else {
          propertyExtractor.getDelegates += callbacks
          cancelBlock(propertyExtractor, callbacks)
        }
      })(resValue => {

        if (finish != null)
          finish(Right(resValue))

        propertyExtractor.clear()
        jStubHandlerAsyncOp
      })
    }
    async
  }

  private class PropertyExtractor[KeyT, ValueT](setterArg: CachedAsyncOpTypes.ResultSetter[ValueT],
                                                getterArg: CachedAsyncOpTypes.ResultGetter[ValueT],
                                                objectArg: CachedAsyncOp[KeyT, ValueT],
                                                uniqueKeyArg: KeyT,
                                                loaderArg: Async[ValueT]) {

    type PropertyExtractorSetter = (ValueT) => Unit
    type PropertyExtractorGetter = () => Option[ValueT]

    var setter      = setterArg
    var getter      = getterArg
    var cacheObject = objectArg
    var uniqueKey   = uniqueKeyArg

    setAsyncLoader(loaderArg)

    private def getObjectRelatedPropertyData: ObjectRelatedPropertyData[ValueT] = {

      cacheObject.delegatesByKey.get(uniqueKey).fold({

        val properties = new ObjectRelatedPropertyData[ValueT]
        cacheObject.delegatesByKey.put(uniqueKey, properties)
        properties
      })(el => el)
    }

    def getDelegates: mutable.ArrayBuffer[CallbacksBlocksHolder[ValueT]] = {

      getObjectRelatedPropertyData.delegates
    }

    def setDelegates(delegates: mutable.ArrayBuffer[CallbacksBlocksHolder[ValueT]]): Unit = {

      getObjectRelatedPropertyData.delegates = delegates
    }

    def getLoaderHandler: AsyncHandler = {

      getObjectRelatedPropertyData.loaderHandler
    }

    def setLoaderHandler(handler: AsyncHandler) {

      getObjectRelatedPropertyData.loaderHandler = handler
    }

    def getAsyncLoader: Async[ValueT] = {

      getObjectRelatedPropertyData.asyncLoader
    }

    def setAsyncLoader(loader: Async[ValueT]) {

      getObjectRelatedPropertyData.asyncLoader = loader
    }

    def getFinishBlock: AsyncFinishCallback[ValueT] = {

      getObjectRelatedPropertyData.finish
    }

    def setFinishBlock(finish: AsyncFinishCallback[ValueT]) {

      getObjectRelatedPropertyData.finish = finish
    }

    def getAsyncResult: Option[ValueT] = {

      if (getter != null) {
        getter()
      } else {
        None
      }
    }

    def setAsyncResult(result: Option[ValueT]): Unit = {

      if (setter != null) {
        result.foreach(value => setter(value))
      }
    }

    def clear() {

      if (cacheObject != null)
        cacheObject.delegatesByKey.remove(uniqueKey)

      setter      = null
      getter      = null
      cacheObject = null
    }
  }
}
