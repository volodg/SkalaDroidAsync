package org.jff.restKit

import org.jff.async.AsyncOps._
import org.jff.async.JAsyncOpContinuity._
import org.jff.async.jAsyncOpHelpers._
import java.util.Date
import org.jff.utils.errors.JError
import org.jff.utils.blocks.jUtilsBlockDefinitions

object SmartDataLoader {

  type AsyncBinderForIdentifier[Identifier, Result] = (Identifier) => AsyncBinder[Array[Byte], Result]
  type CacheKeyForIdentifier   [Identifier]         = (Identifier) => String

  class SmartDataLoaderFields[Identifier, Result](val loadDataIdentifier: Identifier,
                                                  val dataLoaderForIdentifier: AsyncBinder[Identifier, Array[Byte]],
                                                  val analyzerForData: AsyncBinderForIdentifier[Identifier, Result],
                                                  val cacheKeyForIdentifier: CacheKeyForIdentifier[Identifier],
                                                  val doesNotIgnoreFreshDataLoadFail: Boolean,
                                                  val cache: RestKitCache,
                                                  val cacheDataLifeTimeInSeconds: Long)

  def jSmartDataLoaderWithCache[Identifier, Result](args: SmartDataLoaderFields[Identifier, Result]): Async[Result] = {

    val loadDataIdentifier             = args.loadDataIdentifier
    val dataLoaderForIdentifier        = args.dataLoaderForIdentifier
    val analyzerForData                = args.analyzerForData
    val cache                          = args.cache
    val cacheKeyForIdentifier          = args.cacheKeyForIdentifier
    val cacheDataLifeTimeInSeconds     = args.cacheDataLifeTimeInSeconds
    val doesNotIgnoreFreshDataLoadFail = args.doesNotIgnoreFreshDataLoadFail

    if (loadDataIdentifier == null)
      throw new NullPointerException

    if (dataLoaderForIdentifier == null)
      throw new NullPointerException

    var key: String = null
    if (cache != null) {

      key = if (cacheKeyForIdentifier != null)
        cacheKeyForIdentifier(loadDataIdentifier)
      else
        loadDataIdentifier.toString
    }

    def cachedDataLoader(progress: AsyncProgressCallback, finish: AsyncFinishCallback[RestKitCachedData]): AsyncHandler = {

      val dataLoaderBinder = dataLoaderWithCachedResultBinder(doesNotIgnoreFreshDataLoadFail,
                                                              dataLoaderForIdentifier,
                                                              loadDataIdentifier)

      val loader = if (cache == null) {
        dataLoaderBinder(null)
      } else {

        val loadCachedData = loadFreshCachedDataWithUpdateDate(key,
                                                               cache.cachedDataLoaderForKey(key),
                                                               cacheDataLifeTimeInSeconds)

        bindTrySequenceOfAsyncOperations(loadCachedData, dataLoaderBinder)
      }

      loader(progress, finish)
    }

    def analyzer(response: RestKitCachedData): Async[Result] = {

      val binder   = analyzerForData(loadDataIdentifier)
      val analyzer = binder(response.data)

      if (cache != null) {

        def cacheBinder(analyzedData: Result): Async[Result] = {

          val resultLoader = asyncOperationWithResult(analyzedData)

          if (response.updateTime == 0) {
            val loader = cache.loaderToSetDataForKey(response.data, key)
            sequenceOfAsyncOperations(loader, resultLoader)
          } else
            resultLoader
        }

        bindSequenceOfAsyncOperations(analyzer, cacheBinder)
      }
      else
        analyzer
    }

    bindSequenceOfAsyncOperations(cachedDataLoader, analyzer)
  }

  private def dataLoaderWithCachedResultBinder[Identifier](doesNotIgnoreFreshDataLoadFail: Boolean,
                                                           dataLoaderForIdentifier: AsyncBinder[Identifier, Array[Byte]],
                                                           loadDataIdentifier: Identifier): AsyncBinder[Error, ResponseDataWithUpdateData] = {

    def binder(bindError: Error): Async[ResponseDataWithUpdateData] = {

      def finishCallbackHook(srvResponse: jUtilsBlockDefinitions.OptionResult[Array[Byte]], doneCallback: AsyncFinishCallback[ResponseDataWithUpdateData]) {

        srvResponse.fold(error => {

          if (doesNotIgnoreFreshDataLoadFail) {

            doneCallback(Left(error))
          } else {

            bindError match {

              case noFreshDataError: ErrorNoFreshData =>
                val newResult = new ResponseDataWithUpdateData(noFreshDataError.cachedData.data, noFreshDataError.cachedData.updateTime())
                doneCallback(Right(newResult))
              case _ =>
                doneCallback(Left(error))
            }
          }
        }, srvResponse => {
          val newResult = new ResponseDataWithUpdateData(srvResponse, 0)
          doneCallback(Right(newResult))
        })
      }
      asyncOperationWithFinishHookBlock(dataLoaderForIdentifier(loadDataIdentifier), finishCallbackHook)
    }
    binder
  }

  private def loadFreshCachedDataWithUpdateDate(key: String,
                                                cachedDataLoader: Async[RestKitCachedData],
                                                cacheDataLifeTimeInSeconds: Long): Async[RestKitCachedData] = {

    def validateByDateResultBinder(cachedData: RestKitCachedData): Async[RestKitCachedData] = {
      val newTime = cachedData.updateTime + cacheDataLifeTimeInSeconds * 1000
      if (newTime > (new Date).getTime) {
        asyncOperationWithResult(cachedData)
      } else {
        asyncOperationWithError(new ErrorNoFreshData(cachedData))
      }
    }

    bindSequenceOfAsyncOperations(cachedDataLoader, validateByDateResultBinder)
  }

  class ResponseDataWithUpdateData(dataArg: Array[Byte], updateTimeArg: Long) extends RestKitCachedData {

    def data: Array[Byte] = { dataArg }
    def updateTime(): Long  = { updateTimeArg }
  }

  private class ErrorNoFreshData(val cachedData: RestKitCachedData) extends JError
}
