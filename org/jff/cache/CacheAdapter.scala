package org.jff.cache

import org.jff.restKit.{SmartDataLoader, RestKitCache}
import org.jff.async.{AsyncOpUtils, AsyncOps}
import org.jff.async.AsyncOps.AsyncProgressCallback
import org.jff.utils.errors.JError
import org.jff.restKit.SmartDataLoader.ResponseDataWithUpdateData
import org.jff.utils.blocks.jUtilsBlockDefinitions

object CacheAdapter {

  type CacheFactory = () => CacheDB
}

class CacheAdapter(factory: CacheAdapter.CacheFactory) extends RestKitCache {

  def loaderToSetDataForKey(data: Array[Byte], key: String): AsyncOps.Async[String] = {

    AsyncOpUtils.asyncOperationWithSyncOperation((progress: AsyncProgressCallback) => {

      val cache = factory()
      cache.setDataForKey(data, key)
      cache.closeDB()
      Right("loaderToSetDataForKey success")
    })
  }

  //returns JFFRestKitCachedData instance in result callback
  def cachedDataLoaderForKey(key: String): AsyncOps.Async[ResponseDataWithUpdateData] = {

    AsyncOpUtils.asyncOperationWithSyncOperation((progress: AsyncProgressCallback) => {

      val cache = factory()

      val isEmpty:jUtilsBlockDefinitions.OptionResult[ResponseDataWithUpdateData] = Left(JError("no cached data for key: "))
      val res = cache.dataAndLastUpdateTimeForKey(key).fold(isEmpty)(res => {
        Right(new SmartDataLoader.ResponseDataWithUpdateData(res._1, res._2))
      })

      cache.closeDB()

      res
    })
  }
}
