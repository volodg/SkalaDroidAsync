package org.jff.restKit

import org.jff.async.AsyncOps.Async
import org.jff.restKit.SmartDataLoader.ResponseDataWithUpdateData

trait RestKitCachedData {

  def data: Array[Byte]
  def updateTime(): Long
}

trait RestKitCache {

  def loaderToSetDataForKey(data: Array[Byte], key: String): Async[String]

  //returns JFFRestKitCachedData instance in result callback
  def cachedDataLoaderForKey(key: String): Async[ResponseDataWithUpdateData]
}
