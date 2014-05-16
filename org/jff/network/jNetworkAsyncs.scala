package org.jff.network

import java.net.URL
import android.util.Log
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpUtils
import org.jff.utils.blocks.jUtilsBlockDefinitions
import org.jff.network.details.JNetworkAsyncOperation
import org.jff.network.callbacks.JNetworkResponseDataCallback

object jNetworkAsyncs {

  private def privateGenericChunkedURLResponseLoader(params: JURLConnectionParams, todoAnalyzer: Any): Async[JHTTPResponse] = {
    
    val analyzer = JNetworkAsyncOperation.networkResponse(params)
    AsyncOpUtils.asyncOperationWithSyncOperation(analyzer)
  }
  
  private def privateGenericDataURLResponseLoader(params: JURLConnectionParams, todoAnalyzer: Any): Async[Array[Byte]] = {
    
    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Array[Byte]]) : AsyncHandler = {
      
      val loader = privateGenericChunkedURLResponseLoader(params, null/*responseAnalyzer*/)
      
      val responseData: ArrayBuffer[Byte] = ArrayBuffer[Byte]()
      
      def dataProgressCallback(progressInfo: Any): Unit = {

        progressInfo match {

          case info: JNetworkResponseDataCallback =>
            val chunk: Array[Byte] = progressInfo.asInstanceOf[JNetworkResponseDataCallback].dataChunk
            responseData ++= chunk
          case _ =>
        }
        if (progress != null)
          progress(progressInfo)
      }
      
      def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[JHTTPResponse]) {

        result.fold(error => {

          finish(Left(error))
        }, result => {

          if (responseData.length == 0) {
            Log.d("info", f"!!!WARNING!!! request with params: $params%s got an empty response")
          }
          finish(Right(responseData.toArray))
        })
      }
      
      loader(dataProgressCallback, doneCallbackWrapper)
    }
    async
  }
  
  def genericDataURLResponseLoader(params: JURLConnectionParams): Async[Array[Byte]] = {
    
    privateGenericDataURLResponseLoader(params, null)
  }

  def dataURLResponseLoader(url: URL, postParams: Option[JURLConnectionParams.EntityType], headers: Option[immutable.Map[String, String]]): Async[Array[Byte]] = {

    val params = new JURLConnectionParams(url)
    params.entity  = postParams
    params.headers = headers
    privateGenericDataURLResponseLoader(params, null)
  }
}