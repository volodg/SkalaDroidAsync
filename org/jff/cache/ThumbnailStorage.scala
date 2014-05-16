package org.jff.cache

import java.net.URL
import org.jff.async.AsyncOps._
import org.jff.async.JAsyncOpContinuity._
import org.jff.async.jAsyncOpHelpers._
import org.jff.async.cached.CachedAsyncOp
import org.jff.cache.errors.{CacheLoadImageError, CacheNoURLError}
import org.jff.restKit.SmartDataLoader.{ResponseDataWithUpdateData, SmartDataLoaderFields}
import org.jff.restKit.{SmartDataLoader, RestKitCache}
import org.jff.network.jNetworkAsyncs
import android.graphics.{Bitmap, BitmapFactory}

object ThumbnailStorage extends CachedAsyncOp[Map[String, Any], Bitmap] {

  lazy val noImageDataURLString = new URL("http://jff.cache.com/nodata")

  def notNullURL(url: URL): URL = {

    if (url != null) url else noImageDataURLString
  }

  def thumbnailLoaderForUrl(url: URL): Async[Bitmap] = {

    if (url == null)
      throw new NullPointerException

    if (url == noImageDataURLString)
      return asyncOperationWithError(new CacheNoURLError)

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Bitmap]): AsyncHandler = {

      val loader = cachedInDBImageDataLoaderForUrl(url, doesNotIgnoreFreshDataLoadFail = false)

      val uniqueKey = Map(
        "url" -> url,
        "cmd" -> "ThumbnailStorage.thumbnailLoaderForUrl"
      )

      asyncOpWithPropertySetterGetterKeyAndLoader(null, null, uniqueKey, loader)(progress, finish)
    }
    async
  }

  def tryThumbnailLoaderForUrls(urls: Array[URL]): Async[Bitmap] = {

    trySequenceOfAsyncOperationsArray(urls.map((url: URL) => {

      thumbnailLoaderForUrl(url)
    }))
  }

  private def cacheDataLifeTimeInSeconds: Long = {

    Caches.cacheDBInfoByName(Caches.thumbnailDBName).fold({
      throw new IllegalStateException("Caches should be initialized first")
      0: Long
    })(_.timeToLiveInHours * 3600)
  }

  private def imageCacheAdapter: RestKitCache = {

    def adapter(): CacheDB = {

      Caches.createThumbnailDB()
    }

    class ImageCacheAdapter extends CacheAdapter(adapter) {

      override def loaderToSetDataForKey(data: Array[Byte], key: String): Async[String] = {

        //add balancer here
        super.loaderToSetDataForKey(data, key)
      }

      //returns JFFRestKitCachedData instance in result callback
      override def cachedDataLoaderForKey(key: String): Async[ResponseDataWithUpdateData] = {

        //add balancer here
        super.cachedDataLoaderForKey(key)
      }
    }

    new ImageCacheAdapter
  }

  private def cachedInDBImageDataLoaderForUrl(url: URL, doesNotIgnoreFreshDataLoadFail: Boolean): Async[Bitmap] = {

    def dataLoaderForIdentifier(identifier: URL): Async[Array[Byte]] = {

      jNetworkAsyncs.dataURLResponseLoader(identifier, None, None)
    }

    def imageDataToUIImageBinder(url: URL): AsyncBinder[Array[Byte], Bitmap] = {

      def binder(data: Array[Byte]): Async[Bitmap] = {

        if (url == noImageDataURLString) {

          return asyncOperationWithError(new CacheNoURLError)
        }
        try {
          val image = BitmapFactory.decodeByteArray(data, 0, data.length)
          asyncOperationWithResult(image)
        } catch {
          case e : Throwable =>
            asyncOperationWithError(new CacheLoadImageError(e))
        }
      }
      binder
    }

    val args = new SmartDataLoaderFields[URL, Bitmap](
      url,
      dataLoaderForIdentifier,
      imageDataToUIImageBinder,
      null,
      doesNotIgnoreFreshDataLoadFail,
      imageCacheAdapter,
      cacheDataLifeTimeInSeconds)

    val loader = SmartDataLoader.jSmartDataLoaderWithCache(args)

    bindTrySequenceOfAsyncOperations(loader, (bindResult: Any) => {

      val error = bindResult.asInstanceOf[Error]
      asyncOperationWithError(new CacheLoadImageError(error))
    })
  }
}
