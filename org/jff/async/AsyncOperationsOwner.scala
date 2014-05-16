package org.jff.async

import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpHandlerTask._
import scala.collection.mutable.ArrayBuffer
import org.jff.utils.blocks.jUtilsBlockDefinitions

class AsyncOperationsOwner {

  private class ActiveLoaderData {

    var handler: AsyncHandler = null

    def clear() { handler = null }

    override def equals(other: Any): Boolean = {
      this.eq(other.asInstanceOf[AnyRef])
    }
  }

  private val loaders = new ArrayBuffer[ActiveLoaderData]

  def ownedAsyncOperation[T](loader: Async[T]): Async[T] = {

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[T]): AsyncHandler = {

      val loaderData = new ActiveLoaderData
      loaders += loaderData

      def finishWrapper(result: jUtilsBlockDefinitions.OptionResult[T]) {

        loaders -= loaderData

        if (finish != null)
          finish(result)

        loaderData.clear()
      }

      loaderData.handler = loader(progress, finishWrapper)

      def handler(task: AsyncOpHandlerTask) {

        if (!loaders.contains(loaderData))
          return

        loaders -= loaderData

        val handler = loaderData.handler
        if (handler != null)
          handler(task)

        loaderData.clear()
      }
      handler
    }
    async
  }

  def handleAllWithTask(task: AsyncOpHandlerTask): Boolean = {

    val result = loaders.size != 0
    val tmpLoaders = loaders.toArray
    loaders.clear()
    tmpLoaders.foreach(_.handler(task))
    result
  }
}
