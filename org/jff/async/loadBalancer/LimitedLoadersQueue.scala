package org.jff.async.loadBalancer

import org.jff.async.loadBalancer.QueueExecutionOrder.QueueExecutionOrder
import org.jff.async.AsyncOps._
import scala.collection.mutable.ArrayBuffer
import org.jff.utils.blocks.jUtilsBlockDefinitions
import scala.util.Random
import org.jff.async.AsyncOpHandlerTask._
import org.jff.async.errors.JAsyncOpFinishedByCancellationError

private object LimitedLoadersQueue {
  lazy val random = new Random()
}

class LimitedLoadersQueue[R](executionOrder: QueueExecutionOrder, limitCount: Int) {

  def this() = this(QueueExecutionOrder.OrderFifo, 10)
  def this(executionOrder: QueueExecutionOrder) = this(executionOrder, 10)

  private lazy val activeLoaders  = ArrayBuffer[BaseLoaderOwner]()
  private lazy val pendingLoaders = ArrayBuffer[BaseLoaderOwner]()

  private lazy val orderStrategy: QueueStrategy = {

    executionOrder match {

      case QueueExecutionOrder.OrderFifo   => new StrategyFifo
      case QueueExecutionOrder.OrderStack  => new StrategyStack
      case QueueExecutionOrder.OrderRandom => new StrategyRandom
    }
  }

  def balancedLoaderWithLoader(loader: Async[R]): Async[R] = {

    balancedLoaderWithLoader(loader, barrier = false)
  }

  def barrierBalancedLoaderWithLoader(loader: Async[R]): Async[R] = {

    balancedLoaderWithLoader(loader, barrier = true)
  }

  private def balancedLoaderWithLoader(loader: Async[R], barrier: Boolean): Async[R] = {

    def async(progress: AsyncProgressCallback, finish: AsyncFinishCallback[R]): AsyncHandler = {

      def onFinish(loaderOwner: BaseLoaderOwner) {

        activeLoaders -= loaderOwner
        performPendingLoaders()
      }

      val loaderHolder = new BaseLoaderOwner(
        loader,
        progress,
        finish,
        barrier,
        onFinish)

      pendingLoaders += loaderHolder
      performPendingLoaders()

      def handler(task: AsyncOpHandlerTask) {

        task match {

          case AsyncOpHandlerTaskUnSubscribe =>
            loaderHolder.progressCallback = null
            loaderHolder.doneCallback     = null
          case AsyncOpHandlerTaskCancel =>
            if (loaderHolder.loadersHandler != null) {
              loaderHolder.loadersHandler(AsyncOpHandlerTaskCancel)
            } else {
              val doneCallback = loaderHolder.doneCallback
              pendingLoaders -= loaderHolder
              if (doneCallback != null)
                doneCallback(Left(new JAsyncOpFinishedByCancellationError))
            }
        }
      }
      handler
    }
    async
  }

  private def hasLoadersReadyToStartForPendingLoader(pendingLoader: BaseLoaderOwner): Boolean = {

    if (pendingLoader.barrier) {

      activeLoaders.length == 0
    } else {

      limitCount > activeLoaders.length &&
        pendingLoaders.length > 0 &&
        activeLoaders.forall(el => !el.barrier)
    }
  }

  private def nextPendingLoader: Option[BaseLoaderOwner] = {

    if (pendingLoaders.length > 0) {
      Some(orderStrategy.firstPendingLoader)
    } else {
      None
    }
  }

  private def performPendingLoaders() {

    nextPendingLoader.foreach(pendingLoader => {

      if (hasLoadersReadyToStartForPendingLoader(pendingLoader)) {

        orderStrategy.executePendingLoader(pendingLoader)
        performPendingLoaders()
      }
    })
  }

  private trait QueueStrategy {

    def firstPendingLoader: BaseLoaderOwner

    def executePendingLoader(pendingLoader: BaseLoaderOwner) {

      pendingLoaders -= pendingLoader
      activeLoaders  += pendingLoader
      pendingLoader.performLoader()
    }
  }

  private class StrategyFifo extends QueueStrategy {

    def firstPendingLoader: BaseLoaderOwner = {

      pendingLoaders(0)
    }
  }

  private class StrategyStack extends QueueStrategy {

    def firstPendingLoader: BaseLoaderOwner = {

      pendingLoaders.last
    }
  }

  private class StrategyRandom extends QueueStrategy {

    def firstPendingLoader: BaseLoaderOwner = {

      val index = LimitedLoadersQueue.random.nextInt(pendingLoaders.length)
      pendingLoaders(index)
    }
  }

  private class BaseLoaderOwner(loader: Async[R],
                                var progressCallback: AsyncProgressCallback,
                                var doneCallback    : AsyncFinishCallback[R],
                                val barrier: Boolean,
                                onFinish: (BaseLoaderOwner) => Unit) {

    var loadersHandler: AsyncHandler = null

    def performLoader() {

      if (loadersHandler != null)
        throw new IllegalStateException("call performLoader only once")

      def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[R]) {

        onFinish(this)

        if (doneCallback != null)
          doneCallback(result)
      }

      loadersHandler = loader(progressCallback, doneCallbackWrapper)
    }
  }
}
