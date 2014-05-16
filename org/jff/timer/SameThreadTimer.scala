package org.jff.timer

import java.util.{Timer, TimerTask}
import org.jff.utils.blocks.jUtilsBlockDefinitions._
import android.os.Handler

object SameThreadTimer {

  type TimerAction = (ActionBlock) => Unit

  def schedule(action: TimerAction, delay: Long): ActionBlock = {
    schedule(action, delay, None)
  }

  def schedule(action: TimerAction, delay: Long, period: Long): ActionBlock = {
    schedule(action, delay, Some(period))
  }

  private def schedule(action: TimerAction, delay: Long, period: Option[Long]): ActionBlock = {

    var timer = new Timer()

    def cancelBlock(): Unit = {
      if (timer != null) {
        timer.cancel()
        timer = null
      }
    }

    val handler = new Handler()

    val job = new TimerTask {
      def run(): Unit = {
        handler.post(new Runnable {
          override def run() { action(cancelBlock) }
        })
      }
    }

    period.fold({
      timer.schedule(job, delay)
    })(period => {
      timer.schedule(job, delay, period)
    })

    cancelBlock
  }
}
