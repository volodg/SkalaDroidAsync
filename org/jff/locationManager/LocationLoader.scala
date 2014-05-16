package org.jff.locationManager

import android.os.IBinder
import android.content.{Context, Intent}
import android.location.{Location, LocationManager, LocationListener}
import org.jff.async.AsyncOps._
import org.jff.utils.errors.JError
import org.jff.timer.SameThreadTimer
import org.jff.async.AsyncOpHandlerTask._
import org.jff.utils.WeakReferencedObserver
import org.jff.async.builder.AsyncOpInterface
import org.jff.async.builder.jAsyncOpBuilder._
import org.jff.utils.blocks.jUtilsBlockDefinitions.ActionBlock
import java.util.Date

object LocationLoader {

  private trait LocationDataObserver {
    def didChangeLocation(location: Location)
  }

  private object LocationDataListener extends LocationListener with WeakReferencedObserver[LocationDataObserver] {

    val updatePeriod = 20*60*1000

    private var lastLocation: Location = null

    private var started = false
    private def startObservingIfNeeds(locationManager: LocationManager, provider: String) {

      if (started)
        return

      started = true
      val minTime     = updatePeriod
      val minDistance = 2000
      locationManager.requestLocationUpdates(provider, minTime, minDistance, this)
    }

    def getLastLocation(context: Context): Location = {

      if (lastLocation != null)
        return lastLocation

      val service = context.getSystemService(Context.LOCATION_SERVICE)

      val locationManager = service.asInstanceOf[LocationManager]

      val provider = LocationManager.PASSIVE_PROVIDER

      startObservingIfNeeds(locationManager, provider)

      lastLocation = locationManager.getLastKnownLocation(provider)
      lastLocation
    }

    override def onLocationChanged(location: android.location.Location) {

      lastLocation = location
      visitEach((observer: LocationDataObserver) => {
        observer.didChangeLocation(location)
      })
    }

    def onStatusChanged(s: java.lang.String, i: Int, bundle: android.os.Bundle) {}

    def onProviderEnabled(s: java.lang.String) {}

    def onProviderDisabled(s: java.lang.String) {}

    def onBind(intent: Intent): IBinder = { null }
  }

  private class LocationLoader(context: Context) extends AsyncOpInterface[Location] with LocationDataObserver {

    var finish: AsyncFinishCallback[Location] = null

    var cancelTimer: ActionBlock = null

    def asyncOpWithCallbacks(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Location]) {

      val location = LocationDataListener.getLastLocation(context)

      if (location != null && (new Date).getTime - location.getTime < LocationDataListener.updatePeriod) {
        finish(Right(location))
        return
      }

      LocationDataListener.addObserver(this)

      cancelTimer = SameThreadTimer.schedule((cancel) => {
        cancel()
        finish(Left(JError("no location")))
        onFinish()
      }, 3000)
    }

    def doTask(task: AsyncOpHandlerTask) { onFinish() }

    private def onFinish() {

      LocationDataListener.removeObserver(this)
      if (cancelTimer != null) {
        cancelTimer()
        cancelTimer = null
      }
    }

    override def didChangeLocation(location: Location) {

      finish(Right(location))
      onFinish()
    }
  }

  def locationLoader(context: Context): Async[Location] = {

    buildAsyncOpWithInstanceBuilder(() => new LocationLoader(context))
  }
}
