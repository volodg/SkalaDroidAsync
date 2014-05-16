package org.jff.gcm

import com.google.android.gms.gcm.GoogleCloudMessaging
import com.google.android.gms.common.{ConnectionResult, GooglePlayServicesUtil}
import android.util.Log
import android.os.Bundle
import android.app.Activity
import android.content.{Context, SharedPreferences}
import android.content.pm.PackageManager.NameNotFoundException
import org.jff.async.AsyncOpUtils
import org.jff.async.jAsyncOpHelpers._
import org.jff.async.JAsyncOpContinuity._
import org.jff.async.cached.CachedAsyncOp
import org.jff.async.AsyncOps.{Async, AsyncProgressCallback}

//http://developer.android.com/google/gcm/client.html
trait GCMActivity extends Activity {

  private val PROPERTY_REG_ID      = "registration_id"
  private val PROPERTY_APP_VERSION = "appVersion"
  private val PLAY_SERVICES_RESOLUTION_REQUEST = 9000

  protected def getGCMSenderID: String

  /**
   * Tag used on log messages.
   */
  private val TAG = "iAsync"

  var gcm    : GoogleCloudMessaging = null
  var prefs  : SharedPreferences = null
  var context: Context = null

  private var regID: Option[String] = None
  private lazy val regIDMerger = new CachedAsyncOp[String, String]

  protected def onGetGCMRegistrationID(registrationID: String)

  override def onCreate(savedInstanceState: Bundle) {

    super.onCreate(savedInstanceState)

    context = getApplicationContext

    // Check device for Play Services APK. If check succeeds, proceed with
    //  GCM registration.
    if (checkPlayServices) {

      val gcm = GoogleCloudMessaging.getInstance(this)
      regID = getRegistrationId

      Log.d(TAG, "regID: " + regID)

      def setter(value: String) { regID = Some(value) }
      def getter(): Option[String] = { regID }

      def saveRegID(regID: String): Async[String] = {
        Log.d(TAG, "regID: " + regID)
        storeRegistrationId(context, regID)
        asyncOperationWithResult(regID)
      }

      val loader = bindSequenceOfAsyncOperations(regIDLoader(gcm), saveRegID)
      regIDMerger.asyncOpWithPropertySetterGetterKeyAndLoader(setter, getter, "GCMActivity.onCreate", loader)(null, result => {
        result.right.foreach(regID => onGetGCMRegistrationID(regID))
      })
    } else {
      Log.i(TAG, "No valid Google Play Services APK found.")
    }
  }

  private def regIDLoader(gcm: GoogleCloudMessaging): Async[String] = {
    AsyncOpUtils.asyncOperationWithSyncOperation((progress: AsyncProgressCallback) => {
      val regID = gcm.register(getGCMSenderID)
      Right(regID)
    })
  }

  private def checkPlayServices: Boolean = {
    val resultCode = GooglePlayServicesUtil.isGooglePlayServicesAvailable(this)
    if (resultCode != ConnectionResult.SUCCESS) {
      if (GooglePlayServicesUtil.isUserRecoverableError(resultCode)) {
        GooglePlayServicesUtil.getErrorDialog(resultCode, this, PLAY_SERVICES_RESOLUTION_REQUEST, null).show()
      } else {
        Log.i(TAG, "This device is not supported.")
        finish()
      }
      false
    }
    true
  }

  private def getRegistrationId: Option[String] = {
    val prefs = getGCMPreferences
    val registrationId = prefs.getString(PROPERTY_REG_ID, "")
    if (registrationId.isEmpty) {
      Log.i(TAG, "Registration not found.")
      return None
    }
    val registeredVersion = prefs.getInt(PROPERTY_APP_VERSION, Integer.MIN_VALUE)
    val currentVersion    = getAppVersion(context)
    if (registeredVersion != currentVersion) {
      Log.i(TAG, "App version changed.")
      None
    } else
      Some(registrationId)
  }

  private def getAppVersion(context: Context): Int = {
    try {
      val packageInfo = context.getPackageManager.getPackageInfo(context.getPackageName, 0)
      packageInfo.versionCode
    } catch {
      case e: NameNotFoundException =>
        // should never happen
        throw new RuntimeException("Could not get package name: " + e)
    }
  }

  private def getGCMPreferences: SharedPreferences = {
    // This sample app persists the registration ID in shared preferences, but
    // how you store the regID in your app is up to you.
    getSharedPreferences(classOf[GCMActivity].getSimpleName, Context.MODE_PRIVATE)
  }

  private def storeRegistrationId(context: Context, regId: String) {
    val prefs = getGCMPreferences
    val appVersion = getAppVersion(context)
    Log.i(TAG, "Saving regId on app version " + appVersion)
    val editor = prefs.edit()
    editor.putString(PROPERTY_REG_ID, regId)
    editor.putInt(PROPERTY_APP_VERSION, appVersion)
    editor.commit()
  }
}

