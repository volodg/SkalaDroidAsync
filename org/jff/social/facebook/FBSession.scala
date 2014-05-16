package org.jff.social.facebook

import android.os.Bundle
import android.app.Activity
import android.content.Context
import org.jff.json.JSONTools
import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpUtils
import org.jff.async.jAsyncOpHelpers._
import org.jff.social.facebook.errors._
import org.jff.async.cached.CachedAsyncOp
import org.jff.async.AsyncOpHandlerTask._
import org.jff.async.JAsyncOpContinuity._
import org.jff.async.builder.AsyncOpInterface
import org.jff.async.builder.jAsyncOpBuilder._
import org.jff.utils.blocks.jUtilsBlockDefinitions
import java.net.URL
import com.facebook._
import com.facebook.model.GraphObject
import scala.collection.mutable
import scala.collection.JavaConverters._

//TODO http://stackoverflow.com/questions/14096403/facebook-login-with-android-sdk-3-0-causing-anrs-or-not-working-at-all

object FBSession extends CachedAsyncOp {

  lazy private val sessionCache = new CachedAsyncOp[String, Session]

  private class LoginAdapter(session: Session,
                             permissions: Set[String],
                             activity: Activity) extends AsyncOpInterface[Session] {

    val currPermissions = permissionsSet(session)

    private def isValidSession(session: Session): Boolean = {
      lazy val hasAllPermissions = permissions.subsetOf(currPermissions)
      session != null && session.isOpened && session.getAccessToken != null && session.getAccessToken.length > 0 && hasAllPermissions
    }

    val requestPermissions = permissions.union(currPermissions)

    private var statusCallback: Session.StatusCallback = null
    def asyncOpWithCallbacks(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Session]) {

      if (isValidSession(session)) {
        handleLoginWithSession(session, session.getState, null, finish)
        return
      }

      statusCallback = new Session.StatusCallback() {
        def call(session: Session, state: SessionState, exception: Exception) {
          handleLoginWithSession(session, state, exception, finish)
        }
      }

      val openRequest = new Session.OpenRequest(activity)

      openRequest.setDefaultAudience(SessionDefaultAudience.ONLY_ME)
      openRequest.setPermissions(requestPermissions.toList.asJava)
      openRequest.setCallback(statusCallback)

      session.openForRead(openRequest)
    }

    private def handleLoginWithSession(session: Session, state: SessionState, exception: Exception, finish: AsyncFinishCallback[Session]) {

      if (state == SessionState.OPENING || finish == null)
        return

      var error: FBError = null

      if (exception != null) {
        error = new FacebookAuthorizeErrorWithException(exception)
      } else if (session == null || !session.isOpened || session.getAccessToken == null || session.getAccessToken.length == 0) {
        error = new FacebookAuthorizeError
      }

      if (statusCallback != null) {
        session.removeCallback(statusCallback)
        statusCallback = null
      }
      finish(if (error != null) Left(error) else Right(session))
    }

    def doTask(task: AsyncOpHandlerTask) {}
  }

  private class LogoutAdapter(session: Session, renewSystemAuthorization: Boolean) extends AsyncOpInterface[Unit] {

    private def logout() {
      session.closeAndClearTokenInformation()
      notifyFinished()//TODO call after some delay like in IOS
    }

    private var finish: AsyncFinishCallback[Unit] = null
    private def notifyFinished() {
      if (finish != null)
        finish(Right())
    }

    def asyncOpWithCallbacks(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Unit]) {
      this.finish = finish
      logout()
    }
    def doTask(task: AsyncOpHandlerTask) {}
  }

  private def authFacebookSessionLoaderForSession(session: Session,
                                                  permissions: Set[String],
                                                  activity: Activity): Async[Session] = {

    def builder(): AsyncOpInterface[Session] = {
      new LoginAdapter(session, permissions, activity)
    }

    buildAsyncOpWithInstanceBuilder(builder)
  }

  private def facebookSession(context: Context): Session = {

    var session = Session.getActiveSession

    if (session == null || !session.isOpened) {
      session = new Session(context)
      Session.setActiveSession(session)
    }

    session
  }

  private def setFacebookSession(session: Session) {
    Session.setActiveSession(session)
  }

  private def permissionsSet(session: Session): Set[String] = {

    val set = mutable.HashSet[String]()
    val iterator = session.getPermissions.iterator()
    while (iterator.hasNext) {
      set += iterator.next()
    }
    set.toSet
  }

  private def authFacebookSessionLoader(activity: Activity, permissions: Set[String]): Async[Session] = {

    def loader(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Session]): AsyncHandler = {

      val session = facebookSession(activity)

      var currPermissions = permissionsSet(session)
      currPermissions = currPermissions.union(permissions)

      val loader = authFacebookSessionLoaderForSession(session, currPermissions, activity)

      def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[Session]) {

        result.right.foreach(result => {

          val session = result
          setFacebookSession(session)
        })

        finish(result)
      }

      loader(progress, doneCallbackWrapper)
    }

    sessionCache.asyncOpWithPropertySetterGetterKeyAndLoader(null, null, "authFacebookSessionLoader", loader)
  }

  private val defaultPermissions = Set("user_photos", "email")

  def authTokenLoader(activity: Activity): Async[String] = {

    def binder(session: Session): Async[String] = {

      asyncOperationWithResult(session.getAccessToken)
    }

    bindSequenceOfAsyncOperations(authFacebookSessionLoader(activity, defaultPermissions), binder)
  }

  private def facebookLogout(session: Session, renewSystemAuthorization: Boolean): Async[Unit] = {

    def builder(): AsyncOpInterface[Unit] = {

      new LogoutAdapter(session, renewSystemAuthorization)
    }

    buildAsyncOpWithInstanceBuilder(builder)
  }

  def logoutLoaderWithRenewSystemAuthorization(renewSystemAuthorization: Boolean): Async[Unit] = {

    def loader(progress: AsyncProgressCallback, finish: AsyncFinishCallback[Unit]): AsyncHandler = {

      val session = Session.getActiveSession
      val loader = if (session != null) facebookLogout(session, renewSystemAuthorization) else asyncOperationWithResult(())

      def doneCallbackWrapper(result: jUtilsBlockDefinitions.OptionResult[Unit]) {

        result.fold(error => {
          finish(Left(error))
        }, result => {
          setFacebookSession(null)
          finish(Right(()))
        })
      }

      loader(progress, doneCallbackWrapper)
    }

    loader
  }

  class Album(val albumId: String, val albumName: String, val albumType:String) {

    var albumPhotos: Array[Photo] = Array()
  }

  class Photo(val photoID: String, val smallImageURL: URL, val bigImageURL: URL, val albumID: String) {

  }

  private class RequestToLoader(request: Request) extends AsyncOpInterface[GraphObject] {

    def asyncOpWithCallbacks(progress: AsyncProgressCallback, finish: AsyncFinishCallback[GraphObject]) {

      request.setCallback(new Request.Callback() {
        def onCompleted(response: Response) {

          val result = response.getGraphObject

          if (result != null) {
            finish(Right(result))
          } else {
            finish(Left(new RequestError(response.getError)))
          }
        }
      })
      request.executeAsync()
    }

    def doTask(task: AsyncOpHandlerTask) {
    }
  }

  private def loaderWithRequest(request: Request): Async[GraphObject] = {
    buildAsyncOpWithInstanceBuilder(() => new RequestToLoader(request))
  }

  def albumsLoader(activity: Activity): Async[Array[Album]] = {

    val authLoader = authFacebookSessionLoader(activity, defaultPermissions)

    bindSequenceOfAsyncOperations(authLoader, (session: Session) => {

      def userAlbumsLoaderWithUserId: Async[GraphObject] = {

        val fqlQuery = "SELECT aid,name,modified,type FROM album WHERE owner=me()"
        val params = new Bundle
        params.putString("q", fqlQuery)

        loaderWithRequest(new Request(
          session,
          "/fql",
          params,
          HttpMethod.GET,
          null
        ))
      }

      def userPhotosWithUserId: Async[GraphObject] = {

        val fqlQuery = "SELECT object_id, src_big, src_small, aid FROM photo WHERE aid IN (SELECT aid FROM album WHERE owner = me())"
        val params = new Bundle
        params.putString("q", fqlQuery)

        loaderWithRequest(new Request(
          session,
          "/fql",
          params,
          HttpMethod.GET,
          null
        ))
      }

      def albumsParser(graphObject: GraphObject): Async[Array[Album]] = {

        AsyncOpUtils.asyncOperationWithSyncOperation(progress => {

          val jsonData = graphObject.getInnerJSONObject.getJSONArray("data")

          val albumsJson = JSONTools.JSONArrayToArrayOfObjects(jsonData, i => jsonData.getJSONObject(i))

          val albums = albumsJson.map(json => {

            val albumId   = json.getString("aid")
            val albumName = json.getString("name")
            val albumType = json.getString("type")
            new Album(albumId, albumName, albumType)
          })

          Right(albums.sortWith((album1, album2) => album1.albumType == "profile").toArray)
        })
      }

      def usersPhotoParser(graphObject: GraphObject): Async[Array[Photo]] = {

        AsyncOpUtils.asyncOperationWithSyncOperation(progress => {

          val jsonData = graphObject.getInnerJSONObject.getJSONArray("data")

          val photosJson = JSONTools.JSONArrayToArrayOfObjects(jsonData, i => jsonData.getJSONObject(i))

          val photos = photosJson.map(json => {

            val photoId       = json.getString("object_id")
            val smallImageURL = new URL(json.getString("src_small"))
            val bigImageURL   = new URL(json.getString("src_big"))
            val albumID       = json.getString("aid")

            new Photo(photoId, smallImageURL, bigImageURL, albumID)
          }).toArray

          Right(photos)
        })
      }

      val albumsLoader = bindSequenceOfAsyncOperations(userAlbumsLoaderWithUserId, albumsParser)
      val photosLoader = bindSequenceOfAsyncOperations(userPhotosWithUserId, usersPhotoParser)

      def mergePhotosWithAlbumsBinder(objects: (Array[Album], Array[Photo])): Async[Array[Album]] = {

        objects._1.foreach(album => {
          album.albumPhotos = objects._2.filter(_.albumID == album.albumId)
        })

        val albums = objects._1.filter(_.albumPhotos.length > 0)
        asyncOperationWithResult(albums)
      }

      bindSequenceOfAsyncOperations(
        groupOfAsyncOperations(albumsLoader, photosLoader),
        mergePhotosWithAlbumsBinder)
    })
  }
}
