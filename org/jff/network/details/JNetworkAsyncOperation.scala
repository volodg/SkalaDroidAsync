package org.jff.network.details

import org.jff.async.AsyncOps._
import org.jff.network.{JHTTPUploadProgress, JHTTPResponse, JURLConnectionParams}
import org.jff.utils.blocks.jUtilsBlockDefinitions
import org.jff.network.callbacks.JNetworkResponseDataCallback
import java.util.zip.GZIPInputStream
import android.util.Log
import java.io.{InputStream, OutputStream, ByteArrayOutputStream}
import java.net.{CookiePolicy, CookieManager, CookieHandler, HttpURLConnection}
import org.jff.network.query.XQueryComponents
import org.jff.network.errors.NetworkError

object JNetworkAsyncOperation {

  CookieHandler.setDefault(new CookieManager(null, CookiePolicy.ACCEPT_ALL))

  def networkResponse(params: JURLConnectionParams): SyncOp[JHTTPResponse] = {

    def syncOp(progress: AsyncProgressCallback): jUtilsBlockDefinitions.OptionResult[JHTTPResponse] = {

      def fetch(): jUtilsBlockDefinitions.OptionResult[JHTTPResponse] = {

        val httpConnection = params.url.openConnection().asInstanceOf[HttpURLConnection]

        httpConnection.setRequestMethod(params.httpMethod)

        httpConnection.setReadTimeout(7000)
        httpConnection.setConnectTimeout(7000)

        params.headers.foreach(_.foreach(header => httpConnection.addRequestProperty(header._1, header._2)))

        //write data
        {
          val zero:Option[OutputStream] = None
          val bodyStreamOpt = params.entity.fold(zero)(entity => {

            httpConnection.setDoOutput(true)
            val query  = XQueryComponents.dataFromQueryComponents(entity)
            val stream = httpConnection.getOutputStream

            stream.write(query)
            stream.flush()

            Some(stream)
          })

          //
          params.uploadData.foreach(uploadData => {

            val bodyOutputStream = bodyStreamOpt.fold({

              httpConnection.setDoOutput(true)
              httpConnection.getOutputStream
            })(stream => stream)

            var bytesUploaded = 0

            val progressStream = new InputStream{

              override def read: Int = uploadData.dataStream.read

              override def read(buffer: Array[Byte], byteOffset: Int, byteCount: Int): Int = {

                val res = uploadData.dataStream.read(buffer, byteOffset, byteCount)

                bytesUploaded += res

                if (progress != null) {
                  uploadData.dataLength.foreach(length => {
                    val progressPercents = (bytesUploaded:Double)/(length:Double)
                    val uploadProgress = new JHTTPUploadProgress(progressPercents)
                    progress(uploadProgress)
                  })
                }

                res
              }
            }

            {
              var nRead: Int = 0
              val data = new Array[Byte](16384)

              nRead = progressStream.read(data, 0, data.length)
              while (nRead != -1) {
                bodyOutputStream.write(data, 0, nRead)
                nRead = progressStream.read(data, 0, data.length)
              }

              bodyOutputStream.flush()
            }
          })

          bodyStreamOpt.foreach(stream => {
            stream.close()
          })
        }

        //val status = httpConnection.getResponseCode
//        val response = new JHTTPResponse(httpConnection.getResponseCode, httpConnection.getResponseMessage)
//        Log.d("wd", "status: " + response.httpCode)
//        Log.d("wd", "httpResponse: " + response.httpResponse)

        //read response
        {
          var inStream = httpConnection.getInputStream

          val contentEncoding = httpConnection.getHeaderField("Content-Encoding")
          if (contentEncoding != null && contentEncoding.equalsIgnoreCase("gzip")) {
            inStream = new GZIPInputStream(inStream)
          }

          val dataChunk: Array[Byte] = {

            val buffer = new ByteArrayOutputStream

            var nRead: Int = 0
            val data = new Array[Byte](16384)

            nRead = inStream.read(data, 0, data.length)
            while (nRead != -1) {
              buffer.write(data, 0, nRead)
              nRead = inStream.read(data, 0, data.length)
            }

            buffer.flush()
            val res = buffer.toByteArray
            buffer.close()
            res
          }
          inStream.close()

          val progressInfo = new JNetworkResponseDataCallback
          progressInfo.dataChunk = dataChunk

          if (progress != null) {
            progress(progressInfo)
          }
        }

        httpConnection.disconnect()

        Right(new JHTTPResponse(httpConnection.getResponseCode, httpConnection.getResponseMessage))
      }

      try {
        fetch()
      } catch {
        case ex: Throwable =>
          Log.d("iAsync", "network done error: " + ex)
          Left(new NetworkError(ex))
      }
    }
    syncOp
  }
}