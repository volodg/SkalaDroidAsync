package org.jff.network.callbacks

class JNetworkResponseDataCallback extends JNetworkAsyncOpCallback {

  var dataChunk: Array[Byte] = null
  
  var downloadedBytesCount: Long = 0
  var totalBytesCount     : Long = 0
}