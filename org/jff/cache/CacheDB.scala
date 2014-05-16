package org.jff.cache

trait CacheDB {

  def dataForKey(key: String): Option[Array[Byte]]
  def dataAndLastUpdateTimeForKey(key: String): Option[(Array[Byte], Long)]

  def setDataForKey(data: Array[Byte], key: String)

  def removeRecordsForKey(key: String)
  def removeAllRecords()

  def closeDB()
}
