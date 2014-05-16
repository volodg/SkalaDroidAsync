package org.jff.cache

import org.jff.xml.dom.XMLElement

class CacheDBInfo(val dbPropertyName: String,
                  val fileName: String,
                  val version: Int,
                  val timeToLiveInHours: Long,
                  val lastAccessDateInHours: Option[Long],
                  val maxSizeInMB: Option[Long])

object CacheDBInfo {

  def createCacheDBInfoWitXMLElement(imageCacheData: XMLElement, elName: String): Option[CacheDBInfo] = {

    def createCacheDBInfo(elName: String,
                          fileName: String,
                          version: Int,
                          timeToLiveInHours: Long): CacheDBInfo = {

      val autoRemoveOpt = imageCacheData.subElementByName("autoRemove")
      val zero: Option[Long] = None
      val autoRemoveFields: (Option[Long], Option[Long]) = autoRemoveOpt.fold((zero, zero))(autoRemove => {
        val lastAccessDateInHours = autoRemove.subElementByName("lastAccessDateInHours").map(_.text.toLong)
        val maxSizeInMB           = autoRemove.subElementByName("maxSizeInMB").map(_.text.toLong)

        def zeroAsNone(value: Option[Long]): Option[Long] = {
          value.fold(None:Option[Long])(intVal => if (intVal == 0) None else value)
        }

        (zeroAsNone(lastAccessDateInHours), zeroAsNone(maxSizeInMB))
      })
      new CacheDBInfo(elName, fileName, version, timeToLiveInHours, autoRemoveFields._1, autoRemoveFields._2)
    }

    for {
      fileName          <- imageCacheData.subElementByName("fileName")
      version           <- imageCacheData.subElementByName("version")
      timeToLiveInHours <- imageCacheData.subElementByName("timeToLiveInHours")
    } yield createCacheDBInfo(elName, fileName.text, version.text.toInt, timeToLiveInHours.text.toLong)
  }
}