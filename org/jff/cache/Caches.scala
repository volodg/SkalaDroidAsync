package org.jff.cache

import android.content.res.XmlResourceParser
import org.jff.xml.dom.{XMLElement, DOMXMLParser}
import org.jff.xml.XmlSaxParserAdapter
import org.jff.cache.details.KeyValueDB
import scala.collection.mutable
import scala.collection.immutable
import android.content.Context
import java.io._
import org.jff.async.AsyncOpUtils
import org.jff.utils.blocks.jUtilsBlockDefinitions
import org.jff.async.AsyncOps.AsyncProgressCallback
import org.jff.timer.SameThreadTimer

object Caches {

  private var dbInfo : DBInfo  = null
  private var context: Context = null

  def thumbnailDBName: String = "JFF_THUMBNAIL_DB"

  def createThumbnailDB(): CacheDB = createCacheForName(thumbnailDBName)

  def createCacheForName(name: String): CacheDB = {
    val dbCacheInfo = this.dbInfo.cacheDbInfoByName.get(name).get
    new InternalCacheDB(dbCacheInfo, this.dbInfo, context)
  }

  def cacheDBInfoByName(name: String): Option[CacheDBInfo] = dbInfo.cacheDbInfoByName.get(name)

  def thumbnailDB: CacheDB = cacheByName(thumbnailDBName).get

  def cacheByName(name: String): Option[CacheDB] = cacheDbByName.get(name)

  class DBInfo(xml: XmlResourceParser, context: Context) {

    def createCacheDbInfoByName(xml: XmlResourceParser): immutable.Map[String, CacheDBInfo] = {

      val dom = new DOMXMLParser

      XmlSaxParserAdapter.parse(xml, dom)

      val rootElement = dom.result._1

      val mutCacheDbInfoByName = mutable.Map[String, CacheDBInfo]()

      def parseDBInfoElement(el: XMLElement) {

        def result = for {
          dbName <- el.attributes.get("name")
          info   <- CacheDBInfo.createCacheDBInfoWitXMLElement(el, dbName)
        } yield info

        result.foreach(info => mutCacheDbInfoByName += (info.dbPropertyName -> info))
      }

      rootElement.children.foreach(parseDBInfoElement)

      mutCacheDbInfoByName.toMap
    }

    val cacheDbInfoByName: immutable.Map[String, CacheDBInfo] = createCacheDbInfoByName(xml)

    def saveCurrentDBInfoVersions() {

      synchronized {

        val mutableCurrentVersions = mutable.Map[String, Int]()
        cacheDbInfoByName.foreach(el => {mutableCurrentVersions += el._1 -> el._2.version})

        val currentVersions: immutable.Map[String, Int] = mutableCurrentVersions.toMap

        if (currentVersions != _currentDbVersionsByName) {

          _currentDbVersionsByName = currentVersions
          serialize(currentVersions, currentDBInfoFilePath)
        }
      }
    }

    private def currentDBInfoFilePath: String = {

      val appPath = context.getFilesDir.getAbsolutePath
      f"$appPath%s/JFFCurrentDBVersions.data"
    }

    private type VersionContainer = immutable.Map[String, Int]
    private var _currentDbVersionsByName: VersionContainer = null

    def currentDbVersionsByName: VersionContainer = {

      if (_currentDbVersionsByName == null) {

        deSerialize(currentDBInfoFilePath).fold({
          _currentDbVersionsByName = immutable.Map[String, Int]()
        })(result => {
          _currentDbVersionsByName = result
        })
      }

      _currentDbVersionsByName
    }

    private def deSerialize(path: String): Option[VersionContainer] = {

      try {
        val in = new ObjectInputStream(new FileInputStream(new File(path)))
        Some(in.readObject().asInstanceOf[VersionContainer])
      } catch {
        case ex: Throwable => None
      }
    }

    private def serialize(a: VersionContainer, path: String) {

      val bytes = new FileOutputStream(new File(path))
      val out = new ObjectOutputStream(bytes)
      out.writeObject(a)
      out.close()
    }
  }

  private var cacheDbByName: mutable.Map[String, InternalCacheDB] = null

  def initializeWithXMLResource(xml: XmlResourceParser, context: Context) {

    if (dbInfo != null) {
      //throw new IllegalStateException("should be initialized only once")
      return
    }

    this.context = context
    dbInfo = new DBInfo(xml, context)
    cacheDbByName = createCachesWithDBInfo(dbInfo, context)

    synchronizeDB()

    cacheDbByName.foreach(el => el._2.runAutoRemoveDataScheduler())
  }

  private def createCachesWithDBInfo(dbInfo: DBInfo, context: Context): mutable.Map[String, InternalCacheDB] = {

    val mutCacheDbByName = mutable.Map[String, InternalCacheDB]()

    dbInfo.cacheDbInfoByName.foreach(el => mutCacheDbByName += el._1 -> new InternalCacheDB(el._2, dbInfo, context))

    mutCacheDbByName
  }

  private class InternalCacheDB(infoArg: CacheDBInfo, dbInfo: DBInfo, context: Context) extends KeyValueDB(infoArg.fileName, context) {

    val info = infoArg

    def runAutoRemoveDataScheduler() {

      def removeOldData() {

        val removeRarelyAccessDataDelay = infoArg.lastAccessDateInHours
        removeRarelyAccessDataDelay.foreach(hours => {

          removeRecordsToAccessDate(hours * 3600 * 1000)
        })

        infoArg.maxSizeInMB.foreach(maxSizeInMB => {

          val bytes = maxSizeInMB * 1024 * 1024
          removeRecordsWhileTotalSizeMoreThenBytes(bytes)
        })
      }

      AutoRemoveData.runAutoRemoveDataScheduler(this, infoArg, removeOldData)
    }

    def migrateDB() {

      val currentDbInfo = dbInfo.currentDbVersionsByName.get(info.dbPropertyName)

      currentDbInfo.foreach(currVersion => {
        if (info.version > currVersion) {
          removeAllRecords()
        }
      })
    }
  }

  private def synchronizeDB() {

    val cacheDbInfoByName = Caches.cacheDbByName
    cacheDbInfoByName.foreach(el => el._2.migrateDB())
    dbInfo.saveCurrentDBInfoVersions()
  }
}

private object AutoRemoveData {

  lazy private val autoRemoveScheduled = mutable.Set[String]()

  def runAutoRemoveDataScheduler(db: KeyValueDB, infoArg: CacheDBInfo, removeOldData: () => Unit) {

    val dbKey = infoArg.dbPropertyName

    val scheduled = autoRemoveScheduled.contains(dbKey)
    if (scheduled)
      return

    synchronized {

      val scheduled = autoRemoveScheduled.contains(dbKey)
      if (!scheduled) {

        autoRemoveScheduled += dbKey

        SameThreadTimer.schedule((cancel) => {

          def removeOldDataSyncOp(progress: AsyncProgressCallback): jUtilsBlockDefinitions.OptionResult[Int] = {
            removeOldData()
            Right(0)
          }
          val loader = AsyncOpUtils.asyncOperationWithSyncOperation(removeOldDataSyncOp)
          loader(null, null)
        }, 0, 3600000)
      }
    }
  }
}