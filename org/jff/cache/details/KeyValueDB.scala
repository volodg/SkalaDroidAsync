package org.jff.cache.details

import org.jff.cache.CacheDB
import scala.collection.mutable
import android.database.sqlite.SQLiteDatabase
import android.content.{ContentValues, Context}
import java.io.{FileInputStream, FileOutputStream, File}
import android.util.Log
import java.util.Date

class KeyValueDB(fileNameArg: String, appContext: Context) extends CacheDB {

  private val folder   = jKeyValueDatabaseFolder  (fileNameArg)
  private val fileName = jKeyValueDatabaseFileName(fileNameArg)
  private val locker   = DBLocksHolder.lockObjectForFileName(folder + "/" + fileName)
  private val db       = openDB(folder, fileName)

  def closeDB() {

    if (db.isOpen)
      db.close()
  }

  override def finalize() {

    closeDB()
    super.finalize()
  }

  private def openDB(folder: String, fileName: String): SQLiteDatabase = {

    locker.synchronized {

      val dir = new File(folder)

      if (!dir.isDirectory) {

        val ok = dir.mkdirs()

        if (!ok)
          throw new IllegalStateException("make dir error: " + folder)
      }

      val db = SQLiteDatabase.openOrCreateDatabase(new File(folder + "/" + fileName), null)

      db.execSQL("PRAGMA cache_size = 1000")
      db.execSQL("CREATE TABLE IF NOT EXISTS records ( record_id TEXT primary key, file_link varchar(100), update_time real, access_time real );")

      Some(db)
    } match {

      case Some(db: SQLiteDatabase) => db
      case _ => null
    }
  }

  private def jKeyValueDatabaseFolder(fileName: String): String = {

    val appPath = appContext.getFilesDir.getAbsolutePath
    val result = f"$appPath%s/jKeyValueDatabasePath/" + fileName
    result.substring(0, result.lastIndexOf('/'))
  }

  private def jKeyValueDatabaseFileName(fileName: String): String = {

    if (fileName.contains("/")) {

      fileName.substring(fileName.lastIndexOf('/') + 1, fileName.size - 1)
    } else {

      fileName
    }
  }

  def updateAccessTime(recordID: String) {

    val values = new ContentValues
    values.put("access_time", (new Date).getTime:java.lang.Long)

    locker.synchronized {

      db update("records", values, f"record_id='$recordID%s'", null)
    }
  }

  private def fileLinkForRecordId(recordID: String): Option[String] = {

    locker.synchronized {

      val cursor = db.rawQuery(f"SELECT file_link FROM records WHERE record_id='$recordID%s'", null)

      val res = if (cursor.moveToFirst()) {
        Some(cursor.getString(cursor.getColumnIndex("file_link")))
      } else {
        None
      }

      cursor.close()

      res
    }
  }

  def removeRecordsForKey(key: String) {

    fileLinkForRecordId(key).foreach(fileLink => removeRecordsForRecordId(key, fileLink))
  }

  def removeRecordsToDate(date: Long, dateFieldName: String) {

    val query = s"SELECT file_link FROM records WHERE $dateFieldName < '$date';"

    locker.synchronized {

      ///First remove all files
      val cursor = db.rawQuery(query, null)
      if (cursor.moveToFirst()) {

        do {
          val fileLink = cursor.getString(cursor.getColumnIndex("file_link"))
          val file = new File(FileManager.cacheDBFileLinkPathWithFolder(fileLink, folder))
          file.delete()
        } while (cursor.moveToNext())
      } else {
        Log.w("SQLITE KEY VALUE DB", "no data to remove")
      }
      cursor.close()

      db delete("records", s"$dateFieldName < '$date'", null)
    }
  }

  def removeRecordsToUpdateDate(date: Long) {

    removeRecordsToDate(date, "update_time")
  }

  def removeRecordsToAccessDate(date: Long) {

    removeRecordsToDate(date, "access_time")
  }

  private def removeRecordsForRecordId(recordId: String, fileLink: String) {

    FileManager.cacheDBFileLinkRemoveFileWithFolder(fileLink, folder)

    val res = locker.synchronized {

      db.delete("records", s"record_id='$recordId'", null)
    }

    if (res == 0) {

      Log.w("SQLITE KEY VALUE DB", "no data for recordId: " + recordId)
    }
  }

  private def updateDataForRecordWithFileLink(data: Array[Byte], recordId: String, fileLink: String) {

    FileManager.cacheDBFileLinkSaveData(data, fileLink, folder)

    val values = new ContentValues
    val time = (new Date).getTime: java.lang.Long
    values.put("update_time", time)
    values.put("access_time", time)

    val res = locker.synchronized {

      db update("records", values, f"record_id='$recordId%s'", null)
    }

    if (res == 0) {

      Log.w("SQLITE KEY VALUE DB", "no data for recordId: " + recordId)
    }
  }

  private def addDataForRecord(data: Array[Byte], recordId: String) {

    val fileLink = java.util.UUID.randomUUID.toString

    val values = new ContentValues
    val time = (new Date).getTime: java.lang.Long
    values.put("record_id", recordId)
    values.put("file_link", fileLink)
    values.put("update_time", time)
    values.put("access_time", time)

    locker.synchronized {

      val res = db.insert("records", null, values: ContentValues)
      if (res > 0) {

        FileManager.cacheDBFileLinkSaveData(data, fileLink, folder)
      } else {

        Log.e("SQLITE KEY VALUE DB", "can not insert values: " + values)
      }
    }
  }

  private def folderSize: Long = {

    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    val files = recursiveListFiles(new File(folder))

    files.foldLeft(0: Long)((accum, file) => accum + file.length)
  }

  def removeRecordsWhileTotalSizeMoreThenBytes(sizeInBytes: Long) {

    val selectQuery = "SELECT file_link FROM records ORDER BY access_time"

    val totalSize = folderSize

    if (totalSize <= sizeInBytes)
      return

    locker.synchronized {

      var filesRemoved = 0

      var sizeToRemove = totalSize - sizeInBytes

      val cursor = db.rawQuery(selectQuery, null) //ORDER BY ASC is default

      if (cursor.moveToFirst()) {

        do {
          val fileLink = cursor.getString(cursor.getColumnIndex("file_link"))

          val file = new File(FileManager.cacheDBFileLinkPathWithFolder(fileLink, folder))
          val fileSize = file.length()
          file.delete()

          filesRemoved += 1

          if (sizeToRemove > fileSize) {

            sizeToRemove -= fileSize
          } else {

            sizeToRemove = 0
          }

        } while (cursor.moveToNext() && sizeToRemove > 0)
      } else {
        Log.w("SQLITE KEY VALUE DB", "no data to remove")
      }

      cursor.close()

      if (filesRemoved > 0) {

        val res = db.delete("records", f"file_link IN ($selectQuery%s LIMIT $filesRemoved%s);", null)
        if (res == 0) {
          Log.w("SQLITE KEY VALUE DB", "nothing to remove")
        }
      }
    }
  }

  def dataForKey(key: String): Option[Array[Byte]] = {

    dataAndLastUpdateTimeForKey(key).fold(None:Option[Array[Byte]])(res => Some(res._1))
  }

  def dataAndLastUpdateTimeForKey(key: String): Option[(Array[Byte], Long)] = {

    val res = locker.synchronized {

      val cursor = db.rawQuery(f"SELECT file_link, update_time FROM records WHERE record_id='$key%s'", null)

      val res = if (cursor.moveToFirst()) {

        val fileLink = cursor.getString(cursor.getColumnIndex("file_link"))

        for {
          recordData <- FileManager.cacheDBFileLinkDataWithFolder(fileLink, folder)
        } yield (recordData, cursor.getLong(cursor.getColumnIndex("update_time")))
      } else {
        None
      }

      cursor.close()

      res
    }

    res.foreach(_ => updateAccessTime(key))

    res
  }

  def removeAllRecords() {

    locker.synchronized {

      {
        val cursor = db.rawQuery("SELECT file_link FROM records;", null)

        if (cursor.moveToFirst()) {

          do {
            val fileLink = cursor.getString(cursor.getColumnIndex("file_link"))
            FileManager.cacheDBFileLinkRemoveFileWithFolder(fileLink, folder)
          } while (cursor.moveToNext())
        }

        cursor.close()
      }

      {
        val res = db.delete("records", null, null)
        if (res == 0) {
          Log.w("SQLITE KEY VALUE DB", "nothing to remove")
        }
      }
    }
  }

  def setDataForKey(data: Array[Byte], key: String) {

    locker.synchronized {

      val fileLink = fileLinkForRecordId(key)

      fileLink.fold(addDataForRecord(data, key))(fileLink => {

        if (data == null) {

          removeRecordsForRecordId(key, fileLink)
        } else {

          updateDataForRecordWithFileLink(data, key, fileLink)
        }
      })
    }
  }
}

private object FileManager {

  def cacheDBFileLinkRemoveFileWithFolder(fileLink: String, folder: String) {

    val path = cacheDBFileLinkPathWithFolder(fileLink, folder)
    val file = new File(path)
    file.delete()
  }

  def cacheDBFileLinkPathWithFolder(fileLink: String, folder: String): String = {

    folder + "/" + fileLink
  }

  def cacheDBFileLinkSaveData(data: Array[Byte], fileLink: String, folder: String) {

    val path = cacheDBFileLinkPathWithFolder(fileLink, folder)
    val output = new FileOutputStream(new File(path))
    output.write(data)
    output.close()
  }

  def cacheDBFileLinkDataWithFolder(fileLink: String, folder: String): Option[Array[Byte]] = {

    val fileName  = cacheDBFileLinkPathWithFolder(fileLink, folder)

    try {
      val file = new File(fileName)
      val in = new FileInputStream(file)
      val bytes = new Array[Byte](file.length.toInt)
      in.read(bytes)
      in.close()
      Some(bytes)
    } catch {

      case ex: Throwable =>
        Log.e("SQLITE KEY VALUE DB", "no file with name: " + fileName)
        None
    }
  }
}

private object DBLocksHolder {

  lazy private val syncObjectByFileName = mutable.Map[String, Object]()

  def lockObjectForFileName(fileName: String): Object = {

    synchronized {

      syncObjectByFileName.get(fileName).fold({

        val result = new Object
        syncObjectByFileName += fileName -> result
        result
      })(result => result)
    }
  }
}
