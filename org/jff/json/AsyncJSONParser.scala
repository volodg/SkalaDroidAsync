package org.jff.json

import org.json.JSONObject
import org.json.JSONException
import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpUtils._
import org.jff.json.errors.ParseJsonError
import org.jff.utils.blocks.jUtilsBlockDefinitions.OptionResult

object AsyncJSONParser {

  private def stringToJSON(input: String, context: Any): SyncOp[JSONObject] = {

    def syncOp(progress: AsyncProgressCallback): OptionResult[JSONObject] = {
      try {
        val jObj  = new JSONObject(input)
        Right(jObj)
      } catch {
        case e: JSONException =>
          val error = new ParseJsonError()
          error.exception = e
          error.context   = context
          error.data      = input
          Left(error)
      }
    }
    syncOp
  }

  def asyncOperationJsonStringParserWithContext(input: String, context: Any): Async[JSONObject] = {
    asyncOperationWithSyncOperation(stringToJSON(input, context))
  }

  def asyncOperationJsonDataParserWithContext(data: Array[Byte], context: Any): Async[JSONObject] = {
    def loadDataBlock(progress: AsyncProgressCallback): OptionResult[JSONObject] = {
      stringToJSON(new String(data), context)(progress)
    }
    asyncOperationWithSyncOperation(loadDataBlock)
  }
}