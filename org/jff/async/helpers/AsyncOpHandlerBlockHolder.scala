package org.jff.async.helpers

import org.jff.async.AsyncOps._
import org.jff.async.AsyncOpHandlerTask._

class AsyncOpHandlerBlockHolder {

  var loaderHandler: AsyncHandler = null
  
  def smartLoaderHandler(): AsyncHandler = {
    
    (task: AsyncOpHandlerTask) => performCancelBlockOnceWithArgument(task)
  }
  
  def performCancelBlockOnceWithArgument(task: AsyncOpHandlerTask) {

    if (task >= AsyncOpHandlerTaskUndefined) {
      
      throw new IllegalArgumentException()
    }
    
    if (loaderHandler == null)
        return
    
    val block = loaderHandler
    loaderHandler = null
    block(task)
  }
}