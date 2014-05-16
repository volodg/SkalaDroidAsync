package org.jff.async

object AsyncOpHandlerTask extends Enumeration {
  
  type AsyncOpHandlerTask = Value
  val AsyncOpHandlerTaskUnSubscribe,
  AsyncOpHandlerTaskCancel,
  AsyncOpHandlerTaskUndefined
  = Value
}
