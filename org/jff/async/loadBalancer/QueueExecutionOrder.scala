package org.jff.async.loadBalancer

object QueueExecutionOrder extends Enumeration {

  type QueueExecutionOrder = Value
  val OrderFifo,
  OrderStack,
  OrderRandom
  = Value
}
