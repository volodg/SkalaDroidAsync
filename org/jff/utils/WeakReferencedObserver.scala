package org.jff.utils

import java.util
import java.lang.ref.WeakReference
import scala.collection.mutable.ArrayBuffer

//TODO use scala WeakReference type
//TODO rename to WeakReferencedVisitor
trait WeakReferencedObserver[T] {

  private type ObserversContainer = util.ArrayList[WeakReference[T]]
  private val observers = new ObserversContainer
  private lazy val strongObservers = new ArrayBuffer[T]
    
  def addObserver(observer: T, strongReference: Boolean = false) {

    observers.add(new WeakReference(observer))
    if (strongReference)
      strongObservers += observer
  }

  def removeObserver(observer: T) {
      
    var index = observers.size() - 1
    while (index >= 0) {
        
      val reference = observers.get(index)
      val observerToRemove = reference.get
      if (observerToRemove == observer) {
          
        observers.remove(index)
        return
      }
      
      index -= 1
    }

    strongObservers -= observer
  }
    
  type ObserverVisitor = (T) => Unit
  def visitEach(visitor: ObserverVisitor) {

    def untilVisitor(data: T): Boolean = {
      visitor(data)
      false
    }
    visitAll(untilVisitor)
  }

  type ObserverUntilVisitor = (T) => Boolean
  def visitAll(visitor: ObserverUntilVisitor): Boolean = {

    var index = observers.size() - 1
    var visited = false

    while (index >= 0 && index < observers.size() && !visited) {
      val reference = observers.get(index)
      val observerToVisit = reference.get
      if (observerToVisit == null) {
        observers.remove(index)
      } else {
        visited = visitor(observerToVisit)
      }

      index -= 1
    }
    visited
  }
}