package org.jff.xml.dom

import scala.collection.{mutable, immutable}

class XMLElement(val parent: XMLElement,
                 val elementName: String,
                 val attributes: immutable.Map[String, String]) {

  var text: String = null
  var children: mutable.ArrayBuffer[XMLElement] = null

  def subElementByName(name: String): Option[XMLElement] = {
    children.find(_.elementName == name)
  }

  def subElementsByName(name: String): Array[XMLElement] = {
    children.filter(_.elementName == name).toArray
  }
}
