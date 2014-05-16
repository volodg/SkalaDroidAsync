package org.jff.xml.dom

import scala.collection.mutable
import org.jff.xml.{XMLAttributeSet, XMLParser}
import org.jff.xml.errors.ParseError

class DOMXMLParser extends XMLParser {

  var result: (XMLElement, Error) = (null, new Error("DOMXMLParser - nothing was parsed"))

  private var currentElement: XMLElement = null

  def parserDidStartDocument() {
  }

  def parserDidEndDocument() {
  }

  def onStartElement(elementName: String, attributesSet: XMLAttributeSet) {

    val previousElement = currentElement

    val attributes = attributesSet.toMap

    currentElement = new XMLElement(previousElement, elementName, attributes)
  }

  def didEndElement(elementName: String) {

    val parent = currentElement.parent

    if (parent == null) {
      result = (currentElement, null)
      return
    }

    if (parent.children == null)
      parent.children = new mutable.ArrayBuffer

    parent.children += currentElement

    currentElement = parent
  }

  def foundText(text: String) {

    currentElement.text = text
  }

  def parseErrorOccurred(error: ParseError) {

    result = (null, error)
  }
}

