package org.jff.xml

import org.jff.xml.errors.ParseError
import scala.collection.mutable
import scala.collection.immutable

trait XMLAttributeSet {

  def getAttributeCount: Int
  def getAttributeName(i: Int): String
  def getAttributeValue(i: Int): String

  def toMap:immutable.Map[String, String] = {

    val attributes = mutable.Map[String, String]()

    for (i <- 0 to getAttributeCount - 1) {

      attributes += getAttributeName(i) -> getAttributeValue(i)
    }

    attributes.toMap
  }
}

trait XMLParser {

  def parserDidStartDocument()
  def parserDidEndDocument()

  def onStartElement(elementName: String, attributes: XMLAttributeSet)
  def didEndElement(elementName: String)

  def foundText(text: String)

  def parseErrorOccurred(error: ParseError)
}
