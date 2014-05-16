package org.jff.xml

import android.content.res.XmlResourceParser
import org.xmlpull.v1.XmlPullParser
import android.util.Log
import org.jff.xml.errors.ParseError

//TODO use scala XML instead of
object XmlSaxParserAdapter {

  private trait SaxParser extends XMLAttributeSet {

    def next(): Int
    def getEventType: Int
    def close() = {}
    def getName: String
    def getText: String
  }

  private class XmlPullParserAdapter(xml: XmlPullParser) extends SaxParser {

    def next(): Int = { xml.next() }
    def getEventType: Int = { xml.getEventType }
    def getName: String = { xml.getName }
    def getText: String = { xml.getText }

    def getAttributeCount: Int = { xml.getAttributeCount }
    def getAttributeName(i: Int): String = { xml.getAttributeName(i) }
    def getAttributeValue(i: Int): String = { xml.getAttributeValue(i) }
  }

  private class XmlResourceParserAdapter(xml: XmlResourceParser) extends SaxParser {

    def next(): Int = { xml.next() }
    def getEventType: Int = { xml.getEventType }
    override def close() = { xml.close() }
    def getName: String = { xml.getName }
    def getText: String = { xml.getText }

    def getAttributeCount: Int = { xml.getAttributeCount }
    def getAttributeName(i: Int): String = { xml.getAttributeName(i) }
    def getAttributeValue(i: Int): String = { xml.getAttributeValue(i) }
  }

  def parse(xml: SaxParser, xmlObserver: XMLParser) {

    try {
      xml.next()
      var eventType = xml.getEventType
      while (eventType != XmlPullParser.END_DOCUMENT) {

        eventType match {

          case XmlPullParser.START_DOCUMENT =>
            xmlObserver.parserDidStartDocument()
          case XmlPullParser.END_DOCUMENT =>
            xmlObserver.parserDidEndDocument()
            xml.close()
          case XmlPullParser.START_TAG =>
            xmlObserver.onStartElement(xml.getName, xml)
          case XmlPullParser.END_TAG =>
            xmlObserver.didEndElement(xml.getName)
          case XmlPullParser.TEXT =>
            xmlObserver.foundText(xml.getText)
          case event =>
            Log.d("J_XML", "event with type: " + event + "was ignored")
        }

        eventType = xml.next()
      }

    } catch {

      case ex: Throwable =>
        xmlObserver.parseErrorOccurred(new ParseError(ex))
        xml.close()
    }
  }

  def parse(xml: XmlResourceParser, xmlObserver: XMLParser) {

    parse(new XmlResourceParserAdapter(xml), xmlObserver)
  }

  def parse(xml: XmlPullParser, xmlObserver: XMLParser) {

    parse(new XmlPullParserAdapter(xml), xmlObserver)
  }
}
