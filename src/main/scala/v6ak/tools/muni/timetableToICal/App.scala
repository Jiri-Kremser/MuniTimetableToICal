package v6ak.tools.muni.timetableToICal

import resource.managed
import xml.XML
import Console.err
import org.scala_tools.time.Imports._
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.{Date=>JDate}
import net.fortuna.ical4j.model.property._
import net.fortuna.ical4j.model.component.VEvent
import net.fortuna.ical4j.model.{Property => IProperty, Dur, Recur, Date => IDate, Calendar => ICalendar, DateTime => IDateTime}

class MyComponent(name:String, var value:String) extends IProperty(name){ // Chro chro. Ale jinak mi to fakt nešlo.
	def getValue = value
	def setValue(x:String){value = x}
	def validate(){}
}

/**
 * @author Vít Šesták 'v6ak'
 */
object App {

	final val Days = Map(
		"Po"-> 1,
		"Mon"->1,
		"Út"-> 2,
		"Tue"->2,
		"St"-> 3,
		"Wed"->3,
		"Čt"-> 4,
		"Thu"->4,
		"Pá"-> 5,
		"Fri"->5,
		"So"-> 6,
		"Sat"->6,
		"Ne"-> 7,
		"Sun"->7
	)

	def main(args : Array[String]) {
		if(args.length != 3){
			err println "usage: <appLauncher> sourceXmlFile targetIcsFile dateTo"
			err println "dateTo: YYYY-MM-DD"
			System exit 1
		}

		val until = new IDate(new SimpleDateFormat("y-M-d").parse(args(2)).getTime+24*60*60*1000)
		val from = args(0)
		val to = args(1)
		val xml = XML loadFile from
		val calendar = new ICalendar

		val startDate = DateTime.now
		def date(day:Int, time: String) = {
			val timeParts = time split ':'
			if(timeParts.length != 2){
				throw new Exception("Invalid time: "+time)
			}
			val d = startDate.withDayOfWeek(day).withTime(timeParts(0).toInt, timeParts(1).toInt, 0, 0)
			d.getMillis
		}

		xml\"tabulka"\"den" foreach { den =>
			val dayName = den.attribute("id").get.toString
			val day = Days(dayName)
			den\"radek"\"slot" foreach { slot =>
				val fromTime = slot.attribute("odcas").get.toString
				val toTime = slot.attribute("docas").get.toString
				val place = (slot\"mistnosti"\"mistnost"\"mistnostozn").text
				val akce = slot\"akce"
				val code = (akce\"kod").text
				val name = (akce\"nazev").text
				val fromMilis = date(day, fromTime)
				val toMilis = date(day, toTime)
				var isNote = false
				val dateFormat = new java.text.SimpleDateFormat("y-d-M H:mm")
				val year = java.util.Calendar.getInstance.get(java.util.Calendar.YEAR).toString
				slot\"poznamka" foreach { poznamka =>
                                	val id = poznamka.attribute("id").get.toString
					isNote = true
					val note = (xml\"poznamky"\"poznamka" filter (x => (x \ "@id").text equals id)).text
					val TimeRecord = """\p{Upper}\p{Lower} ([0-3]?\d)\. ([0-1]?\d)\. ([0-2]?\d:[0-6]\d)--([0-2]?\d:[0-6]\d)""".r
					note split """,\s(?=\p{Upper}\p{Lower})""" foreach { chunk =>
						val record = TimeRecord.findAllIn(chunk).matchData.toList(0)
						val day = record.group(1)
						val month = record.group(2)
						val datePrefix = year + "-" + day + "-" + month + " "
						val fromTimeMilis = dateFormat.parse(datePrefix + record.group(3)).getTime
						val toTimeMilis = dateFormat.parse(datePrefix + record.group(4)).getTime
						val newEvent = createEvent(fromTimeMilis.toLong, toTimeMilis.toLong, code+"@"+place+": "+name, place)
                                		calendar.getComponents.add(newEvent)
					}
				}
				if (!isNote) {
					val event = createEvent(fromMilis, toMilis, code+"@"+place+": "+name, place)
					event.getProperties.add(new RRule(new Recur(Recur.WEEKLY, until)))
					calendar.getComponents.add(event)
				}
			}
		}
		for(out <- managed(new FileWriter(to))){
			out write calendar.toString
		}
	}
	def createEvent(fromMilis : Long, toMilis : Long, summary : String, location : String) = { 
		val event = new VEvent(new IDate(new JDate(fromMilis)), new Dur(new JDate(fromMilis), new JDate(toMilis)), summary)
		List(
			"DTSTAMP",
			"DTSTART"
		) foreach { name =>
			event.getProperties.removeAll(event.getProperties(name))
		}
		List(
			new MyComponent("DTSTAMP;TZID=Europe/Prague", new IDateTime(fromMilis).toString+"Z"),
			new MyComponent("DTSTART;TZID=Europe/Prague", new IDateTime(fromMilis).toString+"Z"),
			new Location(location)
		) foreach {
			event.getProperties.add(_)
		}
		event
	}

}
