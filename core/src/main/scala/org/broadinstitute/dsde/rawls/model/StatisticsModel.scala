package org.broadinstitute.dsde.rawls.model

import spray.json._

/**
 * Created by mbemis on 7/20/16.
 */

sealed trait Statistic
case class SingleStatistic(value: Double) extends Statistic
case class SummaryStatistics(min: Double, max: Double, mean: Double, stddev: Double) extends Statistic
case class NamedStatistic(name: String, value: Double) extends Statistic
case class EntityStatistics(workspaceNamespace: Option[String], workspaceName: Option[String], entityStats: Seq[NamedStatistic]) extends Statistic
case class StatisticsReport(startDate: String, endDate: String, statistics: Map[String, Statistic])

class StatisticsJsonSupport extends JsonSupport {
  import spray.json.DefaultJsonProtocol._

  implicit val SingleStatisticFormat = jsonFormat1(SingleStatistic)
  implicit val SummaryStatisticsFormat = jsonFormat4(SummaryStatistics)

  implicit object NamedStatisticFormat extends RootJsonFormat[NamedStatistic] {
    override def write(obj: NamedStatistic): JsValue = JsObject(Map(obj.name -> JsNumber(obj.value)))
    override def read(json: JsValue): NamedStatistic = ???
  }
  implicit val EntityStatisticFormat = jsonFormat3(EntityStatistics)

  implicit object StatisticFormat extends RootJsonFormat[Statistic] {
    override def write(obj: Statistic): JsValue = obj match {
      case SingleStatistic(value) => JsObject(Map("value" -> JsNumber(value)))
      case SummaryStatistics(min, max, mean, stddev) => JsObject(Map("min" -> JsNumber(min), "max" -> JsNumber(max), "mean" -> JsNumber(mean), "stddev" -> JsNumber(stddev)))
      case ns:NamedStatistic => NamedStatisticFormat.write(ns)
      case es:EntityStatistics =>
        val entityStats:Map[String,JsValue] = es.entityStats.map{ns => ns.name -> JsNumber(ns.value)}.toMap
        val fields: Map[String,JsValue] =
          Map("entityStats" -> JsObject(entityStats)) ++
            es.workspaceNamespace.map("workspaceNamespace" -> JsString(_)) ++
            es.workspaceName.map("workspaceName" -> JsString(_))
        JsObject(fields)
    }
    override def read(json: JsValue): Statistic = ???
  }

  implicit val StatisticsReportFormat = jsonFormat3(StatisticsReport)
}

object StatisticsJsonSupport extends StatisticsJsonSupport
