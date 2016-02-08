package org.broadinstitute.dsde.rawls.dataaccess.slick

import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext

class DataAccessComponent(val driver: JdbcProfile)(implicit val executionContext: ExecutionContext)
extends DriverComponent with AllComponents {
  import driver.api._

  lazy val schema =
    pendingBucketDeletionQuery.schema
}
