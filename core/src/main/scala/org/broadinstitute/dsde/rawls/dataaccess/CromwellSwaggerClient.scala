package org.broadinstitute.dsde.rawls.dataaccess


import com.typesafe.scalalogging.LazyLogging
import cromwell.client.ApiClient
import cromwell.client.api.WomtoolApi
import cromwell.client.model.WorkflowDescription
import org.broadinstitute.dsde.rawls.model.UserInfo

class CromwellSwaggerClient(cromwellBasePath: String) extends LazyLogging {


  private def getRandomCromwellWomtoolApi(accessToken: String): WomtoolApi = {
    logger.info("CROMWELL BASE PATH: " + cromwellBasePath)
    val apiClient = new ApiClient()
    apiClient.setAccessToken(accessToken)
    apiClient.setBasePath(cromwellBasePath)
    new WomtoolApi(apiClient)
  }

  def validate(userInfo: UserInfo, wdl: String): WorkflowDescription = {
    getRandomCromwellWomtoolApi(userInfo.accessToken.token).describe("v1", wdl, null, null, null, null)
  }

}