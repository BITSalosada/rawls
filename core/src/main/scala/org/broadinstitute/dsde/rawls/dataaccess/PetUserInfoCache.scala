package org.broadinstitute.dsde.rawls.dataaccess

import java.util.concurrent.TimeUnit

import com.google.common.cache.{CacheBuilder, CacheLoader}
import org.broadinstitute.dsde.rawls.model.{RawlsUserEmail, UserInfo}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

trait PetUserInfoCache { this: SamDAO =>
  val cacheEnabled: Boolean
  val cacheExpiryTime: FiniteDuration
  val cacheMaxSize: Int
  implicit val executionContext: ExecutionContext

  case class UserEmailAndProject(userEmail: RawlsUserEmail, googleProject: String)
  //"Fast" lookup of pet's access token, using the cache.
  def getCachedPetUserInfo(userEmail: RawlsUserEmail, googleProject: String): Future[UserInfo] = {
    if (cacheEnabled) {
      petUserInfoCache.get(UserEmailAndProject(userEmail, googleProject))
    } else {
      getPetUserInfo(userEmail, googleProject)
    }
  }

  private val petUserInfoCache = CacheBuilder.newBuilder()
    .expireAfterWrite(cacheExpiryTime.toMinutes, TimeUnit.MINUTES)
    .maximumSize(cacheMaxSize)
    .build(
      new CacheLoader[UserEmailAndProject, Future[UserInfo]] {
        def load(userEmailAndProject: UserEmailAndProject) = {
          val userInfo = getPetUserInfo(userEmailAndProject.userEmail, userEmailAndProject.googleProject)
          userInfo
        }
      }
    )

  //"Slow" lookup of pet's UserInfo. The cache calls this when it needs to.
  private def getPetUserInfo(userEmail: RawlsUserEmail, googleProject: String): Future[UserInfo] = {
    for {
      petSAJson <- getPetServiceAccountKeyForUser(googleProject, userEmail)
      petUserInfo = GoogleServicesDAO.getUserInfoUsingJson(petSAJson)
    } yield {
      petUserInfo
    }
  }
}
