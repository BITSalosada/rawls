import sbt._

object Dependencies {
  val akkaV = "2.4.19"
  val akkaHttpV = "10.0.10"
  val sprayV = "1.3.4"
  val olderSprayV = "1.3.3" // "latest" is different for various spray packages
  val slickV = "3.2.1"

  val googleV = "1.23.0"

  val cromwellVersion = "31-39223b8"

  val catsVersion = "1.0.0-MF"

  def excludeGuavaJDK5(m: ModuleID): ModuleID = m.exclude("com.google.guava", "guava-jdk5")

  val slick: ModuleID =         "com.typesafe.slick" %% "slick"           % slickV
  val slickHikariCP: ModuleID = "com.typesafe.slick" %% "slick-hikaricp"  % slickV

  val excludeAkkaActor =        ExclusionRule(organization = "com.typesafe.akka", name = "akka-actor_2.12")
  val excludeAkkaStream =       ExclusionRule(organization = "com.typesafe.akka", name = "akka-stream_2.12")

  val akkaActor: ModuleID =         "com.typesafe.akka"   %%  "akka-actor"           % akkaV
  val akkaContrib: ModuleID =       "com.typesafe.akka"   %%  "akka-contrib"         % akkaV
  val akkaSlf4j: ModuleID =         "com.typesafe.akka"   %%  "akka-slf4j"           % akkaV
  val akkaHttp: ModuleID =          "com.typesafe.akka"   %%  "akka-http"            % akkaHttpV           excludeAll(excludeAkkaActor, excludeAkkaStream)
  val akkaHttpSprayJson: ModuleID = "com.typesafe.akka"   %%  "akka-http-spray-json" % akkaHttpV
  val akkaTestKit: ModuleID =       "com.typesafe.akka"   %%  "akka-testkit"         % akkaV     % "test"
  val akkaHttpTestKit: ModuleID =   "com.typesafe.akka"   %%  "akka-http-testkit"    % akkaHttpV % "test"

  val googleApiClient: ModuleID =             excludeGuavaJDK5("com.google.api-client"  % "google-api-client"                         % googleV)
  val googleCloudBilling: ModuleID =          excludeGuavaJDK5("com.google.apis"        % "google-api-services-cloudbilling"          % ("v1-rev14-" + googleV))
  val googleGenomics: ModuleID =              excludeGuavaJDK5("com.google.apis"        % "google-api-services-genomics"              % ("v1-rev527-" + googleV))
  val googleStorage: ModuleID =               excludeGuavaJDK5("com.google.apis"        % "google-api-services-storage"               % ("v1-rev116-" + googleV))
  val googleCloudResourceManager: ModuleID =  excludeGuavaJDK5("com.google.apis"        % "google-api-services-cloudresourcemanager"  % ("v1-rev457-" + googleV))

  val googleCompute: ModuleID =           "com.google.apis"   % "google-api-services-compute"           % ("v1-rev164-" + googleV)
  val googleAdminDirectory: ModuleID =    "com.google.apis"   % "google-api-services-admin-directory"   % ("directory_v1-rev88-" + googleV)
  val googlePlus: ModuleID =              "com.google.apis"   % "google-api-services-plus"              % ("v1-rev531-" + googleV)
  val googleOAuth2: ModuleID =            "com.google.apis"   % "google-api-services-oauth2"            % ("v1-rev136-" + googleV)
  val googlePubSub: ModuleID =            "com.google.apis"   % "google-api-services-pubsub"            % ("v1-rev364-" + googleV)
  val googleServicemanagement: ModuleID = "com.google.apis"   % "google-api-services-servicemanagement" % ("v1-rev370-" + googleV)
  val googleGuava: ModuleID =             "com.google.guava"  % "guava" % "19.0"
  val googleRpc: ModuleID =               "io.grpc" % "grpc-core" % "1.5.0"
  val googleOAuth2too: ModuleID = "com.google.auth" % "google-auth-library-oauth2-http" % "0.9.0"

  val catsCore: ModuleID =    "org.typelevel" %% "cats-core"    % catsVersion
  val catsKernel: ModuleID =  "org.typelevel" %% "cats-kernel"  % catsVersion
  val catsMacros: ModuleID =  "org.typelevel" %% "cats-macros"  % catsVersion

  // metrics-scala transitively pulls in io.dropwizard.metrics:metrics-core
  val metricsScala: ModuleID =       "nl.grons"              %% "metrics-scala"    % "3.5.6"
  val metricsStatsd: ModuleID =      "com.readytalk"         %  "metrics3-statsd"  % "4.2.0"

  val scalaLogging: ModuleID =    "com.typesafe.scala-logging"    %% "scala-logging"        % "3.7.1"
  val jacksonCore: ModuleID =     "com.fasterxml.jackson.core"    % "jackson-core"          % "2.8.10"
  val jodaTime: ModuleID =        "joda-time"                     % "joda-time"             % "2.9.4"
  val jodaConvert: ModuleID =     "org.joda"                      % "joda-convert"          % "1.8"
  val typesafeConfig: ModuleID =  "com.typesafe"                  % "config"                % "1.3.0"
  val ravenLogback: ModuleID =    "com.getsentry.raven"           % "raven-logback"         % "7.8.6"
  val swaggerUI: ModuleID =       "org.webjars"                   % "swagger-ui"            % "2.2.5"
  val commonsJEXL: ModuleID =     "org.apache.commons"            % "commons-jexl"          % "2.1.1"
  val httpClient: ModuleID =      "org.apache.httpcomponents"     % "httpclient"            % "4.5.3"  // upgrading a transitive dependency to avoid security warnings
  val parserCombinators =         "org.scala-lang.modules"        %% "scala-parser-combinators" % "1.0.6"
  val mysqlConnector: ModuleID =  "mysql"                         % "mysql-connector-java"  % "5.1.42"
  val liquibaseCore: ModuleID =   "org.liquibase"                 % "liquibase-core"        % "3.5.3"
  val logbackClassic: ModuleID =  "ch.qos.logback"                % "logback-classic"       % "1.2.2"
  val scalaUri: ModuleID =        "io.lemonlabs"                  %% "scala-uri"            % "0.5.3"
  val scalatest: ModuleID =       "org.scalatest"                 %% "scalatest"            % "3.0.1" % "test"
  val mockito: ModuleID =         "org.mockito"                   % "mockito-core"          % "2.7.22" % "test"
  val mockserverNetty: ModuleID = "org.mock-server"               % "mockserver-netty"      % "3.9.2" % "test"
  val ficus: ModuleID =           "com.iheart"                    %% "ficus"                % "1.4.0"

  val cromwellWdl: ModuleID = ("org.broadinstitute" %% "cromwell-wdl-model-draft2" % cromwellVersion
    exclude("org.typelevel", "cats_2.11")
    exclude("io.spray", "spray-json_2.11")
    exclude("com.typesafe.scala-logging", "scala-logging_2.11")
    exclude("io.circe", "circe-core_2.11")
    exclude("org.typelevel", "cats_2.12")
    exclude("io.spray", "spray-json_2.12")
    exclude("io.spray", "akka-parsing_2.12")
    exclude("io.spray", "akka-stream_2.12")
    exclude("com.typesafe.akka", "akka-actor_2.12"))

  val workbenchModelV  = "0.10-6800f3a"
  val workbenchModel: ModuleID = "org.broadinstitute.dsde.workbench" %% "workbench-model"  % workbenchModelV
  val workbenchGoogleV = "0.16-0027221"
  val workbenchGoogle: ModuleID = "org.broadinstitute.dsde.workbench" %% "workbench-google" % workbenchGoogleV
  val workbenchGoogleMocks: ModuleID = "org.broadinstitute.dsde.workbench" %% "workbench-google" % workbenchGoogleV % "test" classifier "tests"

  val metricsDependencies = Seq(
    metricsScala,
    metricsStatsd,
    akkaHttp,
    scalatest,
    mockito
  )

  val googleDependencies = metricsDependencies ++ Seq(
    akkaHttpSprayJson,
    akkaHttp,
    akkaActor,
    akkaHttpTestKit,
    scalatest,

    googleCloudBilling,
    googleGenomics,
    googleStorage,
    googleCloudResourceManager,
    googleCompute,
    googleAdminDirectory,
    googlePlus,
    googleOAuth2,
    googleOAuth2too,
    googlePubSub,
    googleServicemanagement,
    googleGuava
  )

  val utilDependencies = Seq(
    scalaLogging,
    akkaActor,
    akkaHttpTestKit,
    catsCore,
    catsKernel,
    catsMacros,
    scalatest,
    mockito
  )

  val modelDependencies = Seq(
    // I am not certain why I need jackson-core here but IntelliJ is confused without it and tests don't run
    jacksonCore,
    akkaHttpSprayJson,
    akkaHttp,
    jodaTime,
    jodaConvert,
    scalaLogging,
    httpClient,
    googleApiClient,
    scalaUri,
    scalatest
  )

  val rawlsCoreDependencies: Seq[ModuleID] = modelDependencies ++ googleDependencies ++ metricsDependencies ++ Seq(
    typesafeConfig,
    parserCombinators,
    ravenLogback,
    slick,
    slickHikariCP,
    akkaHttp,
    akkaHttp,
    swaggerUI,
    commonsJEXL,
    cromwellWdl,
    catsCore,
    catsKernel,
    catsMacros,
    mysqlConnector,
    liquibaseCore,
    logbackClassic,
    akkaTestKit,
    akkaHttpTestKit,
    mockserverNetty,
    mockito,
    googleRpc,
    workbenchModel,
    workbenchGoogle,
    workbenchGoogleMocks,
    ficus
  )
}
