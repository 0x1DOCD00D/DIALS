package com.lsc

import Utilz.{ConfigDb, CreateLogger}
import org.slf4j.Logger

object Main:
  val logger: Logger = CreateLogger(classOf[ConfigDb.type])
  @main def runMain(params: String*): Unit =
    logger.info(s"Running DIALS ${if params.isEmpty then "without parameters" else "with parameters: $params"}")
    logger.info("Configuration settings:")
    ConfigDb.apply().foreach { case (key, value) =>
      logger.info(s"$key = $value")
    }