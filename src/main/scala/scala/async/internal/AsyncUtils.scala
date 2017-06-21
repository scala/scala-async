/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async.internal

object AsyncUtils {


  private def enabled(level: String) = sys.props.getOrElse(s"scala.async.$level", "false").equalsIgnoreCase("true")

  private[async] val verbose = enabled("debug")
  private[async] val trace   = enabled("trace")

  @inline private[async] def vprintln(s: => Any): Unit = if (verbose) println(s"[async] $s")

  @inline private[async] def trace(s: => Any): Unit = if (trace) println(s"[async] $s")
}
