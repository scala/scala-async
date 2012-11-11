/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

/*
 * @author Philipp Haller
 */
object AsyncUtils {

  private val verbose = false
  
  private[async] def vprintln(s: => Any): Unit = if (verbose)
    println("[async] "+s)
}
