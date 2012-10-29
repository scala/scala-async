/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context

/*
 * @author Philipp Haller
 */
trait AsyncUtils {

  val verbose = true
  
  protected def vprintln(s: Any): Unit = if (verbose)
    println("[async] "+s)
  
  /* Symbol of the `Async.await` method in context `c`.
   */
  protected def awaitSym(c: Context): c.universe.Symbol = {
    val asyncMod = c.mirror.staticModule("scala.async.Async")
    val tpe = asyncMod.moduleClass.asType.toType
    tpe.member(c.universe.newTermName("await"))
  }

}
