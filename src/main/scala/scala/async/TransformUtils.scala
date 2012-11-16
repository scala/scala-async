/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context

/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
class TransformUtils[C <: Context](val c: C) {
  import c.universe._

  protected def defaultValue(tpe: Type): Literal = {
    val defaultValue: Any =
      if (tpe <:< definitions.BooleanTpe) false
      else if (definitions.ScalaNumericValueClasses.exists(tpe <:< _.toType)) 0
      else if (tpe <:< definitions.AnyValTpe) 0
      else null
    Literal(Constant(defaultValue))
  }
}
