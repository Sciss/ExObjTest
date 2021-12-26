/*
 *  IGeneratorEvent.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.exnew
package impl

import de.sciss.lucre.Exec
import de.sciss.lucre.Log.{event => logEvent}

trait IGeneratorEvent[T <: Exec[T], A] extends IEventImpl[T, A] {
  final def fire(update: A)(implicit tx: T): Unit = {
    logEvent.debug(s"$this fire $update")
    IPush(this, update)(tx, targets)
  }
}