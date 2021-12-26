/*
 *  IChangeGeneratorEvent.scala
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

import de.sciss.lucre.{Exec, Txn}
import de.sciss.model.Change

trait IChangeGeneratorEvent[T <: Txn[T], A] extends IGeneratorEvent[T, Change[A]] with IChangeEventImpl[T, A]