/*
 *  IPublisher.scala
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

import de.sciss.lucre.{Exec, Txn}

trait IPublisher[T <: Txn[T], +A] {
  def changed: IEvent[T, A]
}