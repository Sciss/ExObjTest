package de.sciss.lucre.exnew

object ExImport extends ExImport
trait ExImport {
  implicit def stringLiteralExOps(x: String): StringLiteralExOps = new StringLiteralExOps(x)
}