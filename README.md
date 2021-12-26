# ExObjTest

[![Build Status](https://github.com/Sciss/ExObjTest/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/ExObjTest/actions?query=workflow%3A%22Scala+CI%22)

## statement

This project is an experiment to add full [Lucre](https://github.com/Sciss/Lucre)
[Obj type](https://github.com/Sciss/Lucre/blob/main/core/shared/src/main/scala/de/sciss/lucre/Obj.scala)
based on [Lucre Ex](https://github.com/Sciss/Lucre/blob/main/expr0/shared/src/main/scala/de/sciss/lucre/expr/graph/Ex.scala). In short, while `Ex` expression trees form part of
`Act`, `Control`, and `Widget` programs, which are "expanded" in-memory when using their respective `Runner`s, this project attempts to formulate a version that can create persistent
expressions that map other objects to existing expression objects such as `IntObj` or `BooleanObj`. The problems and challenges are described in
[this working document](https://github.com/Sciss/Lucre/blob/wip-int-ex-obj/notes/ExObj.md).
The aim is to build a minimum viable skeleton based on rewriting existing `Ex` API, and when successful, port the changes back to the main Lucre project.

This project is (C)opyright 2021&ndash;2022 by Hanns Holger Rutz. All rights reserved. 
It is released under 
the [GNU Affero General Public License](https://github.com/Sciss/ExObjTest/raw/main/LICENSE) v3+.
The software comes with absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`.

## requirements / installation

This project builds with [sbt](http://www.scala-sbt.org/) against Scala 2.13.

