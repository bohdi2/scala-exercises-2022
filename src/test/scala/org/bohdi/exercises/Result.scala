package org.bohdi.exercises

sealed trait Result[T]
final case class Success[T](value: T) extends Result[T]
final case class Failure[T](reason: String) extends Result[T]

