package com.kastrull.graphsearch

trait State[S <: State[S]] {
  def childStates(): Seq[S]
}

case class MetaState[S <: State[S]](
  state: S,
  cost: Int)

case class MetaInformedState[S <: State[S]](
  state: S,
  cost: Int,
  estFurtherCost: Int)

trait Searcher[S <: State[S]] {

  type Target = (S) => Boolean

  def search(start: S, target: Target): Option[S]
}

trait InformedSearcher[S <: State[S]] {

  type Target = (S) => Boolean

  def search(
    start: S,
    target: Target,
    estFurtherCostFunc: (S) => Int): Option[S]
}