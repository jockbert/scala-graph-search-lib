package com.kastrull.graphsearch

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set
import scala.annotation.tailrec

class AStarSearcher[S <: State[S]]
  extends InformedSearcher[S] {

  def search(start: S, isTarget: Target, estFurtherCostFunc: (S) => Int): Option[S] = {

    val visited = Set[S]()

    // priority queue orders by highest value, so cost is negated.
    val ordering = Ordering.by[MetaInformedState[S], Int] {
      case MetaInformedState(state, cost, estFurtherCost) => -(cost + estFurtherCost)
      case _ => 0
    }

    val fringe = PriorityQueue[MetaInformedState[S]]()(ordering)
    fringe.enqueue(
      MetaInformedState(start, 0, estFurtherCostFunc(start)))

    def enqueueChildren(state: S, cost: Int) {
      val children = state childStates
      val alreadyVisited = { s: S => !visited.contains(s) }
      val uniqueChildren = children filter alreadyVisited
      visited ++= uniqueChildren

      val metaChildren = uniqueChildren.map { s => MetaInformedState(s, cost + 1, estFurtherCostFunc(s)) }
      fringe.enqueue(metaChildren: _*)
    }

    def solutionFound(state: S, cost: Int, loopCount: Int) = {
      println("iter: " + loopCount)
      println("cost: " + cost)
      Some(state)
    }

    @tailrec
    def loop(loopCount: Int): Option[S] =
      if (fringe.isEmpty) None
      else {
        val MetaInformedState(state, cost, estFurtherCost) = fringe.dequeue

        if (isTarget(state))
          solutionFound(state, cost, loopCount)
        else {
          enqueueChildren(state, cost)
          loop(loopCount + 1)
        }
      }

    loop(1)
  }
}