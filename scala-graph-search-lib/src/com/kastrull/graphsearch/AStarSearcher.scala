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

    val queue = PriorityQueue[MetaInformedState[S]]()(ordering)
    queue.enqueue(
      MetaInformedState(start, 0, estFurtherCostFunc(start)))

    def enqueChildren(state: S, cost: Int) {
      val children = state childStates
      val alreadyVisited = { s: S => !visited.contains(s) }
      val uniqueChildren = children filter alreadyVisited
      visited ++= uniqueChildren

      val metaChildren = uniqueChildren.map { s => MetaInformedState(s, cost + 1, estFurtherCostFunc(s)) }
      queue.enqueue(metaChildren: _*)
    }

    def solutionFound(state: S, cost: Int, loopCount: Int) = {
      println("iter: " + loopCount)
      println("cost: " + cost)
      Some(state)
    }

    @tailrec
    def loop(loopCount: Int): Option[S] = {

      if (loopCount % 10000 == 0) println("iter: " + loopCount)

      if (queue.isEmpty) None
      else {
        val MetaInformedState(state, cost, estFurtherCost) = queue.dequeue

        if (isTarget(state))
          solutionFound(state, cost, loopCount)
        else {
          enqueChildren(state, cost)
          loop(loopCount + 1)
        }
      }
    }

    loop(1)
  }
}