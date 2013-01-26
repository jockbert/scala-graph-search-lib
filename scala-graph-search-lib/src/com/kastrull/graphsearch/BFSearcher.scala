package com.kastrull.graphsearch

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.annotation.tailrec

final class BFSearcher[S <: State[S]] extends Searcher[S] {

  def search(start: S, isTarget: Target) = {

    val visited = Set[S]()
    val queue = Queue[MetaState[S]]()
    queue.enqueue(MetaState(start, 0))

    def enqueChildren(state: S, cost: Int) {
      val children = state childStates
      val alreadyVisited = { s: S => !visited.contains(s) }
      val uniqueChildren = children filter alreadyVisited
      visited ++= uniqueChildren

      val metaChildren = uniqueChildren.map { MetaState(_, cost + 1) }
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
        val MetaState(state, cost) = queue.dequeue

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