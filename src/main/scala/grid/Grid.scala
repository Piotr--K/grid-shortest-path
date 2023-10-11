package grid

import scala.util.Random
import scala.collection.immutable._

//Implementation of finding shortest path in the grid (where wrapping around is allowed)
//using bfs algo
class Grid private (private val grid: Vector[Vector[Boolean]]) {
  type Cell = (Int, Int)
  
  def shortestPath(start: Cell, end: Cell): Option[List[Cell]] = {
    def bfs(queue: Queue[(Cell, List[Cell])], visited: Set[Cell]): Option[List[Cell]] = {
      if (queue.isEmpty) {
        None // No path found
      } 
      else {
        val ((currentCell, path), updatedQueue) = queue.dequeue

        if (currentCell == end) {
          Some(path :+ currentCell)
        } 
        else {
          val neighbors = getNeighbors(currentCell).filter(neighbor => isValid(neighbor) && !visited.contains(neighbor))

          val newVisited = visited ++ neighbors
          val newQueue = updatedQueue.enqueue(neighbors.map(neighbor => (neighbor, path :+ currentCell)))

          bfs(newQueue, newVisited)
        }
      }
    }

    val initialQueue = Queue((start, List()))

    bfs(initialQueue, Set(start))
  }
  
  val Columns = grid.length
  val Rows = if (Columns > 0) grid(0).length else 0


  private def isValid(cell: Cell): Boolean = {
    cell._2 >= 0 && cell._2 < Columns && cell._1 >= 0 && cell._1 < Rows
  }

  //we allow wrapping grid around
  //each node should have 4 neighbours (x-1, y)(x+1,y) (x, y-1) (x, y+1)
  private def getNeighbors(node: Cell): List[Cell] = {
    val maxColumns = if (grid.isEmpty) 0 else grid.map(_.size).max
    val maxRows = grid.size

    val n1 = if(node._1 > 0) (node._1 - 1, node._2) else (maxColumns - 1, node._2)
    val n2 = if(node._1 < maxColumns - 1) (node._1 + 1, node._2) else (0, node._2)
    val n3 = if(node._2 > 0) (node._1, node._2 - 1) else (node._1, maxRows - 1)
    val n4 = if(node._2 < maxRows - 1) (node._1, node._2 + 1) else (node._1, 0)
    List(n1, n2, n3, n4)
  }
}
object Grid {
  def apply(rows: Int, columns: Int, count: Int): Grid = {
    val obstacles: Set[(Int,Int)] = randomObstacleSet(rows, columns, count, Set.empty[(Int,Int)])
    val grid = Vector.tabulate(rows, columns)((x,y) => if (obstacles.contains(x,y)) false else true)
    new Grid(grid)
  }

  private def randomObstacleSet(rows: Int, columns: Int, count: Int, currentSet: Set[(Int, Int)]): Set[(Int, Int)] = {
    if(rows * columns >= count)
    {
      val random = new Random()
      if (currentSet.size < count)
      {
        //TODO: this needs to be random int in the range
        val rRow = random.nextInt(rows)
        val rColumn = random.nextInt(columns)
        val res = randomObstacleSet(rows, columns, count, currentSet ++ Set((rRow, rColumn)))

        res
      }
      else
      {
        currentSet
      }
    }
    else {
      currentSet
    }
  }
}
