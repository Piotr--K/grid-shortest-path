package grid

import example.BaseSpec
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.math.floor
import grid.Grid

class GridSpec extends BaseSpec {
  def CellGen (grid: Grid): Gen[(Int, Int)] = for {
    x <- Gen.choose(0, grid.Columns - 1)
    y <- Gen.choose(0, grid.Rows - 1)
  } yield (x, y)

  "Grid shortest path" should "return shortest path between 2 cells" in {
    assert(Grid(3,3,0).shortestPath((0,0),(0,2)) == Some(List((0,0),(0,2))))
    assert(Grid(6,6,0).shortestPath((1,0),(4,3)) == Some(List((1,0),(0,0),(5,0),(4,0),(4,5),(4,4),(4,3))))
  }

  //property based testing:
  //1. for all cell pairs (start, end) belonging to the grid
  //path cant be longer than grid.width / 2 + grid.length / 2 + 1 - because of the wrapping
  //2. for any start cell == end cell path = start cell
  //3. for any combination of start, end cell path is not empty
  "For any combo of start, end Cells in the grid - shortest path" should "be smaller that grid.Columns + grid.Rows" in {
    //TODO: could also randomly generate diff grids
    val grid = Grid(6,6,0)
    forAll(CellGen(grid), CellGen(grid)) {
      (start,end) => grid.shortestPath(start, end).get.length should be <= (floor(grid.Columns / 2).toInt + floor(grid.Rows / 2).toInt + 1)
    }
  }
  
  "For any combo of start, end Cells in the grid - shortestPath" should "return value >= 0" in {
    val grid = Grid(6,6,0)
    forAll(CellGen(grid), CellGen(grid)) {
      (start,end) =>
        grid.shortestPath(start, end).get.length should be >= 1
    }
  }
}
