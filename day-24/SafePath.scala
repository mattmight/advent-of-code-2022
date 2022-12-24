
import scala.collection.mutable.HashMap ;
import scala.collection.mutable.HashSet ;
import scala.collection.mutable.Queue ;


/* 
 World behaves like a lazy look-up table that can tell you the contents
 of any tile at any time point.  

 To determine the contents of a tile at any point in time, it looks at the
 surrounding tiles (including wrapping around) in the previous time step.

 This allows blizzard simulations to be efficiently shared between branches
 of the search.
 */
object World {

   // Tracks the size of the world:
   var x_max : Int = 0 ;
   var y_max : Int = 0 ;

   // The central look-up table: X * Y * Time -> [Char]
   val spacetime : HashMap[(Int,Int,Int),List[Char]] = HashMap() ;
 
   // The entry point on the map:
   val entry : (Int,Int) = (1,0)

   // The exit point on the map:
   lazy val exit : (Int,Int) = (x_max - 1,y_max)

   // Some helper functions for blizzard coordinate math that
   // correctly wrap the coordinates when at the edge of the map:
   object bliz {

     def northOf(x : Int, y: Int) : (Int,Int) = {
       // northern border?
       if (y == 1) { return (x, y_max - 1) } 
       else        { return (x, y-1) }
     }

     def southOf(x : Int, y: Int) : (Int,Int) = {
       // southern border?
       if (y == y_max-1) { return (x, 1) } 
       else              { return (x, y+1) }
     }

     def eastOf(x : Int, y: Int) : (Int,Int) = {
       // eastern border?
       if (x == x_max-1) { return (1,   y) } 
       else              { return (x+1, y) }
     }

     def westOf(x : Int, y: Int) : (Int,Int) = {
       // western border?
       if (x == 1) { return (x_max - 1, y) } 
       else        { return (x-1,       y) }
     }

     
   }

   // Returns whether a given coordiante at a given time is open
   // for occupation by the expedition party:
   def isOpenAt(x : Int, y : Int, time : Int) : Boolean = {
     val chars = World(x,y,time)
     return (chars == List('.'))
   }

   // Returns the set of spacetime locations to which the expedition could move:
   def possibleNextMovesFrom(x : Int, y : Int, time : Int) : List[(Int,Int,Int)] = {
     val nt = time+1
     return (for (nx,ny) <- List( (x+1,y), (x,y), (x-1,y), (x,y+1), (x,y-1) )
                 if isOpenAt(nx,ny,nt)
                 yield (nx, ny, nt))
   }

   // Accessing the spacetime table with syntactic sugar:

   // Returns the contents of the table at this spacetime coordinate:
   def apply(x : Int, y : Int, time : Int) : List[Char] = {
     // Check if it's cached
     val cached = spacetime.get((x,y,time))
     cached match 
       case Some(value) => { return value }
       case None => { /* not yet cached */ }

     // Not yet cached; let's compute it based on the state of the world at time-1

     // First, check if it's an entry or exit:
     if ((x,y) == entry || (x,y) == exit) {
       val chars = List('.') 
       spacetime((x,y,time)) = chars
       return chars
     }

     // Then check if it's a border or over:
     if ((x <= 0) || (y <= 0) || (x >= x_max) || (y >= y_max)) {
       val chars = List('#') 
       spacetime((x,y,time)) = chars
       return chars
     }


     // Check for a blizzard in every direction:
     var chars : List[Char] = List()

     if (World(bliz.northOf(x,y),time-1) contains 'v') {
       chars = 'v' :: chars 
     }

     if (World(bliz.southOf(x,y),time-1) contains '^') {
       chars = '^' :: chars 
     }

     if (World(bliz.eastOf(x,y),time-1) contains '<') {
       chars = '<' :: chars 
     }

     if (World(bliz.westOf(x,y),time-1) contains '>') {
       chars = '>' :: chars 
     }

     // No blizzards at x,y at this time 
     if (chars.isEmpty) {
       chars = List('.')
     }

     // Cache this result rather than re-compute it:
     spacetime((x,y,time)) = chars

     return chars
   }


   // Sugar to take a pair for the first argument:
   def apply(xy : (Int,Int), time : Int) : List[Char] = {
     val (x,y) = xy ;
     return World(x,y,time) 
   }

 
   // Allows updates to the table:
   def update(x : Int, y : Int, time : Int, contents: List[Char]) = {

     spacetime((x,y,time)) = contents
   }


   // Summarizes all the characters at a tile into one character:
   def summary(list : List[Char]) : Char = {
     list match
        case List('#') => '#'
        case List('.') => '.'
        case List('>') => '>'
        case List('^') => '^'
        case List('v') => 'v'
        case List('<') => '<'
        case List('?') => '?'
        case _ => list.length.toString().charAt(0)

   }

   // The starting state for Part 1:
   def initialStatePart1 : AbstractState = {
     return State( 1, 0, 0 )
   }

   // The starting state for Part 2:
   def initialStatePart2 : AbstractState = {
     return BacktrackState( 1, 0, 0, 0 )
   }

   // Print out the state of the world at a given time:
   def printTime(time : Int) = {
     for (y <- 0 to this.y_max) {
       for (x <- 0 to this.x_max) {
         print(summary(World(x,y,time)))
       }
       println()
     }
   }
}


// A state in a state-space search:
trait AbstractState {
  var prev : AbstractState = null ;

  def meetsGoal : Boolean
  def nextStates () : List[AbstractState]

  def time : Int
  def time_= (new_time : Int) : Unit

  def printPath() : Unit = {
    if (this.prev ne null) {
      prev.printPath()
    }
    println(this.toString()) ;
  }

}


// A state for Part 1
// 
// It has expedition location and time.
class State(val x : Int, val y : Int, override var time : Int) extends AbstractState {

  def meetsGoal = (World.exit == (x,y))

  def nextStates() : List[AbstractState] = {
    var nextStates : List[State] = 
    (for (nx,ny,nt) <- World.possibleNextMovesFrom(x,y,time) yield State(nx,ny,nt))

    return nextStates
  }

  private val likely_max_time = 10000

  override def hashCode() : Int = (x * World.y_max + y) * likely_max_time + time

  override def equals(that : Any) : Boolean = {

    val thatt : State = that.asInstanceOf[State]
    
    return (this.x == thatt.x) && (this.y == thatt.y) && (this.time == thatt.time)
  }

  override def toString() : String = s"$x  $y  $time  ${World(x,y,time)}"
}

// A state for Part 2
//
// It adds phases to determine whether it's:
// Phase 0: Entry -> Exit
// Phase 1: Exit -> Entry
// Phase 3: Entry -> Exit
class BacktrackState extends AbstractState {

  var x : Int = -1
  var y : Int = -1
  override var time : Int = -1
  var phase : Int = -1

  def meetsGoal = (World.exit == (x,y) && phase == 2)

  def nextStates() : List[AbstractState] = {
    var nextStates : List[BacktrackState] = 
    (for (nx,ny,nt) <- World.possibleNextMovesFrom(x,y,time) yield BacktrackState(nx,ny,nt,phase))

    return nextStates
  }

  def this(x : Int, y : Int, time : Int, phase : Int) = {
    this()
    this.x = x 
    this.y = y
    this.time = time

    // Check if we've crossed a phase boundary:
    if ((x,y) == World.exit && phase == 0) {

      // We found the exit:
      this.phase = 1

    } else if ((x,y) == World.entry && phase == 1) { 

      // We found the entrance:
      this.phase = 2 

    } else {
      this.phase = phase // Keep the old phase
    }
  }

  private val likely_max_time = 10000

  override def hashCode() : Int = 3*((x * World.y_max + y) * likely_max_time + time) + phase

  override def equals(that : Any) : Boolean = {

    val thatt : BacktrackState = that.asInstanceOf[BacktrackState]
    
    return (this.x == thatt.x) && 
           (this.y == thatt.y) &&
           (this.time == thatt.time) &&
           (this.phase == thatt.phase)

  }

  override def toString() : String = s"$x  $y  $time  $phase ${World(x,y,time)}"

}


object SafePath {

  def find_shortest_path(start : AbstractState) : AbstractState = {

    // TODO: Prune a state if it's already been seen mod blizzard cycle time

    // Keep track of all states visited:
    val visited : HashSet[AbstractState] = HashSet()

    // Keep a queue of next states to scan for BFS:
    val queue : Queue[AbstractState] = Queue()

    // Start with the initial state:
    queue.enqueue(start)
   
    // Look for a final state:
    while (true) {
      val next : AbstractState = queue.dequeue()

      if (next.meetsGoal) {
        return next
      }

      if (!visited.contains(next)) {
        val succs : List[AbstractState] = next.nextStates()

        for (succ <- succs) {
          // Enable backtracking:
          succ.prev = next
        }

        queue.appendAll(succs)

        visited.add(next)
      }

    }

    return null
  }



  def main(args : Array[String]) = {

    // Read in the input into World:
    val allInput = scala.io.Source.fromFile("input.txt").mkString

    val lines = allInput.split("\n") ;

    var y = 0 ;
    for (line <- lines) {
      println (line)
      World.x_max = line.length()-1

      for (x <- 0 to World.x_max) {
         World(x,y,0) = List(line.charAt(x)) ;
      }
      y = y + 1
    }
    
    World.y_max = y-1

    // Find part 1:
    val final_state1 = find_shortest_path(World.initialStatePart1)

    println(s"Part 1: ${final_state1.time}") 

    // Find part 2:
    val final_state2 = find_shortest_path(World.initialStatePart2)
    println(s"Part 2: ${final_state2.time}") 

   }

}
