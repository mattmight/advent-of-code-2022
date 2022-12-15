package main

import (
  "fmt"
)

// A type for points:
type Point struct {
  x int64
  y int64
}

// A type for sensor-beacon point pairs:
type PointPair struct {
  sensor Point
  beacon Point
}

// Globals to hold state and limits:
var SensorBeaconPairs []PointPair 
var MaxRadius int64 = -1
var MinX int64 = 10000000
var MaxX int64 = -10000000
var MinY int64 = 10000000
var MaxY int64 = -10000000

// Helper functions:
func min(a int64, b int64) int64 {
  if a < b {
    return a
  }
  return b
}

func max(a int64, b int64) int64 {
  if a > b {
    return a
  }
  return b
}

func abs(n int64) int64 {
  if n < 0 { 
    return -n
  } else {
    return n
  }
}

// Find the radius of this sensor, based on its nearest beacon:
func coverage_radius(pp PointPair) int64 {
  delta_x := abs(pp.sensor.x - pp.beacon.x)
  delta_y := abs(pp.sensor.y - pp.beacon.y)
  return delta_x + delta_y 
}


// Find the Manhattan distance between two points:
func distance(p1 Point, p2 Point) int64 {
  delta_x := abs(p1.x - p2.x)
  delta_y := abs(p1.y - p2.y)
  return delta_x + delta_y 
}

// Check if any sensor would have detected a beacon this point
// instead of its current beacon:
func would_be_detected(p Point) bool {
  for _,sb_pair := range SensorBeaconPairs {
    radius := coverage_radius(sb_pair)
    delta := distance(p, sb_pair.sensor) 
    if delta <= radius {
      return true
    }
  }
  return false
}

// Computes how much can be skipped in the x direction to get to the 
// edge of a sensor's range when a point is within its scanning radius:
func skip_x(x int64, y int64) int64 {

  for _,sb_pair := range SensorBeaconPairs {
    radius := coverage_radius(sb_pair)

    delta := distance(Point{x,y}, sb_pair.sensor) 
    if delta <= radius {

      // How much distance has y direction used?
      delta_y := abs(y - sb_pair.sensor.y) 

      // How much distance has the x direction used?
      delta_x := abs(x - sb_pair.sensor.x)

      x_budget := radius - delta_y - delta_x

      if x_budget < 1 {
        // Forcibly advance at least one square at the radius:
        x_budget = 1
      }

      // fmt.Printf("skippin: %v\n", x_budget) ;

      return x_budget
    }
  }
  fmt.Printf("Unskippable point found: %v, %v\n", x,y)
  return 0
}


// Checks if a square is occupied by a sensor or a beacon:
func is_occupied (x int64, y int64) bool {
  for _,sb := range SensorBeaconPairs {
    // Check to see if this location holds a sensor or beacon:
    if ((x == sb.sensor.x) && (y == sb.sensor.y)) ||
       ((x == sb.beacon.x) && (y == sb.beacon.y)) {
      return true 
    }
  }
  return false 
}


// Scans a row, counting places a missing beacon can't be:
func scan_row(y int64) int64 {

  var detectable_locs int64 = 0

  for i := MinX - MaxRadius; i < MaxRadius + MaxX; i++ {
    if would_be_detected(Point{ i, y }) {
      if !is_occupied( i, y ) {
        detectable_locs++
      }
    }
  }

  return detectable_locs
}

// Prints out a row of the space:
func print64_row(y int64, minx int64, maxx int64) {

  for i := minx; i <= maxx; i++ {
    if would_be_detected(Point { i ,y }) {
      fmt.Printf("#")
    } else {
      fmt.Printf(".") 
    }
  }

  fmt.Println()
}


func main() {

  // The input was so small it was faster to turn it into a literal 
  // with text editor tricks than to write a parser:A

  real_input := []PointPair{
     { Point { 2208586, 2744871}, Point{  2094814, 3380585 } },
     { Point { 3937279, 2452476}, Point{  3597034, 2313095 } },
     { Point { 3535638, 3151860}, Point{  4098735, 3373876 } },
     { Point { 1867584, 2125870}, Point{  2685670, 2236965 } },
     { Point { 2290971, 1583182}, Point{  2685670, 2236965 } },
     { Point { 3137806, 2216828}, Point{  3233556, 2000000 } },
     { Point { 3393352, 331000},  Point{  3233556, 2000000 } },
     { Point { 1444302, 821689},  Point{  1683873, -199301 } },
     { Point { 1084667, 3412239}, Point{  2094814, 3380585 } },
     { Point { 439341,  3916996}, Point{  -290982, 4102300 } },
     { Point { 295460,  2114590}, Point{  362644,  370187 } },
     { Point { 2212046, 3819484}, Point{  2094814, 3380585 } },
     { Point { 3413280, 3862465}, Point{  4098735, 3373876 } },
     { Point { 3744934, 1572303}, Point{  3597034, 2313095 } },
     { Point { 3349047, 2522469}, Point{  3597034, 2313095 } },
     { Point { 171415,  591241},  Point{  362644,  370187 } },
     { Point { 3237499, 2150414}, Point{  3233556, 2000000 } },
     { Point { 559077,  454593},  Point{  362644,  370187 } },
     { Point { 3030733, 2047512}, Point{  3233556, 2000000 } },
     { Point { 1667358, 3956837}, Point{  2094814, 3380585 } },
     { Point { 1850337, 98963},   Point{  1683873, -199301 } },
     { Point { 2699546, 3157824}, Point{  2094814, 3380585 } },
     { Point { 1113195, 98130},   Point{  1683873, -199301 } },
     { Point { 59337,   246804},  Point{  362644,  370187 } },
     { Point { 566043,  29068},   Point{  362644,  370187 } },
     { Point { 2831421, 2581088}, Point{  2685670, 2236965 } },
     { Point { 597818,  749461},  Point{  362644,  370187 } },
  }
 
  _ = real_input ;

  // The sample input data:
  sample_input := []PointPair{
     { Point{ 2,  18 }, Point{ -2, 15  } },
     { Point{ 9,  16}, Point{ 10, 16  } },
     { Point{ 13,  2}, Point{ 15, 3   } },
     { Point{ 12, 14}, Point{ 10, 16  } },
     { Point{ 10, 20}, Point{ 10, 16  } },
     { Point{ 14, 17}, Point{ 10, 16  } },
     { Point{ 8,   7}, Point{ 2, 10   } },
     { Point{ 2,   0}, Point{ 2, 10   } },
     { Point{ 0,  11}, Point{ 2, 10   } },
     { Point{ 20, 14}, Point{ 25, 17  } },
     { Point{ 17, 20}, Point{ 21, 22  } },
     { Point{ 16,  7}, Point{ 15, 3   } },
     { Point{ 14,  3}, Point{ 15, 3   } },
     { Point{ 20,  1}, Point{ 15, 3   } },
  }

  _ = sample_input ;

  SensorBeaconPairs = real_input 
  // SensorBeaconPairs = sample_input

  // Loop over all the sensor-beacon pairs:

  // Find max distance, min x, max x, min y, max y:
  for _,pp := range SensorBeaconPairs {
    cov := coverage_radius(pp) 
    MaxRadius = max(MaxRadius, cov)

    MinX = min(MinX, min(pp.sensor.x, pp.beacon.x))
    MinY = min(MinY, min(pp.sensor.y, pp.beacon.y))

    MaxX = max(MaxX, max(pp.sensor.x, pp.sensor.x))
    MaxY = max(MaxY, max(pp.sensor.y, pp.sensor.y))

    fmt.Printf("Point: %v, coverage_radius: %v\n", pp, cov) 
  }


  fmt.Printf("Max Radius: %v\n", MaxRadius) 

  fmt.Printf("MinX: %v\nMinY: %v\nMaxX: %v\nMaxY: %v\n", MinX, MinY, MaxX, MaxY)

  /*
  // Print out the entire space:

  fmt.Println("Printing all: ") 
  for y := MinY; y <= MaxY; y++ {
    print64_row(y, MinX, MaxX) 
  }
  */

  detectables := scan_row(2000000)

  fmt.Printf("Detectable in row 2000000: %v\n", detectables) 

  // Beacon has a tuning frequency f:
  
  //   f = x*m + y
  
  // where:
  var m int64 = 4000000

  // limit:
  // var limit int64 = 20  // use this for the sample input
  var limit int64 = 4000000
  

  // Scan only these pairs:

  // Too slow:
  Outer:
  for y := int64(0); y <= limit; y++ {
    for x := int64(0); x <= limit; x += skip_x(x,y) {
      p := Point{ x , y}
      if !would_be_detected(p) {
        fmt.Printf("Undetectable: %v\n", p) 
        fmt.Printf("Tuning frequency: %v\n", x*m + y ) ;
        break Outer 
      }
    }
  }

}
