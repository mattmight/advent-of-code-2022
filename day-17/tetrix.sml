
(* Cut off everything from the newline forward: *)
fun right_trim_charlist (#"<"::tl) = #"<"::(right_trim_charlist tl)
  | right_trim_charlist (#">"::tl) = #">"::(right_trim_charlist tl)
  | right_trim_charlist (#"\n"::tl) = []
  | right_trim_charlist [] = []

fun right_trim_string (str) = (implode (right_trim_charlist (explode str)))

(* Read the jets cycle from input.txt: *)
val input = case TextIO.inputLine (TextIO.openIn "input.txt") of SOME(s) => right_trim_string(s)

(* Shape names and ordering: *)
datatype shape_name = FLATBAR | PLUS | WEDGE | TALLBAR | SQUARE ;

fun shape_succ FLATBAR = PLUS
  | shape_succ PLUS = WEDGE
  | shape_succ WEDGE = TALLBAR
  | shape_succ TALLBAR = SQUARE
  | shape_succ SQUARE = FLATBAR

fun shape_to_int FLATBAR = 0
  | shape_to_int PLUS = 1
  | shape_to_int WEDGE = 2
  | shape_to_int TALLBAR = 3
  | shape_to_int SQUARE = 4

(* An ordering for when used as a key for ORD_MAP: *)
fun compare_shape (shape1, shape2) = Int.compare (shape_to_int shape1, shape_to_int shape2)

(* Shape widths: *)
fun shape_width FLATBAR = 4
  | shape_width PLUS = 3
  | shape_width WEDGE = 3
  | shape_width TALLBAR = 1
  | shape_width SQUARE = 2

(* NOTE: These are specified upside down. *)
fun shape FLATBAR : (char list) list = 
     [ [ #"#" , #"#", #"#", #"#" ] ]

  | shape TALLBAR = 
     [ [ #"#" ] ,
       [ #"#" ] ,
       [ #"#" ] ,
       [ #"#" ] ]

  | shape PLUS = 
     [ [ #" " , #"#" , #" " ] ,
       [ #"#" , #"#" , #"#" ] ,
       [ #" " , #"#" , #" " ] ]

  | shape WEDGE = 
     [ [ #"#" , #"#" , #"#" ] ,
       [ #" " , #" " , #"#" ] ,
       [ #" " , #" " , #"#" ] ]

  | shape SQUARE = 
     [ [ #"#" , #"#" ] ,
       [ #"#" , #"#" ] ]
    ;

(* X and Y positions: *)
type xpos = int
type ypos = int


(* Helper function -- min: *)
fun min(a,b) = if a < b then a else b


(* A binary search tree map with Int as key: *)
structure IntMap = RedBlackMapFn(struct 
  type ord_key=int
  (* NOTE: We're reversing the order, so that firsti can tell us the max key: *)
  fun compare (a,b) = if a > b then LESS else if a = b then EQUAL else GREATER
end)


(* A structure to hold all the elements necessary to simulate this world,
   as parameterized by a specific sequence of jet inputs: *)
functor WorldFn(val jets_string : string) =
struct

  (* Thrown when freezing a world causes blocks to overlap: *)
  exception Collision


  (* Rows are a list of characters, 7 wide: *)
  type row = char list

  (* The grid is a map from row number to the row contents: *)
  type world_grid = row IntMap.map

  (* Within a state, an object state is a currently falling object,
     or it's an object to start falling next: *)
  datatype object_state = Curr of xpos * ypos * shape_name
                        | Next of shape_name

  (* The state of the jets is jet sequence remaining in this cycle: *)
  type jet_state = char list

  (* The total size of the jet cycle: *)
  val jet_cycle_size = String.size jets_string 

  (* The state of the world is a grid, the current/next object and the state of the jets: *)
  type world = {rows : world_grid,
                obj : object_state,
                jets : jet_state}


  (* The initial world: *)
  val world0 : world = 
      { rows = IntMap.empty , obj = Next(FLATBAR) , jets = explode jets_string }

  (* Reinitializes this world with an empty grid: *)
  fun reinit ({rows,obj,jets} : world) : world =
      { rows = IntMap.empty , obj = obj, jets = jets }

  (* Get a specific row: *)
  fun get_row ({rows,...} : world) (y : ypos) : row =
    case IntMap.find (rows, y) of 
       SOME(row) => row
     | NONE => [ #" ", #" ", #" ", #" ", #" ", #" ", #" " ]

  (* Set a specific row: *)
  fun set_row ({rows,obj,jets}: world) (y : ypos) (row : row) : world =
     {rows=IntMap.insert(rows, y, row), obj=obj, jets=jets}

  (* Find the height of the top of the stack in blocks (not rocks): *)
  fun max_height ({rows,...} : world) : int =
    case IntMap.firsti rows of
       SOME(max,_) => max + 1
     | NONE => 0

  (* Print out a specific row:*)
  fun print_row (world : world) (i : int) =
    let
      val row = get_row world i
    in
      print("|");
      print(implode(row));
      print("|");
      print("\n")
    end

  (* Start dropping a new object: *)
  fun intro_object (world : world) (shape_name : shape_name) : world =
    { rows = #rows world,
      obj = Curr(2,max_height(world)+3,shape_name),
      jets = #jets world } 

  (* Print the given rows: *)   
  fun print_rows (world : world, bottom : int, top : int) = 
    if top < bottom
    then
      ()
    else
      (print_row world top; print_rows (world, bottom, top-1)) ;

  (* Print the entire world: *)
  fun print_world world =
    let
      val height = max_height (world)
    in
      print_rows (world, 0, height + 7);
      print (" height: ");
      print (Int.toString height);
      print ("\n")
    end


  (* Merge two character lines together: *)
  fun merge_lines ([],[]) = []
    | merge_lines ([],l2) = l2
    | merge_lines (l1,[]) = l1
    | merge_lines (#" "::r1, #" "::r2) = #" "::(merge_lines(r1,r2))
    | merge_lines (#" "::r1, #"#"::r2) = #"#"::(merge_lines(r1,r2))
    | merge_lines (#"#"::r1, #" "::r2) = #"#"::(merge_lines(r1,r2))
    | merge_lines (#"#"::_,  #"#"::_)  = raise Collision
 
  fun pad (0,line) = line
    | pad (n,line) = #" " :: pad(n-1,line)

  fun paste_line (world : world) (x : xpos, y: ypos) (line : char list) : world =
    let
      val row = get_row world y
      val new_row = merge_lines( row , pad(x,line) )
    in
      set_row world y new_row
    end

  (* Paste an object into the world, locking it in place: *)
  fun paste_object (world : world) (x : xpos, y : ypos) ([]) = world
    | paste_object world (x,y) (ln::rest) = 
        paste_line (paste_object world (x,y+1) rest) (x,y) ln

  (* Freeze the current object into its current place, and prepare to drop the next: *)
  fun freeze ({rows,obj=Curr(x,y,shape_name),jets} : world) : world =
        paste_object {rows=rows, obj=Next(shape_succ shape_name), jets=jets} (x,y) (shape shape_name)
    | freeze world = world

  (* Force the object down a row: *)
  fun force_down ({rows,obj=Curr(x,y,shape),jets} : world) : world =
    {rows=rows, obj=Curr(x,y-1,shape),jets=jets}

  (* Try moving to the left, if possible: *)
  fun try_move_left {rows,obj=Curr(0,y,shape),jets} = {rows=rows,obj=Curr(0,y,shape),jets=jets}
    | try_move_left {rows,obj=Curr(x,y,shape),jets} = {rows=rows,obj=Curr(x-1,y,shape),jets=jets}

  (* Try moving to the irght, if possible: *)
  fun try_move_right {rows,obj=Curr(x,y,shape),jets} = 
        {rows=rows,obj=Curr(min(7-(shape_width shape),x+1),y,shape),jets=jets}

  (* Check if this world has suffered a collision/overlap between two rocks: *)
  fun is_collided world =
    (freeze world; false) handle Collision => true

  (* Process a jet: *)
  fun process_jet (world : world) : world =
   ( (* print(" processing jets: ") ;
        print(implode (#jets world));
      print("\n") ; *)
    case world of 
       {rows, obj, jets = #"<" :: rest} => 
         let 
           val try_left = try_move_left {rows=rows, obj=obj, jets=rest}
         in
           if is_collided try_left
           then {rows=rows, obj=obj, jets=rest}
           else try_left
         end
     | {rows, obj, jets = #">" :: rest} => 
         let
           val try_right = try_move_right {rows=rows, obj=obj, jets=rest}
         in
           if is_collided try_right
           then {rows=rows, obj=obj, jets=rest}
           else try_right
         end
   )

  (* Checks if the current object can drop without colliding: *) 
  fun can_drop {obj=Curr(_,0,_),...} : bool = false
    | can_drop world =
      (freeze ( force_down world ); true) handle Collision => false

  (* Drop or freeze (if no drop is possible):*)
  fun process_drop world = 
    if can_drop world
    then
      force_down world
    else
      freeze world

  (* Print the world as frozen: *)
  fun print_frozen world = print_world (freeze world)

  (* Move the world forward by one tick: *)
  fun next (world : world) : world =
    case (world : world) of 
        {rows, obj=Next(shape), jets} => {rows=rows,
                                          obj=Curr (2, 3 + max_height world,shape), 
                                          jets=jets}

     |  {rows, obj, jets=[]} => {rows=rows, obj=obj, jets=explode jets_string}

     |  {rows, obj, jets} =>
        let
           val moved_world = process_jet world 
           val dropped_world = process_drop moved_world
        in 
           dropped_world
        end

  (* Move the world n ticks forward: *)
  fun next_n 0 world = world
    | next_n n world = 
       (print "step ";
        print (Int.toString n);
        print "\n";
        print_frozen world;
        next_n (n-1) (next world))

  (* Take steps until the next rock drops: *)
  fun next_rock' (world as {rows,obj=Next shape,jets} : world) : world = world
    | next_rock' (world as {rows,obj=Curr (_,_,_),jets}) = next_rock' (next world)
 
  (* Jump to the next rock drop: *) 
  fun next_rock (world : world) : world = next_rock' (next world)

  (* Jump to the nth rock drop: *)
  fun next_rock_n 0 world = world
    | next_rock_n n world = next_rock_n (n-1) (next_rock world)


  (*** Cycle finding machinery ***) 


  (* 

  We'll look for cycles in state that are "flat" -- states that 
  have ####### as their top -- since it's like having a fresh start. 

  Key insight: If two states are both flat, and they both have the same next
  object and they're at the same place in the jet cycle, then they'll both have
  exactly the same evolution.

  *)

  (* NOTE: This doesn't work for the sample input, because it never hits a flat state,
     but it works for the challenge input. *)


  (* Check if a world has a flat top: *)
  fun is_flat (world : world) : bool = 
      ((max_height world = 0) orelse
       ((get_row world ((max_height world) - 1)) =
       [#"#", #"#", #"#", #"#", #"#", #"#", #"#"]))


  (* To find a cyle, we need to keep a log of all flat states 
     until the same flat state is repeated. *)

  type flat_state = shape_name * int 

  structure FlatStateMap = RedBlackMapFn(struct

    type ord_key=flat_state

    fun compare ( (s1,j1) , (s2,j2) ) = 
      case compare_shape (s1,s2) of
          LESS => LESS
        | GREATER => GREATER
        | EQUAL => Int.compare (j1, j2)
  end);

  type fs_map = (int * int) FlatStateMap.map

  (* Run until we find a cycle between flat states: *)
  fun find_flat_cycle (rocks : int) (fs_map : fs_map) (world as {rows,obj=Next(shape),jets} : world)
    : (int * int * int * int * world) =
    if not (is_flat world)
    then find_flat_cycle (rocks+1) fs_map (next_rock world)
    else 
      case FlatStateMap.find (fs_map, (shape,length jets)) of
          SOME(old_rocks, old_height) =>
            (old_rocks,           (* start of the cycle *)
             old_height, 
             rocks,               (* end of the cycle *)
             max_height world, 
             world)
        | NONE => 
          let
            val fs_map' = FlatStateMap.insert (
                            fs_map, 
                            (shape, length jets), 
                            (rocks, max_height world)
                          )
           in find_flat_cycle (rocks+1) fs_map' (next_rock world)
          end

end;


(* Instantiate the world with the input jet cycle sequence:  *)
structure World = WorldFn(val jets_string = input) ;


(* Part 1: *)
val world = World.world0 ;

val world = World.next_rock_n 2022 world ;

print ("Part 1, height: " ^ (Int.toString(World.max_height world)) ^ "\n") ;

(* World.print_frozen world ; *)


(* Find the beginning and end of a cycle: *)
val (start_rocks, start_height, end_rocks, end_height, world_c) = 
   World.find_flat_cycle 0 World.FlatStateMap.empty World.world0 ;

(* Calculate the length of a cycle in both rocks and blocks: *)
val cycle_length_in_rocks = end_rocks - start_rocks ;
val cycle_length_in_blocks = end_height - start_height ;

(* Jump to the start of the first cycle: *)
val target_rocks = 1000000000000 - start_rocks ; (* rocks until the first cycle hits *)

(* Figure out how many full cycles fit in the remainder: *)
val full_cycles = target_rocks div cycle_length_in_rocks ;

(* Figure out how many rocks are still left after the final full cycle: *)
val rocks_left = target_rocks mod cycle_length_in_rocks ;

(* Start the simulation again from this point: *)
val world0' = World.reinit world_c ;
val world_left = World.next_rock_n rocks_left world0' ;

(* Figure out how much more height (in blocks) was added: *)
val height_left = World.max_height world_left ;

(* Add it all together: *)
val total_height = start_height + (full_cycles * cycle_length_in_blocks) + height_left ;

print ("Part 2, height: " ^ (Int.toString total_height) ^ "\n") ;

OS.Process.exit(OS.Process.success);

