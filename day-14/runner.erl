
-module(runner).

-import(string, [concat/2]).

-import(lists, [map/2, sort/1]).

-import(gb_trees, [tree/0, empty/0, to_list/1, insert/3, lookup/2, keys/1]).

-export([main/1, string_join/1, table_keys/1, cave_lines/0, cave_set/4, cave_get/3, horizontal_get/2, max_depth/1]).


% Read the cave specification from input.txt:
% Ex: "498,4 -> 498,6 -> 496,6"
%      =>
%      [ {498,4} , {498,6} , {496,6} ]
cave_lines() ->
  InputLines = read_lines("input.txt"),
  lists:map(fun parse_line/1, InputLines).


% Parse a line from the specification into a line segment:
parse_line(Line) -> 
  Tokens = lists:map(fun list_to_integer/1, string:tokens(Line, " ->,")),
  parse_tokens(Tokens).

parse_tokens([]) -> [];
parse_tokens([ X1,Y1 | Tail ]) ->
  [ {X1,Y1} | parse_tokens(Tail) ].


%% Representing the Cave

% The cave will be represented as a sparse matrix inside the ETS.

% Individual columns will be represented as gb_trees (balanced binary search trees).

% Columns themselves will be stored in the ETS, keyed by the X coordinate.


% vertical_insert : GBTree[Depth,Block] * Depth * Block -> GBTree[Depth,Block]
% Adds a block to a specific column at the given depth
vertical_insert(ColumnMap, Y, Block) ->
  gb_trees:enter(Y, Block, ColumnMap).
 

% vertical_get : GBTree[Depth,Block] * Depth -> Block
% Pulls out the block at a given depth in a column.
vertical_get(Tree, Y) ->
  case gb_trees:lookup(Y,Tree) of
    none        -> " ";
    {value, V}  -> V
  end.


% horizontal_get : DB * XCoord -> Tree[Depth,Block]
horizontal_get(DB, X) ->
  case ets:lookup(DB, X) of 
    []     -> gb_trees:empty();
    [{_,Tree}] -> Tree;
    _ -> erlang:error("unhandled case in horizontal get")
  end.


% cave_set: DB * XCoord * YCoord * Block -> ()
% Sets a block in the sparse matrix.
cave_set(DB, X,Y, Block) ->
  Column = horizontal_get(DB, X),
  NewColumn = vertical_insert(Column, Y, Block),
  ets:insert(DB, { X, NewColumn }).

cave_get(DB, X,Y) ->
  Column = horizontal_get(DB, X),
  vertical_get(Column, Y).


% cave_draw: Draws a line segment:
cave_draw(DB,  X1,Y1,  X2,Y2) when (X1 == X2) and (Y1 == Y2) ->
  cave_set(DB, X1,Y1, "#");
cave_draw(DB,  X1,Y1,  X2,Y2) when (X1 == X2) ->
  cave_set(DB, X1,Y1, "#"),
  if 
  Y1 < Y2 ->
     cave_draw(DB, X1,Y1+1, X2,Y2);
  true ->
     cave_draw(DB, X1,Y1-1, X2,Y2)
  end;
cave_draw(DB,  X1,Y1,  X2,Y2) when (Y1 == Y2) ->
  cave_set(DB, X1,Y1, "#"),
  if 
  X1 < X2 ->
     cave_draw(DB, X1+1,Y1, X2,Y2);
  true ->
     cave_draw(DB, X1-1,Y1, X2,Y2)
  end.
  

% cave_draw: Draws a line, segment by segment:
cave_draw(_DB, []) -> ok;
cave_draw(DB, [{X,Y}]) -> cave_draw(DB, X,Y,  X,Y);
cave_draw(DB, [{X1,Y1}, {X2,Y2} | Tail]) ->
  cave_draw(DB, X1,Y1,  X2,Y2),
  cave_draw(DB, [ {X2,Y2} | Tail ]).
  


% table_keys: Get all the keys in an ETS table:
table_keys(TableName) ->
    FirstKey = ets:first(TableName),
        table_keys(TableName, FirstKey, [FirstKey]).

table_keys(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
    lists:sort(Acc);
table_keys(TableName, CurrentKey, Acc) ->
    NextKey = ets:next(TableName, CurrentKey),
    table_keys(TableName, NextKey, [NextKey|Acc]).


% first: Get the head of a list:
first([Head | _]) -> Head.


% Reads in the given FileName and returns all lines as a list of strings:
read_lines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  lists:map(fun binary_to_list/1, binary:split(Data, [<<"\n">>], [global])).

string_join([]) -> "";
string_join([Head|Tail]) -> concat(Head,string_join(Tail)).


% Find the min and max key in an ETS table:
min_key(DB) -> first(table_keys(DB)).
max_key(DB) -> lists:last(table_keys(DB)).


% Render a row in the cave:
cave_render_row(DB, Y, MinX, Offset, MaxX) ->
  X = MinX + Offset,
  if 
     X > MaxX -> io:fwrite("\n");
     true ->
       io:fwrite(cave_get(DB, X, Y)),
       cave_render_row(DB, Y, MinX, Offset+1, MaxX)
  end.

% Rende an entire cave:
cave_render(DB, MinX, MaxX, MinY, MaxY) ->
  if
    MinY > MaxY -> io:fwrite("\n");
    true -> cave_render_row(DB, MinY, MinX, 0, MaxX),
            cave_render(DB, MinX, MaxX, MinY+1, MaxY)
  end.

cave_render(DB) ->
  MinX = min_key(DB),
  MaxX = max_key(DB),
  MinY = 0,
  MaxY = max_depth(DB),
  io:format("min X: ~p~n", [MinX]),
  cave_render(DB, MinX, MaxX, MinY, MaxY).


% Checks if a given coordinate is "hard" in the cave:
is_hard(DB, X, Y, MaxY) ->
  [ { _, HasFloor } ] = ets:lookup(globals, has_floor),
  if HasFloor and (Y >= MaxY + 2) -> true;
     true -> case cave_get(DB, X, Y) of
               " " -> false;
               "O" -> true;
               "#" -> true;
               _  -> erlang:error("unknown block type")
             end
  end.


% Simulate sand dripping down;
%  return false if no more changes are possible;
%  return true if a change was made
simulate_drop(_DB, _X, Y, MaxY) when Y > (MaxY + 8) -> false; % fell into the abyss
simulate_drop(DB, X, Y, MaxY) ->
  Blocks = {is_hard(DB, X-1, Y+1, MaxY),
            is_hard(DB, X  , Y+1, MaxY),
            is_hard(DB, X+1, Y+1, MaxY)},

  case Blocks of
     {true,true,true} ->
       cave_set(DB, X,Y, "O"),
       {X,Y} /= {500,0}; % nothing changed if we filled the drip point

     {_,false,_} ->
       simulate_drop(DB, X, Y+1, MaxY);

     {false,_,_} ->
       simulate_drop(DB, X-1, Y+1, MaxY);

     {_,_,false} ->
       simulate_drop(DB, X+1, Y+1, MaxY) 
  end.


% Simulates N drops of sand falling into the cave.
simulate_n_drops(DB, 1, X,Y, MaxY) -> simulate_drop(DB, X,Y, MaxY);
simulate_n_drops(DB, N, X,Y, MaxY) ->
 simulate_drop(DB, X,Y, MaxY),
 simulate_n_drops(DB, N-1, X,Y, MaxY).

% Simulates drops until no further changes are possible, counting the total number of rounds:
simulate_until_stable(DB, N, X,Y, MaxY) ->
  Changed = simulate_drop(DB, X, Y, MaxY),
  if Changed -> simulate_until_stable(DB, N+1, X,Y, MaxY);
     true -> N
  end.


% Gets the maximum depth of any block in Column X.
max_depth_at(DB, X) ->
  Column = horizontal_get(DB, X),
  Depths = gb_trees:keys(Column),
  case Depths of
    [] -> abyss;
    _ -> lists:max(Depths)
  end.

% Get the max recorded depth anywhere in the Cave.
max_depth(DB) -> 
  Keys = table_keys(DB),
  MaxDepths = lists:map(fun (Key) -> max_depth_at(DB,Key) end, Keys),
  lists:max(MaxDepths).


% Run the simulation for Part 1 or Part 2 of the problem:
main(Part) -> 

  ets:new(globals, [set,named_table]),

  io:format("part: ~p~n",[Part]),
  io:fwrite("\n"),
 
  % Turn on the floor if Part 2:
  case Part of 
    ['1'] -> ets:insert(globals, { has_floor, false });
    ['2'] -> ets:insert(globals, { has_floor, true  });
    _ -> erlang:error("unknown parameter")
  end,

  % Create a database to hold the sparse cave matrix:
  Cave = ets:new(cave_db, [set,named_table]),

  % Read in all the input lines:
  Lines = cave_lines(),

  % Draw all the line segments in the cave specification:
  lists:foreach (fun (Line) -> cave_draw(Cave, Line) end, Lines),

  % Mark the drip point:
  cave_set( Cave, 500,0 , "+" ),

  % Get the maximum depth of the Cave:
  AbyssDepth = max_depth(Cave),


  % Render the initial state of the Cave:
  io:fwrite("Cave rendering: \n"),
  cave_render( Cave ),

  % Simulate until no changes happen:
  Count = simulate_until_stable( Cave, 0, 500, 0, AbyssDepth ),

  % Use this if you want to see what it looks like after a certain number of drips:
  % Count = 100,
  % simulate_n_drops( Cave, Count, 500, 0, AbyssDepth),


  % Render the final cave configuration:
  cave_render( Cave ),

  % Output the total number of drops:
  Extra = case Part of 
            ['1'] -> 0;
            ['2'] -> 1
          end,

  % Print out the total number of drips:
  io:format("Stable after ~p drops~n", [ Count + Extra ]).

