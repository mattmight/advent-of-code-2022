
% arg_list{1} will be either "part1" or "part2"
% arg_list{2} will be the name of the file
arg_list = argv ();
filename = arg_list{2} ;


% A matrix into which to store the input:
stacks = [] ;


% Start reading the input file:
fid = fopen (filename) ;
line = fgetl(fid) ;


% Read in the initial configuration of the stacks as a matrix of characters:
while strcmp(line,"") == 0 

  % Grab every 4th char starting with the 2nd
  % This skips over '[', ']' and white space in the
  % configuration.
  boxes = line(2:4:end) ;

  % Add these boxes to our list of stacks:
  stacks = vertcat([boxes],stacks) ;

  % Get the next line:
  line = fgetl(fid) ;

endwhile 


% The transpose makes it so that the rows are now the initial stacks:
stacks = stacks' ;


% We don't really want a true, rigid n x m matrix anymore, 
% so we'll put these stacks into an array in which the rows 
% can have varying length:
block_stacks = {} ;

[height,width] = size(stacks) ;

for i = 1:height
  stack_row = stacks(i,:) ;
  stack_row = strtrim(stack_row) ;
  block_stacks{i} = stack_row ;
end


% A push operation that adds to the top of a stack:
function rv = push(v,el) 
  rv = horzcat(v,[el]) ;
endfunction


% A pop operation that pulls off the top of a stack:
function [rv,el] = pop(v)
  el = v(end) ;
  rv = v(1:end-1) ;
endfunction

% Loop over the move commands:
while true

  % Read the line:
  moveline = fgetl(fid) ;

  % Check for end of file:
  if ~ischar(moveline); break; end

  % Quick and dirty parsing of 
  % move <amount> from <source> to <target>
  s = moveline ;
  [move,s]   = strtok(s, " ") ;
  [amount,s] = strtok(s, " ") ;
  [from,s]   = strtok(s, " ") ;
  [source,s] = strtok(s, " ") ;
  [to,s]     = strtok(s, " ") ;
  [target,s] = strtok(s, " ") ;

  amount = str2num(amount) ;
  source = str2num(source) ;
  target = str2num(target) ;

  count = amount ;

  % Move the blocks:
  while count > 0
    count = count - 1 ;

    [rest, top] = pop(block_stacks{source}) ;
    block_stacks{source} = rest ;
    block_stacks{target} = push(block_stacks{target}, top) ;
  end

  % If it's part 2, pull off the moved blocks and flip them,
  % to mimic the effect of moving them all at once:
  if strcmp(arg_list{1}, "part2")
    top = block_stacks{target}(end-amount+1:end) 
    block_stacks{target}(end-amount+1:end) = flip(top)
  end

end  

% Print out the state of the stacks at the end:
disp(block_stacks)

% Close the file:
fclose(fid)

