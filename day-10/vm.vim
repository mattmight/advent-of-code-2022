let g:cycles = 0
let g:x = 1

let g:xvals = [1]

" Executes the instruction on the current line
" It embeds the state of the machine as a comment after the line

function ExecuteCurrentCommand()

  " Grab the current command
  let command = expand('<cword>')

  if command == "addx"
 
    " Grab the numeric operand:
    call search ('-\|\d')
    let arg = expand('<cWORD>')

    " Bump the cycle count:
    let g:cycles = g:cycles + 1

    " Clear any comments on this line:
    execute 's/#.*//e'

    " Add a comment with the state during the first cycle:
    execute ':normal! A' . '   # cycle[' . g:cycles . '].x = ' . g:x

    " Record this state of x in the execution history list:
    call add ( g:xvals,  g:x )

    " Bump the cycle count again:
    let g:cycles = g:cycles + 1

    " Add a comment with the new state during the second cycle:
    execute ':normal! A' . '   # cycle[' . g:cycles . '].x = ' . g:x

    " Record this state in the execution history list:
    call add ( g:xvals,  g:x )
    
    " Bump the value of x:
    let g:x = g:x + arg


  elseif command == "noop"

    " Do nothing, but add a comment of the state of x during this cycle:
    execute ':normal! A' . '   # cycle[' . g:cycles . '].x = ' . g:x

    " Add this to the execution history list:
    call add ( g:xvals,  g:x )

    " Bump the cycle count:
    let g:cycles = g:cycles + 1

  else

    echom "Error: Unknown command: " . command
    
  endif

  " Jump to the next command:
  call search ('addx\|noop')

endfunction


" Runs the virtual machine on the instructions in this file:
function RunVM()

  " Clear all comments
  execute ':0,$s/[ ]*#.*//e'

  " Set the cursor to the first instruction
  call cursor(1,1)

  " Track the value of x, current cycle and all previous x values:
  let g:x = 1
  let g:cycles = 0
  let g:xvals = [1]

  " Run through the program, line by line:
  while line('.') != line('$')
    call ExecuteCurrentCommand()
  endwhile

  " Compute part 1:
  call writefile( [ SumSignalStrengths() ], "part1.txt", "")

  " Wipe the CRT:
  call writefile([ ], "crt.txt", "")
  
  " Render the CRT:
  call RenderCRT()

endfunction

" Renders the CRT to crt.txt
function RenderCRT()

  " Holds all the lines of the CRT:
  let l:out = [] 

  " Current position in the CRT memory:
  let l:crt = 0 

  " Current index in the cycle array:
  let l:i = 1 

  " Current column position on the current line:
  let l:screen_pos = 0

  " Currently rendering line:
  let l:line = ""
 
  " Loop over all locations on the CRT:
  while l:i < 241

    " Find the location on this line:
    let l:screen_pos = l:crt % 40

    " Find the value of x at this cycle:
    let l:x = g:xvals[ l:i ]

    " Check if we're starting a new line of the screen:
    if l:screen_pos == 0
      let l:line = ""
    endif

    " Check if this pixel is lit:
    let l:is_lit = ((l:x-1) <= l:screen_pos) && (l:screen_pos <= (l:x+1))

    " Render this pixel:
    if l:is_lit
      let l:line = l:line . '#'
    else
      let l:line = l:line . ' '
    endif

    " End of this line:
    if l:screen_pos == 39
      call add ( l:out, l:line )
    endif
   
    " Bump the cycle index and the CRT memory location:
    let l:i = l:i + 1
    let l:crt = l:crt + 1

    
  endwhile

  " Write out the contents of the CRT:
  call writefile( l:out, "crt.txt" , "" )
  
endfunction



" Compute the score for part 1:
function SumSignalStrengths()

  let score = 20*g:xvals[20] + 60*g:xvals[60] + 100*g:xvals[100] + 140*g:xvals[140] + 180*g:xvals[180] + 220*g:xvals[220]

  echom "part 1: " . score

  return score
  
endfunction


call RunVM()

