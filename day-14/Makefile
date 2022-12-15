
runner.beam: runner.erl
	erl -compile runner

part1: runner.beam
	erl -noshell -s runner main 1 -s init stop

part2: runner.beam
	erl -noshell -s runner main 2 -s init stop
       

clean:
	rm -fv runner.beam erl_crash.dump
