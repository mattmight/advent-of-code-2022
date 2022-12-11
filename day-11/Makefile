

monkeytoss: monkeytoss.cpp
	g++ -W -std=c++11 -I/opt/homebrew/include -L/opt/homebrew/lib -lgmpxx -lgmp -o monkeytoss monkeytoss.cpp

part1: monkeytoss
	./monkeytoss < input.txt

sample1: monkeytoss
	./monkeytoss < sample-input.txt

part2: monkeytoss
	./monkeytoss --part2 < input.txt

sample2: monkeytoss
	./monkeytoss --part2 < sample-input.txt


clean:
	rm -fv monkeytoss
