namespace signal;

using System.IO;

class Program
{
    // find the locaton in the string after the first span of the provided
    // length in which all characters are distinct:
    static int findDistinctSpan(string line, int span, int location = 0) {

        // Check if the string is empty:
        if (line == "")
            return -1;

        // Check if there is enough string left:
        if (line.Length < span)
            return -1;

        // Grab the next span charaters:
        string substring = line.Substring(0, span);

        // Count the number of distinct characters in the span:
        int distincts = substring.Distinct().Count();

        // Check if every character was unique within the span:
        if (distincts < span)

            // Keep looking:
            return findDistinctSpan(line.Substring(1), span, location + 1);

        else
            // Span found:
            return location + span;
    }

    static void Main(string[] args)
    {
        Console.WriteLine("Hello, World!");

        string path = Directory.GetCurrentDirectory();

        Console.WriteLine("part 1 test cases: ");
        Console.WriteLine(findDistinctSpan("bvwbjplbgvbhsrlpgdmjqwftvncz", 4));
        Console.WriteLine(findDistinctSpan("nppdvjthqldpwncqszvftbrmjlhg", 4));
        Console.WriteLine(findDistinctSpan("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4));
        Console.WriteLine(findDistinctSpan("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4));


        Console.WriteLine("part 2 test cases: ");
        Console.WriteLine(findDistinctSpan("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14));
        Console.WriteLine(findDistinctSpan("bvwbjplbgvbhsrlpgdmjqwftvncz", 14));
        Console.WriteLine(findDistinctSpan("nppdvjthqldpwncqszvftbrmjlhg", 14));
        Console.WriteLine(findDistinctSpan("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14));
        Console.WriteLine(findDistinctSpan("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14));


        foreach (string line in System.IO.File.ReadLines("input.txt")) {
            Console.WriteLine("part 1 answer: " + findDistinctSpan(line, 4));
            Console.WriteLine("part 2 answer: " + findDistinctSpan(line, 14)) ;
        }
    }
}

