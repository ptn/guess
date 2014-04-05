# guess

An AI bot that reads examples of "valid" and "invalid" series of 3 numbers and 
tries to guess the rule that makes them valid or invalid, i.e it produces the 
boolean rule that classifies sample 3-tuples correctly - it guesses the 
pattern.

## Usage

Clone the repo and invoke lein with one of the example input files:

lein run examples/all_equal.txt
lein run examples/ascending_order.txt
lein run examples/plus_one.txt

## Input file format

Just list examples of valid 3-tuples, one per line, insert a blank line, and 
list the examples of invalid 3-tuples.

## License

Copyright © 2014 Pablo Torres

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
