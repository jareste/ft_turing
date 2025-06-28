#!/bin/bash

./test_one.sh "Non-existent file" config/json.json "1232131o0" "No such file or directory"

./test_one.sh "trying left" config/unary_sub.json "=" "Cannot move left from the beginning of the tape"

./test_one.sh "trying right" config/unary_sub.json "111" "Cannot move right from the end of the tape"

./test_one.sh "No transition found" config/unary_sub.json "111-11-1111-1-1111=" "No transition for (skip, -)"

./test_one.sh "Unary_sub" config/unary_sub.json "111-11=" "[1..<->.............] (eraseone, -) -> (HALT, ., LEFT)"
./test_one.sh "Unary_sub" config/unary_sub.json "111-111=" "[...<->..............] (eraseone, -) -> (HALT, ., LEFT)"


./test_one.sh "Unary_add 111+111=" config/unary_add2.json "111+111=" "[111111."
./test_one.sh "Unary_add 1+1=" config/unary_add2.json "1+1=" "[11."
./test_one.sh "Unary_add 111=" config/unary_add2.json "111=" "[111<=>"


./test_one.sh "Palindrome 1" config/palindrome.json "1" "[X<y>"
./test_one.sh "Palindrome 11" config/palindrome.json "11" "[XX<y>"
./test_one.sh "Palindrome 101101" config/palindrome.json "101101" "[XXXXXX<y>"
./test_one.sh "Palindrome 101" config/palindrome.json "101" "[XXX<y>"
./test_one.sh "Palindrome 110111" config/palindrome.json "110111" "[XXX1XX<n>"
./test_one.sh "Palindrome 10" config/palindrome.json "10" "[XX<n>"
./test_one.sh "Palindrome 111000" config/palindrome.json "111000" "[X1100X<n>"
./test_one.sh "Palindrome 001011100" config/palindrome.json "001011100" "[XXXX11XXX<n>"


./test_one.sh "0n1n 0011" config/0n1n.json "0011" "[AABBy"
./test_one.sh "0n1n 01" config/0n1n.json "01" "[ABy"
./test_one.sh "0n1n 00011" config/0n1n.json "00011" "[AAABBn"
./test_one.sh "0n1n 00111" config/0n1n.json "00111" "[AABB1n"
./test_one.sh "0n1n 00" config/0n1n.json "00" "[A0n"
./test_one.sh "0n1n 1" config/0n1n.json "1" "[1n"
./test_one.sh "0n1n 00101" config/0n1n.json "00101" "[AAB0Bn"
./test_one.sh "0n1n 001011" config/0n1n.json "001011" "[AAB0B1n"
./test_one.sh "0n1n 0010110" config/0n1n.json "0010110" "[AAB0B10n"
./test_one.sh "0n1n 0001110" config/0n1n.json "0001110" "[AAABBB0n"
./test_one.sh "0n1n 001101" config/0n1n.json "001101" "[AABB01n"


./test_one.sh "02n 0" config/02n.json "0" "[Xn"
./test_one.sh "02n 00" config/02n.json "00" "[XXy"
./test_one.sh "02n 000" config/02n.json "000" "[XXXn"
./test_one.sh "02n 0000" config/02n.json "0000" "[XXXXy"
./test_one.sh "02n 00000" config/02n.json "00000" "[XXXXXn"
