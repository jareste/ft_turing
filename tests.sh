#!/bin/bash

./test_one.sh "Non-existent file" config/json.json "1232131o0" "No such file or directory"

./test_one.sh "trying left" config/unary_sub.json "=" "Cannot move left from the beginning of the tape"

./test_one.sh "trying right" config/unary_sub.json "111" "Cannot move right from the end of the tape"

./test_one.sh "No transition found" config/unary_sub.json "111-11-1111-1-1111=" "No transition for (skip, -)"

./test_one.sh "Unary_sub" config/unary_sub.json "111-11=" "[1..<->.............] (eraseone, -) -> (HALT, ., LEFT)"
./test_one.sh "Unary_sub" config/unary_sub.json "111-111=" "[...<->..............] (eraseone, -) -> (HALT, ., LEFT)"


./test_one.sh "Unary_add" config/unary_add.json "111+111=" "[...<->..............] (eraseone, -) -> (HALT, ., LEFT)"

