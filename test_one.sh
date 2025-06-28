#!/bin/bash

test_description="$1"
json_file="${2:-config/json.json}"
input_string="${3:-1232131o0}"
expected="${4:-your expected output here}"

output=$(./ft_turing "$json_file" "$input_string" 2>&1)

echo -e "\e[34m**********************************************"
echo -e "$test_description"
echo -e "**********************************************\e[0m"

if [[ "$output" == *"$expected"* ]]; then
  echo -e "\e[32m✔ Test passed!\e[0m"
else
  echo -e "\e[31m✘ Test failed!\e[0m"
  echo "Expected: $expected"
  echo "Got: $output"
  exit 1
fi