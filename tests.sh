#!/bin/bash
set -e
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

for test_case_dir in ${script_dir}/test/*
do
    if [[ ! -d "${test_case_dir}" ]]
    then
        continue
    fi
    test_case=`basename ${test_case_dir}`
    echo -n "${test_case}... "
    out=$( "${script_dir}/test/test.sh" "${test_case}" && failed=0 || failed=1 )
    if [[ -n "${out}" ]]
    then
        echo "FAIL"
        echo "${out}"
        exit 1
    else
        echo "OK"
    fi
done
