#!/bin/bash
set -e
test_case="$1"
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

test_case_dir="${script_dir}/${test_case}"
if [[ ! -d "${test_case_dir}" ]]
then
    echo "Test case directory not found: ${test_case_dir}" >&2
    exit 1
fi

test_run_dir=`mktemp -d`
function cleanup {
    rm -rf -- "${test_run_dir}"
}
trap cleanup EXIT

mkdir "${test_run_dir}/watch"

ts_index_pid=$(
    cd "${script_dir}/.."
    node . "${test_run_dir}/watch" > "${test_run_dir}/actual.log" &
    echo "$!"
)

cp -r "${test_case_dir}/repo/." "${test_run_dir}/watch/."
sleep 1

if [[ -d "${test_case_dir}/step" ]]
then
    rsync --inplace --recursive --delete "${test_case_dir}/step/" "${test_run_dir}/watch/"
    sleep 1
fi

kill "${ts_index_pid}"

sleep 0.5

sed "s#${test_run_dir}#TEST_RUN_DIR#" "${test_run_dir}/actual.log" > "${test_case_dir}/actual.log"
diff "${test_case_dir}/expected.log" "${test_case_dir}/actual.log"
