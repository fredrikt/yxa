#!/bin/bash

params=""

if [ "x$1" = "x--src" ]; then
    params="$params --src"
    shift
fi

if [ ! -d "$1" ]; then
    echo "usage: $0 [--src] dir"
    exit 1
fi

dir="$1"

includes=$(
    ls ${dir}/src ${dir}/src/*/ | while read entry; do
	if [ -d "${dir}/src/$entry" ]; then
	    echo "-I ${dir}/src/$entry"
	fi
    done
)

includes2=$(
    ls ${dir} ${dir}/*/ | while read entry; do
	if [ -d "${dir}/$entry" ]; then
	    echo "-I ${dir}/$entry"
	fi
    done
)

params="$params $includes $includes2 -DLOCAL_MODULE=local_default -r $dir"

dialyzer=$(which dialyzer)

if [ -x "$dialyzer" ]; then
    echo $dialyzer $params
    echo ""
    $dialyzer $params | grep -ve "^{local,.*calls missing or unexported.*local"
else
    echo "No dialyzer in \$PATH"
    exit 1
fi
