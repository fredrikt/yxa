#!/bin/bash

params="-Wunmatched_returns"
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
    test -d ../build/src/eldap/ && echo "-I ../build/src/eldap/"
    test -d ~/yxa/build/src/eldap/ && echo "-I `echo ~/yxa/build/src/eldap/`"
)

includes2=$(
    ls ${dir} ${dir}/*/ | while read entry; do
	if [ -d "${dir}/$entry" ]; then
	    echo "-I ${dir}/$entry"
	fi
    done | sort | uniq
)

params="$params $includes $includes2 -DLOCAL_MODULE=local_default -DYXA_NO_UNITTEST -r $dir"

dialyzer=$(which dialyzer)

if [ -x "$dialyzer" ]; then
    echo $dialyzer $params
    echo ""

    local_call_to_regexp="^local.*call.* missing or unexported.*local"

    out_dir="$TMPDIR/yxa-dialyzer-output" 
    if [ -d "$out_dir" ]; then
	out_base=$(echo "out_$PWD/$dir" | sed -e 's!\./!!g' -e 's!/*$!!g' | tr / _)
	out="$out_dir/${out_base}__`date +%Y-%m-%d_%Hh%Mm%Ss`"

	previous_out=$(ls -1tr $out_dir/${out_base}__* 2>/dev/null | tail -1)
	if [ "x$previous_out" != "x" ]; then
	    echo "Previous output found : $previous_out"
	else
	    echo "No previous output found with this base ($out_base)"
	fi

	echo "Saving output in file '$out'"

	trap "echo ""; echo Removing '$out'; rm -f '$out' '$out.TMP'; exit" INT TERM EXIT

	$dialyzer $params > "$out.TMP" 2>&1

	trap - INT TERM EXIT

	echo ""
	echo ""
	cat "$out.TMP"

	if [ -n "`grep Analysis.failed \"${out}.TMP\"`" ]; then
	    echo ""
	    echo ""
	    echo "Analysis failed, removing '$out'"
	    echo ""
	else
	    cat "$out.TMP" | grep -vie "$local_call_to_regexp" -e "^ done in " > "$out"
	fi
	rm "$out.TMP"
    else
	$dialyzer $params | grep -vie "$local_call_to_regexp"
    fi
else
    echo "No dialyzer in \$PATH"
    exit 1
fi

if [ -f "$previous_out" -a -f "$out" ]; then
    echo ""
    echo ""
    echo "Diff to last output :"
    diff -wu "$previous_out" "$out"
    echo ""
    echo ""
fi
