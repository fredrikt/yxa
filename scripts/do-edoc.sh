#!/bin/sh

file="$1"

if [ ! -f $file ]; then
    echo "Syntax: $0 file"
    exit 1
fi

if [ -f "new_${file}" ]; then
    mv -f "new_${file}" "old_${file}.$$"
fi

if [ -x ../../scripts/format-for-edoc.pl ]; then
    ../../scripts/format-for-edoc.pl "$file" || exit 1
else
    ../scripts/format-for-edoc.pl "$file" || exit 1
fi

#mod=$(echo $file | cut -d . -f 1)
#perl -p -i -e "s/^\-module.*\$/-module\(new_${mod}\)./" "new_${file}"


if [ -f "old_${file}.$$" ]; then
    echo ""
    echo "Diff :"
    echo ""
    diff -u "old_${file}.$$" "new_${file}"
    rm "old_${file}.$$"
else
    echo ""
    echo "No previous file to diff with"
fi

echo ""
echo -n "press enter to edoc : "

read foo

htmlfile=$(echo $file | sed -e 's/erl$/html/')
test -f "$htmlfile" && rm "$htmlfile"
echo /pkg/erlang/R11B-2/bin/erl -noshell -run edoc files "new_${file}" -run init stop
/pkg/erlang/R11B-2/bin/erl -noshell -run edoc files "new_${file}" -run init stop

echo "finished"

