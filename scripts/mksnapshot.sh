#!/bin/sh
#
# Syntax examples :
#
#   $ ./mksnapshot.sh yxa-2007-10-17-su1
#
#   $ ./mksnapshot.sh yxa-1.0rc2 svn+ssh://svn.it.su.se/svn/yxa/branches/1.0
#

exit_clean() {
    local res="$1"
    local msg="$2"

    if [ "x$msg" != "x" ]; then
	echo "$msg"
    fi

    exit $res
}

# parse arguments
version=$(echo $1 | sed -e 's/\/*$//')	# remove trailing backslash

# check if version indicates a snapshot or a release
if [ $(echo $version | grep ^yxa-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] 2>&1) ]; then
    version_kind="snapshot"
    make_release=0

    if [ "x$version" = "x" -o ! -d "$version" ]; then
	exit_clean 1 "Syntax: $0 dir [svn-url (for releases)]"
    fi
else
    if [ $(echo $version | grep "^yxa-[0-9]\.[0-9].*" 2>&1) ]; then
	version_kind="Release"
	make_release=1
	if [ "x$2" = "x" ]; then
	    exit_clean 1 "Missing second argument (svn-url) required for making releases"
	fi
	svn_url="$2"
	if [ -z $(echo $svn_url | grep "/branches/") ]; then
	    exit_clean 1 "Releases should be made from a SVN URL containing '/branches/'"
	fi
    else
	exit_clean 1 "Invalid version format : '$version' (should be yxa-YYYY-MM-DD[something] or yxa-[0-9].[0-9][something]"
    fi
fi

TARFILE="${version}.tar.gz"
if [ -f "$TARFILE" ]; then
    echo "$TARFILE already exists, let 'rm' remove it or press ctrl+c..."
    rm -i "$TARFILE"
    test -f "$TARFILE" && exit_clean 1 "Uhh, you told 'rm' to NOT remove the file '$TARFILE'"
fi

if [ $make_release -ne 0 ]; then
    if [ -d "$version" ]; then
	exit_clean 1 "Directory '$version' not supposed to exist already when making release"
    fi

    echo "'svn export'ing into directory $version"
    svn export "$svn_url" "$version"
fi

pushd "$version" >/dev/null 2>&1 || exit_clean 1 "Could not change directory to '$version'"

test -f src/sipserver.erl || exit_clean 1 "'$version' does not appear to contain YXA"

# fix version info in configure.ac
short_version=$(echo $version | sed -e 's/^yxa-//g')
test -d .svn && svn revert configure.ac
ac_version=$(grep ^AC_INIT configure.ac | sed -e 's/^AC_INIT.YXA, *\(.*\), *yxa-devel@lists\.su\.se.$/\1/g')

if [ "x$ac_version" = "x" ]; then
    echo "grep ^AC_INIT configure.ac :"
    grep ^AC_INIT configure.ac
    exit_clean 1 "Failed extracting current version number from configure.ac"
fi

if [ $make_release -ne 0 ]; then
    if [ "x$short_version" != "x$ac_version" ]; then
	echo "When you make a release, svn-url should point at a brach with the right version "
	echo "already in configure.ac ($ac_version is not right for release $short_version)"
	exit_clean 1 "release versioning error"
    fi
else
    if [ "x$short_version" = "x$ac_version" ]; then
	echo "configure.ac already says version '$short_version' - no update required"
	echo ""
    else
	echo "Putting snapshot version $short_version into configure.ac (was: $ac_version)"
	perl -p -i -e "s/^(AC_INIT.YXA,\s+).+?(\s*,\s*yxa-devel.+)\$/\${1}${short_version}\${2}/g" configure.ac
	grep ^AC_INIT configure.ac
	echo ""
    fi
fi

if [ -z "`grep ^\"AC_INIT.*YXA.*${short_version},\" configure.ac 2>&1`" ]; then
    exit_clean 1 "Failed updating version number in configure.ac"
fi

# figure out Erlang version
OTP=$(head -1 src/*.rel | grep '^%% Erlang OTP R' | head -1 | awk '{print $4}')
echo "Found indication to use Erlang OTP $OTP in one of the .rel files"
echo ""
if [ ! -x "/pkg/erlang/$OTP/bin/erlc" ]; then
    exit_clean 1 "/pkg/erlang/$OTP/bin/erlc not found!"
fi
PATH="/pkg/erlang/$OTP/bin:$PATH"
export PATH

if [ "x`grep ^REQUIRE_ERLANG.*\(Erlang/OTP\ $OTP\) configure.ac`" = "x" ]; then
    echo "src/configure.ac does NOT say that Erlang/OTP $OTP is needed :"
    echo ""
    grep ^REQUIRE_ERLANG configure.ac
    echo ""
    exit_clean 1 "Erlang dependency check is wrong!"
fi


readmever=$(grep "To install YXA, you need to have Erlang/OTP .* installed." README 2>&1)
if [ "x$readmever" != "xTo install YXA, you need to have Erlang/OTP $OTP installed." ]; then
    echo "WARNING: the README file does not say that Erlang/OTP $OTP is required!"
    echo "The readme contains '$readmever'"
    echo ""
    if [ $make_release -ne 0 ]; then
	echo ""
	exit_clean 1 "Refusing to make release under this condition"
    fi
    echo "Press enter to continue, ctrl+c to abort"
    read foo
fi

AUTOCONF="/pkg/autoconf/2.59/bin/autoconf"
echo "Running autoconf ($AUTOCONF)..."
$AUTOCONF || exit_clean 1 "$AUTOCONF failed"

for RMRFDIR in autom4te.cache yaws/docroot/su-internal; do
    test -d $RMRFDIR && (
	echo "Removing ${RMRFDIR}/"
	rm -rf "${RMRFDIR}"
    )
done

test -d autom4te.cache && (
    echo "Removing autom4te.cache/"
    rm -rf autom4te.cache
)

#for REMOVE_FILE in local.erl; do
#    test -f "$REMOVE_FILE" && (
#	echo "Removing ordinary file '$REMOVE_FILE'..."
#	rm -f "$REMOVE_FILE"
#    )
#done

for DIR in $(find . -type d | grep -v .svn); do
    pushd $DIR > /dev/null 2>&1 || exit_clean 1 "Failed changing into directory '$DIR' to remove stray files"
    EFILE=$(ls *.beam *.app \#* .\#* testclient.conf Makefile 2>/dev/null | xargs echo)
    if [ "x$EFILE" != "x" ]; then
	echo "Removing stray file(s) : $EFILE"
	for FILE in $EFILE; do
	    rm -f "$FILE"
	done
    fi
    popd > /dev/null 2>&1
done

if [ $(md5sum ssl.config | awk '{print $1}') != "9f051e5f98e053d251be18982cdadec4" ]; then
    exit_clean 1 "Wrong ssl.config (or my md5 checksum is out of date)"
fi

today=$(date +%Y-%m-%d)
if [ -z "`grep \"^-- $short_version[	 ]*($today)$\" CHANGES`" ]; then
    echo ""
    echo "WARNING: the file 'CHANGES' does not contain a release-line"
    echo ""
    echo "Expected :"
    echo ""
    echo "-- $short_version ($today)"
    echo ""
    echo "Press enter to continue anyways"
    read foo
fi


# remember source directory for build-test below
SRCDIR=$(pwd)

popd >/dev/null 2>&1

exportall=$(find $SRCDIR -name '*.erl' | xargs grep '^-compile(export_all).' 2>&1)
if [ "x$exportall" != "x" ]; then
    echo "One or more modules have '-compile(export_all).' :"
    echo ""
    echo "$exportall"
    echo ""
    exit_clean 1
fi

echo "Testing if it compiles..."
build_template="yxa-mksnapshot.XXXXXX"
if [ $make_release -ne 0 ]; then
    build_template="yxa-mkrelease.XXXXXX"
fi
BUILDDIR=$(mktemp -d $build_template)
test -d "$BUILDDIR" || exit_clean 1 "Failed creating temporary directory"
pushd "$BUILDDIR" >/dev/null 2>&1 || exit_clean 1 "Could not change directory to '$BUILDDIR'"

$SRCDIR/configure || exit_clean 1 "$SRCDIR/configure failed"
test -f Makefile || exit_clean 1 "$SRCDIR/configure did not produce a Makefile"
make || exit_clean 1 "'make' in directory '$BUILDDIR' failed"

for LOCAL in local_su_se local_kth_se; do
    test -f "$SRCDIR/src/${LOCAL}.erl" || exit_clean 1 "Source file '$SRCDIR/src/$LOCAL.erl' does not exist"
    echo "Testing ${LOCAL}..."
    $SRCDIR/configure --with-local=$LOCAL || exit_clean 1 "$SRCDIR/configure --with-local=$LOCAL failed"
    make || exit_clean 1 "--with-local=$LOCAL does not work"
done

echo "Building documentation"
make doc || exit_clean 1 "'make doc' failed"

echo "Running automatic regression tests..."
make test || exit_clean 1 "Regression tests failed"

# return from build-dir
popd >/dev/null 2>&1
rm -rf "$BUILDDIR" || exit_clean 1 "Could not remove temporary builddir '$BUILDDIR'"

echo ""
echo "Test-build was OK, creating $version_kind $TARFILE..."

echo "Creating $version_kind '$TARFILE'..."
tar zcvf "$TARFILE" "$version" || exit_clean 1 "'tar' failed"
echo ""
echo "Info about newly created $version_kind '$version' :"
echo ""
echo "MD5 :"
echo ""
md5sum "$TARFILE"
echo ""
echo "SHA1 :"
echo ""
sha1sum "$TARFILE"
echo ""
ls -l "$TARFILE"

echo ""
echo "Done"

if [ $make_release -ne 0 ]; then
    tags_url=$(echo $svn_url | sed -e "s#/branches.*\$#/tags/${short_version}#")
    
    echo ""
    echo "DON'T FORGET #1 : svn copy \"$svn_url\" \"$tags_url\""
    echo "                  (you might have to change 'svn://' to 'svn+ssh://')"
    echo ""
    echo "DON'T FORGET #2 : Update project homepage"
    echo "	/afs/stacken.kth.se/www/project/yxa/downloads.html"
    echo "	/afs/stacken.kth.se/www/project/yxa/index.html"
    echo ""
fi

exit_clean 0 ""
