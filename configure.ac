dnl Process this file with autoconf to produce a configure script.
AC_REVISION($Revision$)
AC_PREREQ(2.53)
AC_INIT(yxa, 0.1, yxa-devel@lists.su.se)
AC_CONFIG_SRCDIR([registrar.erl])

AC_PROG_CC
AC_PROG_INSTALL

AC_CONFIG_FILES(Makefile)

AC_ARG_WITH(mnesiadir,
	AC_HELP_STRING([--with-mnesiadir=dir],
		[use dir as database for mnesia database]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_$1" = "X"; then
    mnesiadir=""
else
    mnesiadir="$with_mnesiadir"
fi])

AC_ARG_WITH(local,
	AC_HELP_STRING([--with-local=file],
		[use file as local erlang configuration file]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_$1" = "X"; then
    local_file="default"
else
    local_file="$with_local"
fi])


AC_SUBST(DESTDIR)
AC_SUBST(mnesiadir)
AC_SUBST(local_file)

AC_OUTPUT

dnl
dnl This is the release version name-number[beta]
dnl

cat > newversion.erl.in <<EOF
-module(${PACKAGE_NAME}_version).
-export([[get_long_version/0, get_version/0]]).

get_long_version() ->
    "Version: $PACKAGE_STRING by @USER@ on @HOST@ @DATE@".
get_version() ->
    "AC_PACKAGE_STRING".
EOF

if test -f version.erl && cmp -s newversion.erl.in version.erl.in; then
	echo "version.erl is unchanged"
	rm -f newversion.erl.in
else
 	echo "creating version.erl"
 	User=${USER-${LOGNAME}}
 	Host=`(hostname || uname -n || echo unknown) 2>/dev/null | sed 1q`
 	Date=`date`
	mv -f newversion.erl.in version.erl.in
	sed -e "s/@USER@/$User/" -e "s/@HOST@/$Host/" -e "s/@DATE@/$Date/" version.erl.in > version.erl
fi
