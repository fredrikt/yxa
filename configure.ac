dnl Process this file with autoconf to produce a configure script.
AC_REVISION($Revision$)
AC_PREREQ(2.64)
AC_INIT(YXA, 1.0-current, yxa-devel@lists.su.se)
REQUIRE_ERLANG_MIN="5.7.2 (Erlang/OTP R13B-1)"
REQUIRE_ERLANG_MAX="5.9.1 (Erlang/OTP R15B01)"
AC_CONFIG_SRCDIR([src/incomingproxy.erl])
AC_CONFIG_AUX_DIR([scripts])

AC_PROG_CC
AC_PROG_INSTALL

AC_ARG_WITH(erlang,
	AC_HELP_STRING([--with-erlang=dir],
		[search for erlang in dir]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_erlang" = "X"; then
    extra_erl_path=""
else
    extra_erl_path="$with_erlang:$with_erlang/bin:"
fi
])

AC_PATH_TOOL(ERL, erl, , [${extra_erl_path}$PATH])
AC_PATH_TOOL(ERLC, erlc, , [${extra_erl_path}$PATH])

AC_ERLANG_NEED_ERLC
AC_ERLANG_PATH_ERL

dnl check that the installed Erlang/OTP version is the right one for this version of YXA
AC_ARG_ENABLE(erlang-version-check,
[AC_HELP_STRING([--enable-erlang-version-check],
	[Check Erlang/OTP version @<:@default=yes@:>@])])
	case "$enable_erlang_version_check" in
	yes|'')
		ERLANG_VERSION_CHECK([$REQUIRE_ERLANG_MIN],[$REQUIRE_ERLANG_MAX])
		;;
	no)
		ERLANG_VERSION_CHECK([$REQUIRE_ERLANG_MIN],[$REQUIRE_ERLANG_MAX],[warn])
		;;
esac

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_ERTS_VER
ERLANG_SUBST_LIB_VER(asn1)
ERLANG_SUBST_LIB_VER(kernel)
ERLANG_SUBST_LIB_VER(mnesia)
ERLANG_SUBST_LIB_VER(ssl)
ERLANG_SUBST_LIB_VER(stdlib)
ERLANG_SUBST_LIB_VER(yaws)
ERLANG_SUBST_LIB_VER(crypto)
ERLANG_SUBST_LIB_VER(public_key)

AC_CHECK_FILE([${ERLANG_LIB_DIR_public_key}/include/OTP-PUB-KEY.hrl],[HAS_OTP_PUB_KEY_HRL="true"],[HAS_OTP_PUB_KEY_HRL="false"])
AC_SUBST(HAS_OTP_PUB_KEY_HRL)

AC_CONFIG_FILES(
		Makefile \
		src/Makefile \
		src/config/Makefile \
		src/cpl/Makefile \
		src/event/Makefile \
		src/event_handler/Makefile \
		src/include/Makefile \
		src/local/Makefile \
		src/mysql/Makefile \
		src/eldap/Makefile \
		src/database/Makefile \
		src/transportlayer/Makefile \
		src/sipuserdb/Makefile \
		src/transactionlayer/Makefile \
		yaws/src/Makefile
	       )

AC_ARG_WITH(mnesiadir,
	AC_HELP_STRING([--with-mnesiadir=dir],
		[use dir as database for mnesia database]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_mnesiadir" = "X"; then
    mnesiadir="/var/yxa/db"
else
    mnesiadir="$with_mnesiadir"
fi],[mnesiadir="/var/yxa/db"])

AC_ARG_WITH(sslcertdir,
	AC_HELP_STRING([--with-sslcertdir=dir],
		[use dir as path to your SSL certificates]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_sslcertdir" = "X"; then
    sslcertdir='${confdir}'
else
    sslcertdir="$with_sslcertdir"
fi],[sslcertdir='${confdir}'])


AC_ARG_WITH(local,
	AC_HELP_STRING([--with-local=module],
		[use module as local hooks module (e.g. "my_local" - don't include the ".erl" suffix)]),
[if test "$withval" = "yes" -o "$withval" = "no" -o "X$with_local" = "X"; then
    local_module="local_default"
else
    local_module="$with_local"
fi],[local_module="local_default"])

AC_SUBST(DESTDIR)
AC_SUBST(mnesiadir)
AC_SUBST(sslcertdir)
AC_SUBST(local_module)
AC_SUBST(ERL)
AC_SUBST(ERLC)

AC_OUTPUT

dnl
dnl This is the release version name-number[beta]
dnl

cat > newversion.erl.in <<EOF
-module(version).
-export([[get_version/0,
	 get_long_version/0
	]]).

get_package_name() -> "AC_PACKAGE_NAME".
get_package_version() -> "AC_PACKAGE_VERSION".

get_compile_user() -> "@USER@".
get_compile_host() -> "@HOST@".
get_compile_date() -> "@DATE@".

get_long_version() ->
    get_package_name() ++ " " ++
    get_package_version() ++ ", compiled by " ++
    get_compile_user() ++ " on " ++
    get_compile_host() ++ " at " ++
    get_compile_date().
get_version() ->
    get_package_version().
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
