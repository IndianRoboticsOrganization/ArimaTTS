# Process this file with autoconf to produce a configure script.
AC_INIT(HTS-demo_CMU-ARCTIC-SLT,2.2,hts-users@sp.nitech.ac.jp)
AC_COPYRIGHT(Copyright 2001-2011 Nagoya Institute of Technology)
AC_COPYRIGHT(Copyright 2001-2008 Tokyo Institute of Technology)



# Checks for programs.
# awk
AC_PROG_AWK



# perl
AC_PATH_PROG([PERL], [perl], [/usr/bin], [$PATH:/usr/bin])
if test ! -f "$PERL"; then
	AC_MSG_ERROR(Can't find perl)
fi





# bc
AC_PATH_PROG([BC], [bc], [/usr/bin], [$PATH:/usr/bin])
if test ! -f "$BC"; then
	AC_MSG_ERROR(Can't find bc)
fi

# wc
AC_PATH_PROG([WC], [wc], [/usr/bin], [$PATH:/usr/bin])
if test ! -f "$WC"; then
	AC_MSG_ERROR(Can't find wc)
fi

# tee
AC_PATH_PROG([TEE], [tee], [/usr/bin], [$PATH:/usr/bin])
if test ! -f "$TEE"; then
	AC_MSG_ERROR(Can't find tee)
fi



# festival
# festival search path
AC_ARG_WITH(fest-search-path,
		AS_HELP_STRING([--with-fest-search-path],[specify the additional festival search path]),
		[fest_search_path=$withval])

# dumpfeats (festival)
AC_PATH_PROG([DUMPFEATS], [dumpfeats], [:], [${fest_search_path}:$PATH])
if test ! -f "$DUMPFEATS"; then
	AC_MSG_ERROR(Can't find dumpfeats of festival)
fi



# hts_engine API
# hts_engine search path
AC_ARG_WITH(hts-engine-search-path,
		AS_HELP_STRING([--with-hts-engine-search-path],[specify the additional hts_engine binary search path]),
		[hts_engine_search_path=$withval])

# hts_engine
AC_PATH_PROG([ENGINE], [hts_engine], [:], [${hts_engine_search_path}:$PATH])
if test ! -f "$ENGINE"; then
	AC_MSG_ERROR(Can't find hts_engine)
fi


# output data/Makefile and scripts/Config.pm
AC_OUTPUT(scripts/Makefile scripts/hts_engine.pl)
