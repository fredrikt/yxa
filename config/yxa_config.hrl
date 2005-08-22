%%%-------------------------------------------------------------------
%%% File    : yxa_config.hrl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Records for Yxa configuration subsystem.
%%%
%%% Created : 16 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------

%% record for passing configuration parsed from backends to yxa_config
-record(yxa_cfg, {
	  entrys = []	%% list() of {Key, Value, Src} - Src is the backend module
	 }).

-record(cfg_entry, {
	  key,				%% atom()
	  default,			%% term(), the default value
	  required	= false,	%% true | false, required parameter?
	  list_of	= false,	%% true | false, list of values or not?
	  type,				%% atom | integer | Type (see below)
	  normalize	= true,  	%% true | false, normalize or leave as-is?
	  soft_reload	= true		%% true | false, can this parameters value be changed without restart?
	 }).

%% Types :
%%
%%   bool		the atom 'true' or 'false'
%%   term		term(), anything - no type checking or pre-parsing performed
%%   string		string is a list of lists __with length greater than one__
%%   regexp_rewrite	{Regexp, Rewrite} where Regexp = string(), Rewrite = string()
%%   regexp_match	{Regexp, Match} where Regexp = string(), Match = term()
%%   sipurl		string parseable by sipurl:parse()
%%   sip_sipurl		string parseable by sipurl:parse_url_with_default_protocol("sip", ...)
%%   sips_sipurl       	string parseable by sipurl:parse_url_with_default_protocol("sips", ...)
%%

%% parameters containing sensitive information that we should avoid logging when changing etc.
-define(NO_DISCLOSURE, [sipauth_password,
			ldap_password,
			x_yxa_peer_auth_secret,
			x_yxa_peer_auth]).
	
-define(COMMON_DEFAULTS,
	[
	 %% General
	 #cfg_entry{key		= databaseservers,
		    list_of	= true,
		    type	= atom
		   },
	 #cfg_entry{key		= logger_logbasename,
		    type	= string,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= max_logfile_size,
		    default	= 262144000,
		    type	= integer
		   },
	 #cfg_entry{key		= event_handler_handlers,
		    list_of	= true,
		    type	= atom
		   },
	 #cfg_entry{key	= homedomain,
		    list_of	= true,
		    type	= string
		   },

	 %% SIP authentication settings
	 #cfg_entry{key		= sipauth_realm,
		    type	= string,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipauth_password,
		    type	= string,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipauth_unauth_classlist,
		    default	= [],
		    list_of	= true,
		    type	= atom
		   },

	 %% Userdb
	 #cfg_entry{key		= userdb_modules,
		    default	= [sipuserdb_mnesia],
		    list_of	= true,
		    type	= atom,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= sipuserdb_file_filename,
		    type	= string,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= sipuserdb_file_refresh_interval,
		    type	= integer,
		    default	= 15
		   },
	 #cfg_entry{key		= sipuserdb_mysql_host,
		    type	= string
		   },
	 #cfg_entry{key		= sipuserdb_mysql_port,
		    type	= integer
		   },
	 #cfg_entry{key		= sipuserdb_mysql_user,
		    type	= string,
		    normalize   = false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_password,
		    type	= string,
		    normalize   = false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_database,
		    type	= string,
		    normalize   = false
		   },
	 %% sipuserdb_mysql
	 #cfg_entry{key		= sipuserdb_mysql_get_user,
		    type	= string,
		    default	= "select sipuser from users where sipuser = ?",
		    required	= true,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_get_user_for_address,
		    type	= string,
		    default	= "select sipuser from addresses where address = ?",
		    required	= true,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_get_addresses_for_user,
		    type	= string,
		    default	= "select address from addresses where sipuser = ?",
		    required	= true,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_get_classes_for_user,
		    type	= string,
		    default	= "select class from classes where sipuser = ?",
		    required	= true,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_get_password_for_user,
		    type	= string,
		    default	= "select password from users where sipuser = ?",
		    required	= true,
		    normalize	= false
		   },
	 #cfg_entry{key		= sipuserdb_mysql_get_telephonenumber_for_user,
		    type	= string,
		    default	= "select address from addresses where sipuser = ? and is_telnr = 'Y'",
		    required	= true,
		    normalize	= false
		   },


	 %% SIP stack settings
	 #cfg_entry{key		= default_max_forwards,
		    default	= 70,
		    type	= integer
		   },
	 #cfg_entry{key		= max_max_forwards,
		    default	= 255,
		    type	= integer
		   },
	 #cfg_entry{key		= detect_loops,
		    default	= true,
		    type	= bool,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= request_rport,
		    default	= false,
		    type	= bool
		   },
	 #cfg_entry{key		= stateless_challenges,
		    default	= false,
		    type	= bool
		   },
	 #cfg_entry{key		= record_route,
		    default	= false,
		    type	= bool
		   },
	 #cfg_entry{key		= record_route_url,
		    type	= sipurl
		   },
	 #cfg_entry{key	= timerT1,
		    default	= 500,
		    type	= integer
		   },
	 #cfg_entry{key	= timerT2,
		    default	= 4000,
		    type	= integer
		   },
	 #cfg_entry{key	= timerT4,
		    default	= 5000,
		    type	= integer
		   },

	 %% Transport layer settings
	 #cfg_entry{key		= listenport,
		    default	= 5060,
		    type	= integer,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= tls_listenport,
		    default	= 5061,
		    type	= integer,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= myhostnames,
		    required	= true,
		    list_of	= true,
		    type	= string
		   },
	 #cfg_entry{key		= tcp_connection_idle_timeout,
		    default	= 300,
		    type	= integer
		   },
	 #cfg_entry{key		= tcp_connect_timeout,
		    default	= 20,
		    type	= integer},
	 #cfg_entry{key		= enable_v6,
		    default	= false,
		    type	= bool
		   },

	 %% TLS settings
	 #cfg_entry{key		= ssl_server_certfile,
		    type	= string
		   },
	 #cfg_entry{key		= ssl_client_certfile,
		    type	= string
		   },
	 #cfg_entry{key		= ssl_require_client_has_cert,
		    default	= false,
		    type	= bool
		   },
	 #cfg_entry{key		= ssl_client_ssloptions,
		    default	= [],
		    list_of	= true,
		    type	= term
		   },
	 #cfg_entry{key		= ssl_server_ssloptions,
		    default	= [],
		    list_of	= true,
		    type	= term
		   },
	 #cfg_entry{key		= ssl_check_subject_altname,
		    default	= true,
		    type	= bool
		   },
	 #cfg_entry{key		= ssl_require_sips_registration,
		    default	= true,
		    type	= bool
		   },
	 #cfg_entry{key		= tls_disable_client,
		    default	= false,
		    type	= bool
		   },
	 #cfg_entry{key		= tls_disable_server,
		    default	= false,
		    type	= bool,
		    soft_reload	= false
		   },

	 %% LDAP
	 #cfg_entry{key		= ldap_server,
		    type	= string,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= ldap_username,
		    type	= string,
		    normalize	= false,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= ldap_password,
		    type	= string,
		    normalize	= false,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= ldap_searchbase,
		    type	= string,
		    normalize	= false,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= ldap_use_ssl,
		    default	= false,
		    type	= bool,
		    soft_reload	= false
		   },
	 #cfg_entry{key		= ldap_connection_query_limit,
		    default	= 500,
		    type	= integer
		   },
	 #cfg_entry{key		= ldap_userattribute,
		    default	= "sipAuthenticationUser",
		    type	= string,
		    normalize	= false,
		    required	= true
		   },
	 #cfg_entry{key		= ldap_addressattribute,
		    default	= "sipLocalAddress",
		    type	= string,
		    normalize	= false,
		    required	= true
		   },
	 #cfg_entry{key		= ldap_passwordattribute,
		    default	= "sipPassword",
		    type	= string,
		    normalize	= false,
		    required	= true
		   },
	 #cfg_entry{key		= ldap_telephonenumberattribute,
		    default	= "telephoneNumber",
		    type	= string,
		    normalize	= false,
		    required	= true
		   },

	 %% CPL settings
	 #cfg_entry{key	= cpl_minimum_ringtime,
		    default	= 10,
		    type	= integer,
		    required	= true
		   },
	 #cfg_entry{key	= cpl_call_max_timeout,
		    default	= 120,
		    type	= integer,
		    required	= true
		   },
	 #cfg_entry{key	= cpl_time_switch_count_max_lookahead,
		    default	= 20,
		    type	= integer,
		    required	= true
		   },
	 #cfg_entry{key	= cpl_time_switch_count_max,
		    default	= 100,
		    type	= integer,
		    required	= true
		   }
	]).

%%
%% Application defaults - these override the common defaults.
%% You can either re-define something here (like overriding the default value of 
%% 'record_route' for pstnproxy), or add a setting that is completely application
%% specific, like 'appserver' for incomingproxy.
%%
-define(APPLICATION_DEFAULTS,
	[
	 {pstnproxy,
	  [
	   #cfg_entry{key	= databaseservers,
		      list_of	= true,
		      type	= atom,
		      required	= true
		     },
	   #cfg_entry{key	= internal_to_e164,
		      default	= [],
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= e164_to_pstn,
		      default	= [],
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= number_to_pstn,
		      default	= [],
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= pstngatewaynames,
		      list_of	= true,
		      type	= string,
		      required	= true
		     },
	   #cfg_entry{key	= default_pstngateway,
		      type	= string
		     },
	   #cfg_entry{key	= classdefs,
		      default	= [{"", unknown}],
		      list_of	= true,
		      type	= regexp_match
		     },
	   #cfg_entry{key	= sipproxy,
		      type	= sip_sipurl
		     },
	   #cfg_entry{key	= enum_domainlist,
		      default	= [],
		      list_of	= true,
		      type	= string
		     },
	   #cfg_entry{key	= remote_party_id,
		      default	= false,
		      type	= bool
		     },
	   #cfg_entry{key	= pstnproxy_no_sip_dst_code,
		      default	= 480,
		      type	= integer
		     },
	   #cfg_entry{key	= pstnproxy_redirect_on_enum,
		      default	= false,
		      type	= bool
		     },
	   #cfg_entry{key	= x_yxa_peer_auth_secret,
		      type	= string,
		      normalize	= false
		     },
	   #cfg_entry{key	= record_route,
		      default	= true,
		      type	= bool,
		      required	= true
		     },

	   %% undocumented, don't use unless you really know you should!
	   #cfg_entry{key	= pstnproxy_ignore_route_header_bad_idea,
		      default	= false,
		      type	= bool
		     }
	  ]},

	 {incomingproxy,
	  [
	   #cfg_entry{key	= internal_to_e164,
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= e164_to_pstn,
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= number_to_pstn,
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= defaultroute,
		      type	= sip_sipurl
		     },
	   #cfg_entry{key	= enum_domainlist,
		      list_of	= true,
		      type	= string
		     },
	   #cfg_entry{key	= max_register_time,
		      default	= 43200,
		      type	= integer
		     },
	   #cfg_entry{key	= always_verify_homedomain_user,
		      default	= true,
		      type	= bool
		     },
	   #cfg_entry{key	= appserver,
		      type	= sip_sipurl
		     },
	   #cfg_entry{key	= homedomain,
		      list_of	= true,
		      type	= string,
		      required	= true
		     }
	  ]},

	 {outgoingproxy,
	  [
	   #cfg_entry{key	= databaseservers,
		      list_of	= true,
		      type	= atom,
		      required	= true
		     },
	   #cfg_entry{key	= sipproxy,
		      type	= sip_sipurl
		     },
	   #cfg_entry{key	= max_register_time,
		      default	= 43200,
		      type	= integer
		     },
	   #cfg_entry{key	= tcp_connection_idle_timeout,
		      default	= 10000000,
		      type	= integer
		     },
	   #cfg_entry{key	= record_route,
		      default	= true,
		      type	= bool,
		      required	= true
		     }
	  ]},

	 {appserver,
	  [
	   #cfg_entry{key	= databaseservers,
		      list_of	= true,
		      type	= atom,
		      required	= true
		     },
	   #cfg_entry{key	= internal_to_e164,
		      default	= [],
		      list_of	= true,
		      type	= regexp_rewrite
		     },
	   #cfg_entry{key	= appserver_call_timeout,
		      default	= 40,
		      type	= integer
		     },
	   #cfg_entry{key	= appserver_forward_timeout,
		      default	= 40,
		      type	= integer
		     },
	   #cfg_entry{key	= x_yxa_peer_auth,
		      default	= [],
		      list_of	= true,
		      type	= regexp_match,
		      normalize	= false
		     }
	  ]},
	 {testserver, 
	  [
	   #cfg_entry{key	= testserver_userdb,
		      default	= [],
		      list_of	= true,
		      type	= regexp_match,
		      required	= true
		     }
	  ]},
	 {admin_www,
	  [
	   #cfg_entry{key	= databaseservers,
		      list_of	= true,
		      type	= atom,
		      required	= true
		     },
	   #cfg_entry{key	= httpd_config,
		      type	= string,
		      required	= true
		     },
	   #cfg_entry{key	= www_baseurl,
		      type	= string,
		      required	= true
		     }
	  ]}
	]
       ).
