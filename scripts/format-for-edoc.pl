#!/usr/bin/perl
#
# Test with : cp new_transactionlayer.erl old_transactionlayer.erl; \
#		../../scripts/format-for-edoc.pl transactionlayer.erl; \
#		diff -u old_transactionlayer.erl new_transactionlayer.erl
#
#  and then : /pkg/erlang/R11B-2/bin/erl -noshell -run edoc files \
#		'new_transactionlayer.erl' -run init stop
#

use strict;

my %global_data = ("transactionstatelist"			=> "private",
		   "transactionlayer:record:thandler"		=> "Server transaction process reference",
		   "sipuserdb_file_backend"			=> "private",
		   "sipsocket_blacklist_probe"			=> "private",
		   "sipsocket_blacklist"			=> "private",
		   "tcp_connection"				=> "private",
		   "tcp_receiver"				=> "private",
		   "tcp_listener"				=> "private",
		   "tcp_dispatcher"				=> "private",
		   "socketlist"					=> "private",
		   "table_update"				=> "private",
		   "database_call"				=> "hidden",
		   "presence_xmerl_xml"				=> "private",
		   "yxa_app"					=> "hidden",
		   "event_package"				=> "hidden",
		   "incomingproxy_test"				=> "hidden",
		   "outgoingproxy_test"				=> "hidden",
		   "appserver_test"				=> "hidden",
		   "pstnproxy_test"				=> "hidden",
		   "eventserver_test"				=> "hidden",
		   "interpret_time_test"			=> "hidden",
		   "interpret_cpl_test"				=> "hidden",
		   "test_backend"				=> "hidden",
		   "cpl_test_util"				=> "hidden",
		   "xml_parse_test"				=> "hidden",
		   "local_su_se"				=> "hidden",
		   "local_kth_se"				=> "hidden",
		   "local_default"				=> "hidden",
		   "group_regexp"				=> "hidden",
		   "key_val_db"					=> "private",
		   "targetlist"					=> "private",
		   #"contact_param"				=> "private",
		   #"url_param"					=> "private",
		   "fun:terminate/2"				=> "(Reason, State)",
		   "fun_tag:test"				=> "\@hidden",
		   "fun_tag:test_create_table"			=> "\@hidden",
		   "fun_tag:test_get_thandler_self"		=> "\@private",
		   "fun_tag:yxa_config:init_config"		=> "\@private",
		   "fun_tag:yxa_init"				=> "\@private",
		   "fun_tag:create"				=> "\@private",
		   "fun_tag:decode_mnesia_change_event"		=> "\@private",
		   "fun_tag:get_transform_fun"			=> "\@private",
		   "fun_tag:active_subscriber:ft"		=> "\@hidden",
		   "fun_tag:appserver:locations_to_actions"	=> "\@private",
		   "fun_tag:appserver:location_to_call_action"	=> "\@private",
		   "fun_tag:appserver_glue:start_link_cpl"	=> "\@private",
		   "fun_tag:local:default_url2mnesia_userlist"	=> "\@private",
		   "fun_tag:local:default_canonify_user"	=> "\@private",
		   "fun_tag:local:default_canonify_addresses"	=> "\@private",
		   "fun_tag:group_regexp:first_match"		=> "\@hidden",
		   "fun_tag:group_regexp:format_error"		=> "\@hidden",
		   "fun_tag:group_regexp:gsub"			=> "\@hidden",
		   "fun_tag:group_regexp:match"			=> "\@hidden",
		   "fun_tag:group_regexp:matches"		=> "\@hidden",
		   "fun_tag:group_regexp:parse"			=> "\@hidden",
		   "fun_tag:group_regexp:sh_to_awk"		=> "\@hidden",
		   "fun_tag:group_regexp:split"			=> "\@hidden",
		   "fun_tag:group_regexp:sub"			=> "\@hidden",
		   "fun_tag:gruu:show_all"			=> "\@hidden",
		   "fun_tag:interpret_backend:test_proxy_destinations"	=> "\@hidden",
		   "fun_tag:interpret_cpl:test27"		=> "\@hidden",
		   # gen_server stuff
#		   "fun_tag:start_link"				=> "\@private",
		   "fun_tag:init"				=> "\@hidden",
		   "fun_tag:behaviour_info"			=> "\@hidden",
		   "fun_tag:handle_call"			=> "\@hidden",
		   "fun_tag:handle_cast"			=> "\@hidden",
		   "fun_tag:handle_info"			=> "\@hidden",
		   "fun_tag:code_change"			=> "\@hidden",
		   "fun_tag:terminate"				=> "\@hidden"
		    );

my $delete_description_after_type = 0;
my $turn_function_result_into_term = 1;
my $fix_TIMEOUT_macro = 1;
my $debug = 0;
my $spec_separators = 1;

my $desc_count = 0;

my @files = @ARGV;
if (! $files[0] or ! -f $files[0]) {
    die ("Syntax: $0 file ...\n");
}

foreach my $file (@files) {
    do_file($file);
}

exit (0);

sub do_file
{
    my $in = shift;
    my $out = "new_${in}";

    my $module;
    if ($in =~ /([^\/]+)\.erl$/) {
	$module = $1;
    } else {
	die ("$0: Can't figure out module name from '$in'\n");
    }

    print ("Processing '${module}'\n");

    open (IN, "< $in") or die ("$0: Couldn't open in-file '$in' : $!\n");
    open (OUT, "> $out") or die ("$0: Couldn't open out-file '$out' : $!\n");

    my $before_module = 1;
    my $in_function = '';

    my $output_clear_after_buf = 0;

    my $buf = '';
    my %function_data;
    $function_data{module_name} = $module;

    my @lines = <IN>;
    close (IN);

    while (my $line = shift @lines) {
	my $prefix = '';
	my $suffix = '';
	chomp($line);
	$line =~ s/\s+$//o;	# remove trailing whitespace

	if ($before_module) {
	    parse_before_module (\$line, $module, \$before_module, \$suffix, \$prefix);
	} else {
	    if ($line =~ /^%%\s*--------/o or $line =~ /^%%\s-\s-\s-\s-\s-\s-/o) {
		# delimeter, output $buf
		if ($in_function ne '') {
		    print_function (\%function_data, $in_function);
		    $in_function = '';
		    undef (%function_data);
		    $function_data{module_name} = $module;
		}

		#warn ("Print buf :\n$buf\n---end\n") if ($debug);
		print (OUT $buf);
		$buf = '';

		if ($output_clear_after_buf) {
		    $suffix = "\n%% \@clear\n${suffix}";
		    $output_clear_after_buf = 0;
		}
	    } elsif ($line =~ /^%%/o) {
		while ($lines[0] =~ /^%%\s{16,}([^=]+)$/o) {
		    # next line starts with 16 spaces and not containing "=" - a multi-line description,
		    # add to $line before we continue
		    my $nl = $1;
		    shift @lines;
		    chomp ($nl);
		    $line .= " $nl";
		}
		parse_comment(\$line, \$in_function, \%function_data, \$buf, \$output_clear_after_buf);
		next;
	    }

	    if ($line =~ /^-record\(([a-z]+)\s*,/o) {
		# record definitions, create @type
		my $record = $1;
		print_record($record, $module, \$buf, \$prefix);
	    }

	}

#	warn ("Print the following :\n" .
#	      "buf :\n$buf\n" .
#	      "prefix :\n$prefix\n" .
#	      "line :\n$line\n" .
#	      "suffix :\n$suffix\n" .
#	      "---end\n") if ($debug);
	print (OUT "${buf}${prefix}${line}\n${suffix}");
	$buf = '';
    }

    close (OUT);

    return 1;
}

sub print_record
{
    my $record = shift;
    my $module = shift;
    my $buf = shift;
    my $prefix = shift;

    my $record_desc;

    my $type_str = "%% \@type ${record}()";
    
    if (! $$buf) {
	$$buf = $global_data{"${module}:record:${record}"} || '%% no description';
    }

    my $t = "%%" . " " x (length ($type_str) - 2);
    $record_desc = reformat_comment("$t   ", 80, '', $$buf);		# three extra spaces for " = "
    $$buf = '';
    
    $$prefix =
	"${type_str} = #${record}\{\}.\n" .
	"$record_desc";
}

sub parse_comment
{
    my $line = shift;
    my $in_function = shift;
    my $function_data = shift;
    my $buf = shift;
    my $output_clear_after_buf = shift;

    if ($$line =~ /^%% \@/o and $$in_function) {
	$$function_data{tag} .= "$$line\n";
	return 1;
    }

    if ($$line =~ /^%%\sFunction\s*:\s*(\S+?)(\(.*?)\s*$/o) {
	$$in_function = 'spec';
	$$function_data{function_name} = $1;

	if ("$1$2" eq 'handle_call(Msg, From, State)' or
	    "$1$2" eq 'handle_cast(Msg, State)' or
	    "$1$2" eq 'handle_info(Msg, State)' or
	    "$1$2" eq 'package_parameters(PkgS::event_pkg(), Param)' or
	    "$1$2" eq 'subscription_behaviour(PkgS::event_pkg(), Param, Argument)' or
	    "$1$2" eq 'handle_event(Event, State)' or
	    "$1$2" eq 'init(Arg)') {
	    # keep function name in spec for our handle_{call, cast, info} templates, but
	    # not for other functions
	    $$line = "$1$2";
	    $$output_clear_after_buf = 1;
	} else {
	    #warn ("FUN ARGS '$$line' -> $2\n");
	    $$line = "$2";
	}
    } elsif ($$line =~ /^%%\sFunction\s*:\s*(.+\/\d+)\s*$/o) {
	$$in_function = 'spec';

	my $name = $1;
	# incorrect, written as function/arity
	if ($global_data{"fun:$name"}) {
	    $$line = $global_data{"fun:$name"};
	} else {
	    die ("Incorrect function : '$name' and no known substitution\n");
	}
    } elsif ($$in_function and $$line =~ /^%%\s(Descrip|Purpose)\.*\s*:\s*(.*)/o) {
	$$in_function = 'descrip';
	$$line = "$2";
    } elsif ($$in_function and $$line =~ /^%%\sReturns\s*:\s*(.*)/o) {
	$$in_function = 'returns';
	$$line = "$1";
	if ($$line =~ /^([a-z_]+:[a-z0-9_]+(\/\d|\(\)))/o) {
	    # Something like 'gen_server:start_link/4', turn into
	    # %%   Result
	    # %%     Result = term. Result of gen_server:start_link/4
	    $$line = "Result\n%% Result = term().\n Result of $1";

	    if ($turn_function_result_into_term) {
		$$line = "term()";
	    }
	} elsif ($$line =~ /^\s*([A-Z][A-Za-z0-9_]+)\s+=\s+/o) {
	    # turn "%% Returns: Pid = pid()" into "%% Returns : Pid\n%%   Pid = pid()"
	    my $t = $1;
	    my $t_desc = remove_description ($line);
	    my $new_desc = '';
	    $new_desc = reformat_description ($t_desc) if ($t_desc);
	    $$line = "$t\n%%  $$line$new_desc";
	} elsif ($$line eq 'ok | throw()' and $$function_data{function_name} eq 'test') {
	    $$line = 'ok';
	} elsif ($$line =~ /^yet to be/oi) {
	    $$line = "term(), $$line";
	} elsif ($$line =~ /^any[,\s]*\((.+)\)\s*$/o) {
	    # turn "any (descript.)" into "term(), descript."
	    $$line = "term(), $1";
	}
    } elsif ($$in_function eq 'returns' && $$line =~ /^%%\s+Note/oi) {
	$$in_function = 'notes';
    } elsif ($$in_function eq 'spec' && $$line =~ / = /o) {
	$$in_function = 'params';
    }
    
    if ($$in_function eq 'spec' || $$in_function eq 'params' || $$in_function eq 'returns') {
	next if ($$line =~ /^%+\s*$/);	# empty line

	# turn foo record() into #foo{}
	$$line =~ s/(\S+)\s+record\(\)/\#$1\{\}$2/og;

	if ($$in_function eq 'returns') {
	    $$line =~ s/^(.*\|)\s+\(.*$/$1/o;	# delete everything after pipe-char and parenthesis
	    $$line =~ s/^(.*?)\s+\(terminate\/2 is called\)/$1/o;	# delete suffix "(terminate/2 is called)"
	}

	# turn "list() of something, list of foos" into "[something], list of foos"
	if ($$line =~ /\slist\(\)\s+of\s+/ or
	    $$line =~ /^list\(\)\s+of\s+/) {

	    my $t_desc = remove_description ($line);

	    if ($$line =~ /\slist\(\)\s+of\s+(.*?)\s+\|/ or
		$$line =~ /^list\(\)\s+of\s+(.*?)\s+\|/) {
		# list() of string() | something
		$$line =~ s/\slist\(\)\s+of\s+(.*?)\s+\|/ \[$1\] |/o;
		$$line =~ s/^list\(\)\s+of\s+(.*?)\s+\|/\[$1\] |/o;
	    } else {
		# not list of ... OR (|) something, just plain "list() of type"
		$$line =~ s/\slist\(\)\s+of\s+(.*)$/ \[$1\]/o;
		$$line =~ s/^list\(\)\s+of\s+(.*)$/\[$1\]/o;
	    }
	    if ($t_desc and ! $delete_description_after_type) {
		$$line .= reformat_description ($t_desc);
	    }
	}
    }

    if ($$in_function eq 'returns' and $$line =~ /throw\((.*)\)[\s\|]*$/) {
	my $throw = $1;
	if ($1 =~ /\{siperror,.*\.\.\./) {
	    push (@{$$function_data{throws}},
		  "{siperror, Status, Reason}",
		  "{siperror, Status, Reason, ExtraHeaders}"
		  );
	} else {
	    push (@{$$function_data{throws}}, $throw);
	}

	return 1;
    }
    
    if    ($$in_function eq 'spec')	{ $$function_data{spec}		.= "$$line\n"; }
    elsif ($$in_function eq 'params')	{ $$function_data{params}	.= "$$line\n"; }
    elsif ($$in_function eq 'descrip')	{ $$function_data{descrip}	.= "$$line\n"; }
    elsif ($$in_function eq 'returns')	{ $$function_data{returns}	.= "$$line\n"; }
    elsif ($$in_function eq 'notes')	{ $$function_data{notes}	.= "$$line\n"; }
    elsif ($$in_function ne '') {
	die ("BAD \$in_function VALUE : '$$in_function'\n");
    } else {
	# comment, store in $buf
	$$buf .= "$$line\n";
    }

    return 1;
}

sub reformat_comment
{
    my $first_line = shift;
    my $max_len = shift;
    my $special = shift;

    my $out = '';

    my @words = ();
    while (my $in = shift) {
	foreach my $this (split ("\n", $in)) {
	    $this =~ s/^%+\s+//o;	# remove any "%%   "

	    push (@words, split (" ", $this));
	}
    }

    my $out = $first_line;
    my $spaces = length ($out) - 2;	# 2 is length ("%%")
    my $current_line_length = length ($out);

    while (my $word = shift @words) {
	my $w_len = length ($word);

	my $add;
	my $do_linebreak = ($current_line_length + $w_len > $max_len);

	if ($special eq 'function_spec') {
	    if (! $do_linebreak) {
		if ($word =~ /^\(/o) {
		    # starts with "("
		    if ($out ne $first_line) {
			# not the very first word
			$do_linebreak = 1;
		    }
		}
	    }
	}

	if ($word eq "%%") {
	    $word = '';
	    $do_linebreak = 1;
	} else {
	    $word = "$word ";
	}

	if ($do_linebreak) {
	    $add = "\n%%" . " " x $spaces . $word;
	    $current_line_length = length ($add);
	} else {
	    $add = $word;
	    $current_line_length += length ($add);
	}

	$out .= $add;
    }

    # remove trailing spaces
    my @t = split ("\n", $out);
    $out = '';
    foreach my $t2 (@t) {
	$t2 =~ s/\s*$//o;
	$out .= "$t2\n";
    }

    return $out;
}

sub indent_align
{
    my $prefix = shift;
    my $spaces = shift;
    my $align  = shift;
    my $in = shift;

    my $indented = indent($prefix, $spaces, $in);

    # figure out max length of whatever is left of $align
    my $max = 0;
    my $t;
    my @lines = split ("\n", $indented);
    foreach $t (@lines) {
	my $index = index ($t, $align);
	if ($index >= 0) {
	    my $lhs = substr ($t, 0, $index);
	    $lhs =~ s/\s+$//o;	# remove trailing whitespace

	    my $this_len = length ($lhs);
	    $max = $this_len if ($this_len > $max);
	}
    }

    my @out;
    foreach $t (@lines) {
	my $index = index ($t, $align);
	if ($index >= 0) {
	    my $lhs = substr ($t, 0, $index);
	    my $rest = substr ($t, $index);

	    $lhs =~ s/\s+$//o;	# remove trailing whitespace
	    my $this_len = length ($lhs);
	    my $pad = $max - $this_len;

	    push (@out, "$lhs " . " " x $pad . "$rest");
	} else {
	    push (@out, $t);
	}
    }
    my $out = join ("\n", @out);

    #warn ("INDENT ALIGN ON '$align', MAX = $max\nIN :\n$indented\nOUT :\n$out\n");

    return ($out);
}


sub indent
{
    my $prefix = shift;
    my $spaces = shift;
    my $in = shift;
    my @out;
    foreach my $t (split ("\n", $in)) {
	$t =~ s/^%%\s+//o;
	next unless $t;	# skip empty lines

	my $new = $prefix . " " x 12 . $t;
	push (@out, $new);
    }

    return (join ("\n", @out));
}


sub parse_before_module
{
    my $line = shift;
    my $module = shift;
    my $before_module = shift;
    my $suffix = shift;
    my $prefix = shift;

    if ($$line =~ /^%%% \@author/o) {
	die ("$0: '$module' already converted.\n");
    }

    $$line =~ s/^%%%\s+(Author\s+:)/%%% \@author  /o;
    $$line =~ s/^%%%\s+(Descrip\.\s*:)/%%% \@doc     /o;
    $$line =~ s/^%%%\s+(Created\s+:)/%%% \@since   /o;

    if ($$line =~ /^%%% \@since/) {
	$$suffix = "%%% \@end\n";
	if ($global_data{$module} eq "hidden") {
	    $$suffix = "$$suffix%%% \@hidden\n";
	}

	if ($global_data{$module} eq "private") {
	    $$suffix = "$$suffix%%% \@private\n";
	}
    }
    
    if ($$line =~ /^-module\(/) {
	$$before_module = 0;
    }
}

sub print_function
{
    my $function_ref = shift;

    my %function_data = %{$function_ref};

    my $function_name = $function_data{function_name};
    my $module_name = $function_data{module_name};

    #my $want_debug_for_function = 'parse_accept_language';
    my $want_debug_for_function = 'test_proxy_destinations';

    my $spec_str  = "%% \@spec    ";
    my $doc_str   = "%% \@doc     ";
    my $spec_tmp = reformat_comment ($spec_str, 70, 'function_spec', $function_data{spec});
    my @spec = split ("\n", $spec_tmp);
    my $last_spec_line = pop (@spec);
    chomp ($last_spec_line);

    if (! $function_data{returns}) {
	$function_data{returns} = 'term()';
    }

    my $return_added = 0;

    # remove " |" from the last line of our Returns spec (typically because the last last thing was a throw())
    $function_data{returns} =~ s/\s*\|\s*[\r\n]+$//o;

    warn ("RETURNS IN-DATA : $function_data{returns}\n") if ($function_name eq $want_debug_for_function);

    if (length ($last_spec_line) < 67) {
	my @returns = split ("\n", $function_data{returns});

	warn ("NUMBER OF RETURNS ROWS = " . scalar @returns . " :\n$function_data{returns}\n") if ($function_name eq $want_debug_for_function);

	if (1 == scalar @returns) {
	    # 'returns' is a single line, see if it will fit
	    my $r = $returns[0];
	    chomp ($r);
	    my $r_desc = remove_description (\$r);
	    if ($r_desc and ! $delete_description_after_type) {
		$r .= reformat_description ($r_desc);
	    }

	    my $t = $last_spec_line . " -> $r";
	    if (length ($t) < 71) {
		$last_spec_line = "$t\n";
		$return_added = 1;
	    } else {
		warn ("'$t' WON'T FIT\n") if ($function_name eq $want_debug_for_function);
		my $prefix = "%%            ";
		if (length ($r) + length ($prefix) < 71) {
		    $last_spec_line .= " ->\n$prefix$r";
		    $return_added = 1;
		}
	    }
	}
	$last_spec_line .= " ->\n" unless ($return_added);
    } else {
	if ($last_spec_line =~ /^(%%\s+)(.+?) (.*)$/o) {
	    my $prefix = $1;
	    my $middle = $2;
	    my $last = $3;
	    $last_spec_line =
		"${prefix}${middle}\n" .
		"${prefix}${last} ->\n";
	} else {
	    die ("FAILED formatting end-of-spec\n");
	}
    }

    push (@spec, $last_spec_line);
    my $spec = join("\n", @spec);

    my $returns_params = '';

    if (! $return_added) {
	# separate out explanation of parameters in the Returns data
	my $add_to_spec;
	my $is_spec = 1;
	foreach my $t (split ("\n", $function_data{returns})) {
	    if ($t =~ /=/o) {
		# $t is something like 'Var = type()', not 'Var', goes into returns_params instead of spec
		$is_spec = 0;
	    }

	    if ($function_name eq $want_debug_for_function) {
		warn ("RETURNS LINE : '$t', IS_SPEC $is_spec\n");
	    }

	    # if $t has description, reformat it to new format
	    # e.g. turn
	    #           NewHeader = #keylist{}, request header
	    # into
	    #           NewHeader = #keylist{} "request header"
	    #
	    if ($is_spec) {
		my $t_desc = remove_description (\$t);
		if ($t_desc and ! $delete_description_after_type) {
		    $t .= reformat_description ($t_desc);
		}
		
		$add_to_spec .= "$t\n";
	    } else {
		$returns_params .= "$t\n";
	    }
	}

	if ($function_name eq $want_debug_for_function) {
	    warn ("ADD_TO_SPEC : '$add_to_spec' RETURNS_PARAMS '$returns_params'\n");
	}

				   
	chomp ($add_to_spec);

	# need to move Returns: data to end of @spec
	my $add_to_spec2 = indent ("%%", 12, $add_to_spec);
	my @fixpipe = split ("\n", $add_to_spec2);
	# remove "|" from last row
	my $t2 = pop (@fixpipe);
	$t2 =~ s/\|$//o;
	push (@fixpipe, $t2);
	$add_to_spec2 = join ("\n", @fixpipe);
	$spec .= "$add_to_spec2\n" if ($add_to_spec2);
    }

    my $throws = '';
    if ($function_data{throws}) {
	my $throws_t = join(" |\n", @{$function_data{throws}}) . "|\n";

	if ($function_name eq $want_debug_for_function) {
	    warn ("THROWS : '$throws_t'\n");
	}

        $throws_t = indent_align("%%", 12, "|", $throws_t);

	my @t = split("\n", $throws_t);
	# remove "|" from last row
	my $t2 = pop (@t);
	$t2 =~ s/\|$//o;
	push (@t, $t2);
	# insert "@throws" in first line
	$t[0] =~ s/%%\s{10,10}/%% \@throws/o;

	$throws = "%%\n" . join ("\n", @t);
    }

    #warn ("SPEC :\n$spec\n");

    my $params = indent_align("%%", 12, "=", $function_data{params});
    $params = format_param_spec($params);
    
    my $doc = '';
    my $doc_in = $function_data{descrip} . $function_data{notes};
    if ($doc_in) {
	$doc = reformat_comment ($doc_str, 70, '', $doc_in);
    }

    if ($fix_TIMEOUT_macro) {
	$spec =~ s/\?TIMEOUT/Timeout::integer()/go;
    }
    
    my $function_tag = $function_data{tag} || '';
    #warn ("FUN TAG MOD $module_name FUN $function_name -> " . $global_data{"fun_tag:$module_name:$function_name"} . "\n");
    my $extra_function_tag = $global_data{"fun_tag:$module_name:$function_name"} || $global_data{"fun_tag:$function_name"} || '';
    if ($extra_function_tag) {
	$function_tag .= "%% $extra_function_tag\n";
    }

    if ($returns_params) {
	my $t_rp = indent_align("%%", 12, "=", $returns_params);
	$returns_params = format_param_spec ($t_rp);
    }

    my $t =
	"$spec\n" .
	"%%\n" .
 	"$params\n" .
        "%%\n" .
        "$returns_params\n" .
	"$throws\n" .
	"%%\n" .
	"$doc\n" .
	"$function_tag\n" .
	"%% \@end\n";
    
    # remove empty lines, and make sure we don't have double "%%\n" lines in a row
    my @tt = split ("\n", $t);
    $t = '';
    my @tt_res;
    foreach my $tt2 (@tt) {
	#chomp ($tt2);
	$tt2 =~ s/[\r\n]+$//o;	# instead of chomp which is acting strange on me
	next if ($tt2 =~ /^\s*\n*$/o);
	if ($tt2 =~ /^%%\s*\n*$/o) {
	    # check if last line was also a separator ("%%\n")
	    next if ($tt_res[$#tt_res] =~ /^%%\s*\n*$/o);
	}
	push (@tt_res, $tt2);
    }
    $t = join ("\n", @tt_res) . "\n";
	
    warn ("\nPrint function :\n$t\n---end\n") if ($debug or ($function_name eq $want_debug_for_function));

    print (OUT $t);
}

sub remove_description
{
    my $line_ref = shift;

    $$line_ref =~ s/(.*)atom\(\),\s+(.+\|.+)/$1$2/o;		# turn "atom(), foo | bar" into "foo | bar"
    $$line_ref =~ s/^(.*), see below$/$1/o;			# turn "Anything, see below" into "Anything"
    $$line_ref =~ s/(.*)tuple\(\), (\{.+\|.+\})\s*$/$1$2/o;	# turn "tuple(), {x} | {y} into {x} | {y}

    my $desc = get_description ($$line_ref);

    if ($desc) {
	my $new = substr ($$line_ref, 0, 0 - length ($desc));
	# don't use regexp to remove $desc if it contains regexp special characters (like parentheses)
	$$line_ref = $new;
	$desc_count ++;
    }

    return $desc;
}

sub get_description
{
    my $line = shift;

    return 0 if ($line =~ /atom\(\),\s.+\|.+$/o);		# don't treat "true | false" as descr. in "atom(), true | false"
    return 0 if ($line =~ /^.*, \{\}$/o);			# if line starts ends in curly bracket, we are in a tuple definition
    return 0 if ($line =~ /^.*=\s*\{.*\}$/o);			# if line starts in "= {" and ends in "}" we are in a tuple definition

    return $2 if ($line =~ /^(.*\{\})(,.*)$/o);			# turn "#record{}, description" into "#record{}"
    return $2 if ($line =~ /^(.*\(\))(,.*)$/o);			# turn "type(), description" into "type()"
    return $2 if ($line =~ /^(\{atomic,.*\})(,.*)$/o);		# turn "{atomic, something}, description" into "{atomic, something}"
    return $2 if ($line =~ /^(.*none)(,.*)$/o);			# turn "none, description" into "none"
    return $2 if ($line =~ /^(.*\sfalse)(,.*)$/o);		# turn " false, description" into " false"
    return $2 if ($line =~ /^(.*\|\s*[a-z_]+)(,.+)$/o);		# turn "| some_atom, description" into "| some_atom"
    return $2 if ($line =~ /^(.*undefined)(,.*)$/o);		# turn "undefined, description" into "undefined"
    return $2 if ($line =~ /(=.*\})(,.*)$/o);			# turn "= something {foo}, descrip." into "= something {foo}"
    return $2 if ($line =~ /^(.*\s\[.*\])(,.*)$/o);			# turn "[list], description" into "[list]"

    return 0;
}

sub reformat_description
{
    my $desc = shift;
    $desc =~ s/^\s*,\s*//o;	# remove leading ", "
    $desc =~ s/\"/\\\"/go;	# change " to \"
    return (" \"$desc\"");
}

sub format_param_spec
{
    my $in = shift;

    my @out;

    # reformat "Param = x | y | z" into multiple lines if it is too long

    foreach my $line (split ("\n", $in)) {
	my $desc = remove_description (\$line);

	if (length ($line) >= 70) {
	    if ($line =~ /^(%%.*?=\s)(.*\|.*)$/) {
		# has "=" as well as "|"
		my $first = $1;
		my $or_spec = $2;

		#warn ("OR SPEC : '$or_spec'\n");
		$or_spec =~ s/\|/\n/og;		# split on pipe char is strange
		my @t = split ("\n", $or_spec);

		#warn ("ELEMENTS : @t\n");
		my $longest_element = 0;
		foreach my $elem (@t) {
		    $elem =~ s/\s+$//o;	# remove trailing whitespace
		    $elem =~ s/^\s+//o; # remove leading whitespace
		    my $len = length ($elem);
		    $longest_element = $len if ($len > $longest_element);
		}
		# now pad each element that is shorter than the longest one
		foreach my $elem (@t) {
		    my $len = length ($elem);
		    if ($len < ($longest_element + 1)) {
			my $add = ($longest_element - $len + 1);
			#warn ("ADD $add TO ELEM '$elem' (LEN " . length($elem) . ", MAX $longest_element)\n");
			$elem .= " " x $add;
		    }
		}

		my $new_line = $first;
		my $indent = " " x (length ($first) - 2);

		$new_line .= join ("\|\n%%$indent", @t);
		$new_line =~ s/\s+$//o; # remove trailing whitespace
		
		#warn ("REFORMATTED '$line'\n TO :\n$new_line\n\n");
		push (@out, $new_line);
		next;
	    }
	}

	if ($desc and ! $delete_description_after_type) {
	    #warn ("LINE $line\nDESC : $desc\n\n");
	    $line .= reformat_description ($desc);
	}

	push (@out, $line);
    }

    my $out = join ("\n", @out);

    return $out;
}
