VPATH = $(srcdir)

BEAM =	incomingproxy.beam pstnproxy.beam logger.beam util.beam siputil.beam sippacket.beam \
	keylist.beam sipurl.beam siprequest.beam sipheader.beam phone.beam sipauth.beam \
	siphost.beam dnsutil.beam admin_www.beam hex.beam directory.beam eldap.beam \
	LDAPv3.beam appserver.beam sipanswer.beam rtp.beam sdp.beam dtmf.beam sound.beam \
	group_regexp.beam sipclient.beam database_call.beam sipserver.beam \
	database_regexproute.beam sipproxy.beam gssapi.beam lookup.beam local.beam \
	testserver.beam siptimer.beam transactionstatelist.beam targetlist.beam \
	clienttransaction.beam database_forward.beam siplocation.beam \
	sipuserdb.beam sipuserdb_mnesia.beam bootstrap.beam sipuserdb_ldap.beam \
	sipsocket.beam sipsocket_tcp.beam sipsocket_udp.beam socketlist.beam \
	transactionlayer.beam servertransaction.beam sippipe.beam \
	transportlayer.beam tcp_dispatcher.beam tcp_listener.beam \
	tcp_connection.beam tcp_receiver.beam registrar.beam sipserver_sup.beam

CC = gcc

.PRECIOUS: %.boot %.config

all: $(BEAM) $(STARTSCRIPT) bootstrap.sh

clean:
	rm -f *.beam *.boot *.app *.rel *~ *.script *.start bootstrap.sh

sslkey:
	mkdir -p $(sslcertdir)
	chmod 700 $(sslcertdir)
	test -f $(sslcertdir)/ssl.config || cp $(srcdir)/ssl.config $(sslcertdir)/ssl.config
	cd $(sslcertdir) && openssl req -days 2002 -new -text -out cert.req -config ./ssl.config
	cd $(sslcertdir) && openssl rsa -in privkey.pem -out cert.pem -passin pass:foobar
	cd $(sslcertdir) && openssl req -days 2002 -x509 -in cert.req -text -key cert.pem -out cert.cert
	cat $(sslcertdir)/cert.cert $(sslcertdir)/cert.pem > $(sslcertdir)/cert.comb

%.start: %.boot %.config init.sh.in
	sed -e 's!@PROGRAMNAME@!$*!' \
	    -e 's!@CONFIGDIR@!$(configdir)!' \
	    -e 's!@SSLCERTDIR@!$(sslcertdir)!' \
	    -e 's!@MNESIADIR@!$(mnesiadir)!' \
	    -e 's!@BUILDDIR@!$(builddir)!' < $(srcdir)/init.sh.in > $@.new
	chmod +x $@.new
	mv $@.new $@

%.config:
	test -f $@ || ( echo "% Write your configuration here"; echo "[{$*, []}]." ) > $@

%.app: %.app.in
	cp $< $@

%.rel: %.rel.in %.app
	cp $< $@

%.beam: %.erl
	erlc -W +debug_info $<

%.boot %.script: %.rel
	erl -noshell -run systools make_script $* -run init stop

dtmfserver: dtmfserver.o
	$(CC) $(LDFLAGS) -o dtmfserver dtmfserver.o

bootstrap.sh: init.sh.in
	sed -e 's!@PROGRAMNAME@!$*!' \
	    -e 's!@CONFIGDIR@!$(configdir)!' \
	    -e 's!@SSLCERTDIR@!$(sslcertdir)!' \
	    -e 's!@MNESIADIR@!$(mnesiadir)!' \
	    -e 's!@BUILDDIR@!$(builddir)!' < $(srcdir)/init.sh.in > $@.new
	chmod +x $@.new
	mv $@.new $@
