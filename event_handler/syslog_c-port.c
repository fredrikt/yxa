/* Small port driver to log messages to syslog. The advantage of doing
 * this with a port driver is that you don't need a syslog server.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <string.h>

int
nread(unsigned char *buf, unsigned int remaining, unsigned int maxlen)
{
  unsigned char *c = buf;
  unsigned int read_bytes = 0;

  if (remaining > maxlen)
    return -1;

  while (remaining > 0) {
    if ((read_bytes = read (0, c, remaining)) > 0) {
      c += read_bytes;
      remaining -= read_bytes;
    } else
      return -1;
  }

  return read_bytes;
}

int
read_msg(unsigned char *buf, unsigned int maxlen)
{
  unsigned int len;

  if (nread (buf, 2, maxlen) != 2) {
    return -1;
  }

  len = (buf[0] << 8) | buf[1];

  return nread (buf, len, maxlen);
}

int
write_msg(unsigned char *buf, unsigned int len)
{
  unsigned char c;

  /* output length */
  c = (len >> 8) & 0xff;
  if (write (1, &c, 1) != 1) {
    return -1;
  }
  c = len & 0xff;
  if (write (1, &c, 1) != 1) {
    return -1;
  }

  /* write buffer */
  if (write (1, buf, len) != len) {
    return -1;
  }

  return 0;
}

int
main()
{
  unsigned char buf[1024];
  unsigned char *myname;
  int len;

  /* read application name, will be the first message passed to us. */
  len = read_msg(buf, sizeof (buf));
  if (len < 3 || len >= sizeof (buf)) {
    /* 3 is chosen arbitrarily, no application would have a shorter name
     * than that
     */
    write_msg ("Could not get identification string", 35);
    exit (1);
  }
  buf[len] = 0;

  len = strlen (buf) + 4;
  if ((myname = (unsigned char *) malloc (len)) == NULL) {
    snprintf (buf, sizeof (buf), "Failed allocating %i bytes of memory for the identification string\n", len);
    buf[sizeof (buf) - 1] = 0;
    write_msg (buf, strlen (buf));
    exit (1);
  }
  strcpy (myname, "yxa/");
  strcpy (myname + 4, buf);

  openlog (myname, LOG_PID, LOG_USER);

  while ((len = read_msg (buf, sizeof (buf))) > 0) {
    int t = sizeof (buf);
    int prio = LOG_DEBUG;

    /* null terminate */
    if (len < t)
      t = len;
    buf[t] = 0;

    /* figure out priority, it is the first char in buf */
    switch (*buf) {
    case 'i':
      prio = LOG_INFO;
    case 'e':
      prio = LOG_ERR;
    }

    syslog (prio, "%s", buf + 1);
    
    /* 'clear' buf in case we just happen to not really read another message 
     * (clear both the prio byte and the first data byte)
     */
    buf[0] = 0;
    buf[1] = 0;
  }

  write_msg ("Failed reading message", 22);
  printf ("NOT REACHED\n");
  /* NOT REACHED */
  exit (0);
}
