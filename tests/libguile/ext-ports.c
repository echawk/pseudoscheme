/* A port type written in C, built against GNU Guile 3.0.11's headers:
   reading gives the bytes of a C string (the port's stream, through
   SCM_STREAM); writing collects bytes, upper-cased, in a C buffer.
   tests/libguile/ports.scm uses it.  */

#include <libguile.h>
#include <string.h>

struct source { const char *text; size_t pos; };

static scm_t_port_type *source_type, *sink_type;
static char sink_buffer[4096];
static size_t sink_length = 0;
static int closes = 0;

static size_t
source_read (SCM port, SCM dst, size_t start, size_t count)
{
  struct source *s = (struct source *) SCM_STREAM (port);
  size_t left = strlen (s->text) - s->pos;
  size_t n = count < left ? count : left;
  memcpy (SCM_BYTEVECTOR_CONTENTS (dst) + start, s->text + s->pos, n);
  s->pos += n;
  return n;
}

static size_t
sink_write (SCM port, SCM src, size_t start, size_t count)
{
  (void) port;
  const unsigned char *p = (const unsigned char *) SCM_BYTEVECTOR_CONTENTS (src) + start;
  for (size_t i = 0; i < count && sink_length < sizeof sink_buffer - 1; i++)
    {
      unsigned char c = p[i];
      sink_buffer[sink_length++] = (c >= 'a' && c <= 'z') ? c - 32 : c;
    }
  return count;
}

static void
sink_close (SCM port)
{
  (void) port;
  closes++;
}

static SCM
open_source (SCM text)
{
  struct source *s = scm_gc_malloc (sizeof *s, "source");
  s->text = scm_to_utf8_string (text);
  s->pos = 0;
  return scm_c_make_port (source_type, SCM_RDNG, (scm_t_bits) s);
}

static SCM
open_sink (void)
{
  return scm_c_make_port (sink_type, SCM_WRTNG, 0);
}

static SCM
sink_contents (void)
{
  return scm_from_utf8_stringn (sink_buffer, sink_length);
}

static SCM
sink_closes (void)
{
  return scm_from_int (closes);
}

/* reading a port from C */
static SCM
read_from_c (SCM port, SCM n)
{
  char buf[256];
  size_t got = scm_c_read (port, buf, scm_to_size_t (n));
  return scm_from_utf8_stringn (buf, got);
}

void
init_ports (void)
{
  source_type = scm_make_port_type ("source", source_read, NULL);
  sink_type = scm_make_port_type ("sink", NULL, sink_write);
  scm_set_port_close (sink_type, sink_close);
  scm_c_define_gsubr ("open-source", 1, 0, 0, open_source);
  scm_c_define_gsubr ("open-sink", 0, 0, 0, open_sink);
  scm_c_define_gsubr ("sink-contents", 0, 0, 0, sink_contents);
  scm_c_define_gsubr ("sink-closes", 0, 0, 0, sink_closes);
  scm_c_define_gsubr ("read-from-c", 2, 0, 0, read_from_c);
}
