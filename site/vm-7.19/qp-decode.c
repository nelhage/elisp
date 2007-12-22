/* public domain */

/* Quoted Printable on stdin -> converted data on stdout */

#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#ifndef WIN32
#define WIN32
#endif
#endif

#ifdef WIN32
#include <io.h>
#include <fcntl.h>
#endif

char *hexdigits  = "0123456789ABCDEF";
char *hexdigits2 = "0123456789abcdef";

int
main()
{
    char line[2000], *start, *stop, *copy;
    char *d1, *d2, c;
    int lineno;

#ifdef WIN32
    _setmode( _fileno(stdout), _O_BINARY);
#endif

    line[sizeof line - 1] = 0;
    lineno = 1;
    while (fgets(line, sizeof line - 1, stdin)) {
	lineno++;
	start = line;
      keep_processing:
	for (stop = start; *stop && *stop != '=' && *stop != '\n'; stop++)
	  ;
	if (stop != line && *stop == '\n') {
	    copy = stop;
	    do {
		copy--;
		if (*copy != ' ' && *copy != '\t') {
		    copy++;
		    break;
		}
	    } while (copy != line);
	} else {
	    copy = stop;
	}
	while (start != copy) {
	    putchar(*start);
	    start++;
	}
	if (*stop == '\n') {
	    putchar(*stop);
	    lineno++;
	    continue;
	} else if (*stop == 0) {
	    continue;
	} else { /* *stop == '=' */
	    stop++;
	    if ((d1 = strchr(hexdigits, *(stop))) &&
		(d2 = strchr(hexdigits, *(stop+1)))) {
		c = (d1 - hexdigits) * 16 + (d2 - hexdigits);
		putchar(c);
		stop += 2;
	    } else if ((d1 = strchr(hexdigits2, *(stop))) &&
		       (d2 = strchr(hexdigits2, *(stop+1)))) {
		c = (d1 - hexdigits2) * 16 + (d2 - hexdigits2);
		putchar(c);
		stop += 2;
	    } else if (*stop == '\n') {
		/* soft line break */
		stop++;
	    } else if (*stop == '\r') {
		/*
		 * Assume the user's lousy delivery software
		 * didn't convert from Internet's CRLF newline
		 * convention to the local LF convention.
		 */
		stop++;
	    } else if (*stop == ' ' || *stop == '\t') {
	      /* garbage added in transit */
	      for (stop++; *stop && (*stop == ' ' || *stop == '\t'); stop++)
		;
	    } else {
		fprintf(stderr, "line %d: something other than line break or hex digits after = in quoted-printable encoding\n", lineno);
		exit(1);
	    }
	    start = stop;
	    goto keep_processing;
	}
    }
    exit(0);
}
