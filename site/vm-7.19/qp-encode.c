/* public domain */

/*
 * arbitrary data on stdin -> Quoted-Printable data on stdout
 *
 * UNIX's newline convention is used, i.e. one ASCII control-j (10 decimal).
 */

#include <stdio.h>

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

int
main()
{
    int c;
    int cols = 0;

#ifdef WIN32
    _setmode( _fileno(stdout), _O_BINARY);
#endif

    while ((c = getchar()) != EOF) {
	if (c == '\n') {
	    putchar(c);
	    cols = 0;
	} else if (c == ' ') {
	    int nextc;
	    nextc = getchar();
	    if (nextc != '\n' && nextc != EOF) {
		putchar(c);
		cols++;
	    } else {
		putchar('=');
		putchar(hexdigits[c >> 4]);
		putchar(hexdigits[c & 0xf]);
		cols += 3;
	    }
	    if (nextc != EOF)
	      ungetc(nextc, stdin);
	} else if (c < 33 || c > 126 || c == '=' ||
		   /* these are for RFC 2047 Q encoding */
		   c == '?' || c == '_') {
	    putchar('=');
	    putchar(hexdigits[c >> 4]);
	    putchar(hexdigits[c & 0xf]);
	    cols += 3;
	} else if (c == '.' && cols == 0) {
	    int nextc;
	    nextc = getchar();
	    if (nextc == EOF || nextc == '\n') {
		putchar('=');
		putchar(hexdigits[c >> 4]);
		putchar(hexdigits[c & 0xf]);
		cols += 3;
	    } else {
		putchar(c);
		cols++;
	    }
	    if (nextc != EOF)
	      ungetc(nextc, stdin);
	} else {
	    putchar(c);
	    cols++;
	}
	if (cols > 70) {
	    putchar('=');
	    putchar('\n');
	    cols = 0;
	}
    }
    exit(0);
}
