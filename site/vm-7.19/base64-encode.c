/* public domain */

/*
 * arbitrary data on stdin -> BASE64 data on stdout
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

unsigned char alphabet[64] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int
main()
{
    int cols, bits, c, char_count;

#ifdef WIN32
    _setmode( _fileno(stdin), _O_BINARY);
#endif

    char_count = 0;
    bits = 0;
    cols = 0;
    while ((c = getchar()) != EOF) {
	if (c > 255) {
	    fprintf(stderr, "encountered char > 255 (decimal %d)", c);
	    exit(1);
	}
	bits += c;
	char_count++;
	if (char_count == 3) {
	    putchar(alphabet[bits >> 18]);
	    putchar(alphabet[(bits >> 12) & 0x3f]);
	    putchar(alphabet[(bits >> 6) & 0x3f]);
	    putchar(alphabet[bits & 0x3f]);
	    cols += 4;
	    if (cols == 72) {
		putchar('\n');
		cols = 0;
	    }
	    bits = 0;
	    char_count = 0;
	} else {
	    bits <<= 8;
	}
    }
    if (char_count != 0) {
	bits <<= 16 - (8 * char_count);
	putchar(alphabet[bits >> 18]);
	putchar(alphabet[(bits >> 12) & 0x3f]);
	if (char_count == 1) {
	    putchar('=');
	    putchar('=');
	} else {
	    putchar(alphabet[(bits >> 6) & 0x3f]);
	    putchar('=');
	}
	if (cols > 0)
	  putchar('\n');
    }

    exit(0);
}
