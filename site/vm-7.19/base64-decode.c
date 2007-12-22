/* public domain */

/* BASE64 on stdin -> converted data on stdout */

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
    static char inalphabet[256], decoder[256];
    int i, bits, c, char_count, errors = 0;

#ifdef WIN32
    _setmode( _fileno(stdout), _O_BINARY);
#endif

    for (i = (sizeof alphabet) - 1; i >= 0 ; i--) {
	inalphabet[alphabet[i]] = 1;
	decoder[alphabet[i]] = i;
    }

    char_count = 0;
    bits = 0;
    while ((c = getchar()) != EOF) {
	if (c == '=')
	  break;
	if (c > 255 || ! inalphabet[c])
	  continue;
	bits += decoder[c];
	char_count++;
	if (char_count == 4) {
	    putchar((bits >> 16));
	    putchar(((bits >> 8) & 0xff));
	    putchar((bits & 0xff));
	    bits = 0;
	    char_count = 0;
	} else {
	    bits <<= 6;
	}
    }
    if (c == EOF) {
	if (char_count) {
	    fprintf(stderr, "base64 encoding incomplete: at least %d bits truncated",
		    ((4 - char_count) * 6));
	    errors++;
	}
    } else { /* c == '=' */
	switch (char_count) {
	  case 1:
	    fprintf(stderr, "base64 encoding incomplete: at least 2 bits missing");
	    errors++;
	    break;
	  case 2:
	    putchar((bits >> 10));
	    break;
	  case 3:
	    putchar((bits >> 16));
	    putchar(((bits >> 8) & 0xff));
	    break;
	}
    }
    exit(errors ? 1 : 0);
}
