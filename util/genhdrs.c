/*
 * C compiler support file genhdrs.c
 * Copyright (C) Codemist Limited, 1987, 1989.
 * Copyright (C) Acorn Computers Limited., 1988
 * Copyright (C) Advanced RISC Machines Limited 1992-93.
 * SPDX-Licence-Identifier: Apache-2.0
 * All rights reserved.
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/*
 * This file is a program that is run in order to create a file called
 * headers.c which is part of the source of the compiler.  It also creates
 * errors.h.  To do its work it needs prototype files miperrs.h, feerrs.h &
 * mcerrs.h, and access to directories containing the standard headers,
 * such as stdio.h etc.  The object of having this utility is twofold -
 * first it arranges for standard headers to be built into an in-store
 * mini file system so that #including them can be fast, then it organises
 * some data compression for both builtin headers and the text of error
 * messages.
 *
 * Call this program with
 *
 * Genhdrs -o headers.c -i <dir> <files>         or
 * Genhdrs -e errors.h -q miperrs.h -q mcerrs.h -q mcerrs.h [-n] [-s]
 *
 * where the <files> at the end will be a list of names of files from the
 * given directory that are to be included as built-in headers.
 * If the list of files is too long to fit on the line it is possible to go
 *         -v <via-file>
 * where the via-file contains a list.  If -v is specified separate files
 * may not be given as well.
 *
 * Flag -n causes error messages to get mapped to numbers
 * Flag -s causes error messages to be compressed
 *
 * On an Acorn ARM a certain version of the compiler was 323304 bytes
 * in size with no compression of error messages.  The savings by selecting
 * various of the above options were (where -t was a further option to
 * compress error strings in with the built-in headers, which option has
 * now been removed):
 *
 *      -n          4476
 *      -s          4496
 *      -n -s       8972    (about 2.7%)
 *      -t          2276
 *      -n -t       6752
 *
 * It seems that -n -s will be useful for production code, -s reasonably
 * painless for most system developers (not using -n because system
 * developers may expect to see syserrs from time to time), and no
 * compression being the mode of choice when doing cross-development
 * for a new machine, especially one with a different character set.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <time.h>

#define  SELF           "genhdrs"

/*
 * The following sets ArgvType for 64-bit pointers so that
 * DEC Unix (OSF) cc can be used with the -xtaso-short compiler option
 * to force pointers to be 32-bit.
 */
#if defined(__alpha) && defined(__osf__)
#pragma pointer_size (save)
#pragma pointer_size (long)
#endif
typedef char *ArgvType;
#if defined(__alpha) && defined(__osf__)
#pragma pointer_size (restore)
#endif

#define  MAXPATHS        40
#define  MAXFILENAMESZ  256
#define  MAXSTRINGS     1000       /* How many strings can be stored here */
#define  MAXCHARS       100000     /* Total length of strings to be processed */

static int number_msgs  = 0;
static int emit_tagdirs = 0;    /* whether to emit -TAGS- and -NOT TAGS- messages*/
static int squeeze_msgs = 0;
static int safe_compress= 1;

static int lastchar = 0;
static int npaths = 0;
static int nerrors = 0;
static char *paths[MAXPATHS];
static int nepaths = 0;
static char *epaths[MAXPATHS];
static char hdrname[40][MAXPATHS];
static char file_name[MAXFILENAMESZ];
static unsigned char *strings[MAXSTRINGS];
static int stringlength[MAXSTRINGS];
static unsigned short compression[256], compression_depth[256];
static unsigned char *chardata = NULL;
static int *digraph_counts = NULL;
static int stringcount = 0;
static int msgnumber = 0;
static int charpoint = 0;

/* The following are states in a machine that detects & deletes comments */
#define NORMAL   0x0
#define STRING   0x1
#define CHAR     0x2
#define STRBACK  0x3
#define CHARBACK 0x4
#define SLASH    0x5
#define COMMENT  0x6
#define STAR     0x7
#define STREND   0x8
#define STRSTART 0x9


static FILE *path_open(char *name, char *mode)
{   FILE *f;
    int j;

    for (j = 0;  j < npaths;  ++j)
    {   strcpy(file_name, paths[j]);
        strcat(file_name, name);
        f = fopen(file_name, mode);
        if (f != NULL) return f;
    }
    return NULL;
}

static void outch(int ch)
{   if (charpoint >= MAXCHARS)
    {   charpoint = 0;
        fprintf(stderr, "%s: Too many chars (%u permitted)\n", SELF, MAXCHARS);
        ++nerrors;
    }
    else
        chardata[charpoint++] = (ch);
}

static void copy_header(char *name)
{
    FILE *fh1;
    int ch, state = NORMAL;
    char ansi_name[32];

    fh1 = path_open(name, "r");
    if (fh1==NULL)
    {   fprintf(stderr, "Unable to read input file %s\n", name);
        ++nerrors;
        return;
    }
    if ((name[0] == 'h' || name[0] == 'H') && name[1] == '.')
    {   strcpy(ansi_name, name+2);
        strcat(ansi_name, ".h");
    }
    else if(name[0] == ':') /* Macintosh filenames can start with : */
        strcpy(ansi_name, name+1);
    else
        strcpy(ansi_name, name);

    fprintf(stderr, "%s: Copying %s as %s\n", SELF, file_name, ansi_name);
    strcpy(hdrname[stringcount], ansi_name);
    if (stringcount >= MAXSTRINGS)
    {   fprintf(stderr, "%s: Too many strings (%u allowed)\n", SELF, MAXSTRINGS);
        stringcount = 0;
        ++nerrors;
        return;
    }
    strings[stringcount++] = &chardata[charpoint];

    lastchar = '\n';

    ch = getc(fh1);
    while (ch!=EOF)
    {
        switch (state)
        {
    case NORMAL:
            switch (ch)
            {
        case '\t':
        case ' ':   ch = getc(fh1);
                    if (ch != ' ') outch(' ');
                    continue;
        case '\'':  outch(ch);  state = CHAR;   break;
        case '\"':  outch(ch);  state = STRING; break;
        case '/':               state = SLASH;  break;
        default:    outch(ch);                  break;
            }
            break;
    case STRING:
            switch (ch)
            {
        case '\"':  outch(ch);  state = NORMAL; break;
        case '\\':  outch(ch);  state = STRBACK;break;
        default:    outch(ch);                  break;
            }
            break;
    case CHAR:
            switch (ch)
            {
        case '\'':  outch(ch);  state = NORMAL; break;
        case '\\':  outch(ch);  state = CHARBACK;break;
        default:    outch(ch);                  break;
            }
            break;
    case STRBACK:
            outch(ch);
            state = STRING;
            break;
    case CHARBACK:
            outch(ch);
            state = CHAR;
            break;
    case SLASH:
            switch (ch)
            {
        case '*':               state = COMMENT;break;
        case '\'':  outch('/');
                    outch(ch);  state = CHAR;   break;
        case '\"':  outch('/');
                    outch(ch);  state = STRING; break;
        default:    outch('/');
                    outch(ch);  state = NORMAL; break;
            }
            break;
    case COMMENT:
            if (ch == '*') state = STAR;
            break;
    case STAR:
            switch (ch)
            {
        case '/':               state = NORMAL; break;
        case '*':                               break;
        default:                state = COMMENT;break;
            }
            break;
    default:
            fprintf(stderr, "\nBad state %d\n", state);
            ++nerrors;
            return;
        }
        ch = getc(fh1);
    }
    if (state!=NORMAL || lastchar!='\n')
    {
        fprintf(stderr, "\nUnexpected end of file in %s?\n", file_name);
        ++nerrors;
    }
    outch(0);
    while (charpoint & 3) outch(0);
    fclose(fh1);
}

static void decompress_char(int c)
{
    int z = compression[c];
    if (c == z)
    {   if (c == '\n') c = 'n', putc('\\', stderr);
        else if (c == 0) c = '0', putc('\\', stderr);
        putc(c, stderr);
    }
    else
    {   decompress_char((z >> 8) & 0xff);
        decompress_char(z & 0xff);
    }
}

static int compress_strings(int emsg, int *digraph_counts, char *force_used)
/*
 * value returned is greatest stack depth needed during decompression.
 * A nonzero arg => treat strings as error messages and try to preserve
 * %? and $? escape sequences in them.
 */
{
    int i, nsquashed = 0, stackdepth_needed = 0, worst_char = 0;
    int total_bytes = 0;

    for (i = 0;  i < 256;  i++) compression[i] = compression_depth[i] = 0;

/*
 * Identify the characters that are in use in the given set of strings.
 */
    if (safe_compress && force_used != 0)
        for (i = force_used[0];  i != 0;  i = *(++force_used))
            compression[i & 0xff] = i;

    for (i=0; i<stringcount; i++)
    {   unsigned char *s = strings[i];
        int l = stringlength[i] = 1 + strlen((char *)s);
        int k;
        total_bytes += l;
        for (k=0; k<l; k++) compression[s[k] & 0xff] = s[k];
    }
    fprintf(stderr, "%d bytes of strings found\n", total_bytes);

    for (i=1; i<256 && stackdepth_needed < 10; i++)
    {   int z = compression[i];
        int j, best, bestj;
        if (z == i) continue;   /* Char code already in use */
        for (j=0; j<256*256; j++) digraph_counts[j] = 0;
        for (j=0; j<stringcount; j++)
        {   unsigned char *s = strings[j];
            int l = stringlength[j];
            int k, inesc = 0;
            for (k=1; k<l; k++)
            {   int a = s[k-1] & 0xff, b = s[k] & 0xff;
                if (!emsg) digraph_counts[256*a+b]++;
                else switch (inesc)
                {
        case 0:     switch (a)
                    {
            case '%':   inesc = 1;
                        continue;
            case '$':   inesc = 2;
                        continue;
            default:    if (b != '%' && b != '$' && b != 0)
                            digraph_counts[256*a+b]++;
                        continue;
                    }
        case 1:     switch (tolower(a))
                    {
            case '%': case 'c': case 'd': case 'e': case 'f': case 'g':
            case 'i': case 'n': case 'o': case 'p': case 's': case 'u':
            case 'x':   inesc = 0;  /* Drop through */
            default:    continue;
                    }
        default:    inesc = 0;
                    continue;
                }
            }
        }
        best = bestj = -1;
        for (j=0; j<256*256; j++)
            if (digraph_counts[j] > best) best = digraph_counts[j], bestj = j;
        if (best <= 1) break;   /* Compression finished! */
#ifdef VERBOSE_MODE
        fprintf(stderr, "%.4x -> %.2x  ", bestj, i);
#else
        putc('.', stderr);
        if (++nsquashed == 64) putc('\n', stderr), nsquashed = 0;
#endif
        compression[i] = bestj;
        {   int l1 = compression_depth[bestj & 0xff];
            int l2 = compression_depth[bestj >> 8];
            int l = 1 + l2;
            if (l1 > l) l = l1;
            compression_depth[i] = l;
            if (l > stackdepth_needed)
            {   stackdepth_needed = l;
                worst_char = i;
            }
        }
        for (j=0; j<=stringcount; j++)
        {   unsigned char *s = strings[j];
            int l = stringlength[j];
            int ba = bestj>>8, bb = bestj & 0xff;
            int k, inesc = 0;
            for (k=1; k<l; k++)
            {   int a = s[k-1] & 0xff, b = s[k] & 0xff;
/*
 * I only combine characters if so doing would not disrupt an escape
 * sequence.
 */
                if (!emsg) goto combine;
                else switch (inesc)
                {
        case 0:     switch (a)
                    {
            case '%':   inesc = 1;
                        continue;
            case '$':   inesc = 2;
                        continue;
            default:    if (b != '%' && b != '$' && b != 0) goto combine;
                    }
        case 1:     switch (tolower(a))
                    {
            case '%': case 'c': case 'd': case 'e': case 'f': case 'g':
            case 'i': case 'n': case 'o': case 'p': case 's': case 'u':
            case 'x':   inesc = 0;  /* Drop through */
            default:    continue;
                    }
        default:    inesc = 0;
                    continue;
                }
        combine:
                if (a == ba && b == bb)
                {   s[k-1] = i;
                    /* was: memmove(&s[k], &s[k+1], l-k-1); */
                    {
                        int n = l-k-1;
                        unsigned char *a = &s[k];
                        unsigned char *b = &s[k+1];

                        for (; n-- > 0;) *a++ = *b++;
                    }

                    stringlength[j] = l = l-1;
                    total_bytes--;
                }
            }
        }
    }
    fprintf(stderr,
            "\nNeed %d bytes of stack in decompression for char %.2x (",
            stackdepth_needed, worst_char);
    decompress_char(worst_char);
    fprintf(stderr, ")\nsize = %d now\n", total_bytes);
    return stackdepth_needed;
}

static int print_squashed_msg(unsigned char *s, int l, FILE *fe, int filecol)
{
    int c, i;
    putc('\"', fe);  filecol++;
    for(i=0; i<l; i++)
    {   switch (c = s[i])
        {
    case '\n':
            c = 'n';        /* Drop through */
    case '\"': case '\\':
            break;
    case 0: if (i == l-1) continue;
    default:
            if (filecol > 70 && !isspace(c))
            {   putc('\\', fe);
                putc('\n', fe);
                filecol = 0;
            }
            if (
#ifdef macintosh
                /* some Mac compilers admit that some high-bit set chars are printable */
                /* and some others don't define isprint above 0x7f (which is non-ANSI) */
                (c & ~0x7f) == 0 &&
#endif
                isprint(c)) putc(c, fe), filecol++;
/*
 * If I need to print an (octal) escape I always show 3 digits to avoid
 * confusion if the next character happens to be numeric.
 */
            else fprintf(fe, "\\%.3o", c & 0xff), filecol+=4;
            continue;
        }
        putc('\\', fe);
        putc(c, fe), filecol+=2;
    }
    putc('\"', fe), filecol++;
    return filecol;
}

#define MAP         0
#define NUMBER      1
#define LEAVE       2

static void scan_msg_file(FILE *fq)
{
/*
 * This is only called if I am squeezing error strings.
 * In MAP mode I will collect strings so that the
 * compression scheme has some data to work on.
 */
    int prevch = -1, ch, ch1, state = NORMAL;
    int action = MAP;

    while ((ch1 = ch = getc(fq)) != EOF)
    {
        if (state == NORMAL && prevch == '\n' && ch == '%')
        {   ch1 = ch = getc(fq);
            switch (tolower(ch))
            {
    case 'o':
                action = MAP; break;
    case 's':
    case 'z':   action = LEAVE;  break;
    default:    fprintf(stderr, "\n%%%c unrecognised\n", ch);
                break;
            }
            continue;
        }
        if (state == NORMAL && prevch == '/' && ch == '*')
            state = COMMENT;
        else if (state == COMMENT && prevch == '*' && ch == '/')
            state = NORMAL;
        else if (state == NORMAL && ch == '\"')
        {   state = STRSTART;
            switch (action)
            {
        case MAP:
                if (stringcount >= MAXSTRINGS)
                {   fprintf(stderr,
                      "%s: Too many strings (%u allowed)\n", SELF, MAXSTRINGS);
                    stringcount = 0;
                    ++nerrors;
                    return;
                }
                strings[stringcount++] = &chardata[charpoint];
                break;
        default:
                break;
            }
        }
        else if (state == STRING)
        {   for (;;)
            {   if (ch == '\\')
                {   switch (ch1 = ch = getc(fq))
                    {
            case '\n':
                        ch1 = ch = getc(fq);
                        continue;
            case 'n': case 'N':
                        ch1 = '\n';
                        break;
            case 'a': case 'A':
            case 'b': case 'B':
            case 'f': case 'F':
            case 'r': case 'R':
            case 't': case 'T':
            case 'v': case 'V':
            case 'x': case 'X':
            case '0':   fprintf(stderr,
                                "Bad escape found in header prototype\n");

            default:
                        break;
                    }
                }
                else if (ch == '\"') state = STREND;
                break;
            }
        }
        if (action == MAP && state == STRING) outch(ch1);
        if (state == STREND)
        {   state = NORMAL;
            switch (action)
            {
        case MAP:
                outch(0);
                while (charpoint & 3) outch(0);
                break;
        default:
                break;
            }
        }
        if (state == STRSTART) state = STRING;
        prevch = ch;
    }
}

static void copy_msg_file(FILE *fe, FILE *fq, FILE *ft)
{
    int prevch = -1, ch, state = NORMAL;
    int action = squeeze_msgs ? MAP : LEAVE;
    int efilecol = 0, tfilecol = 0;

/* NB: number_msgs <=> ft != NULL */

    while ((ch = getc(fq)) != EOF)
    {
        if (state == NORMAL && prevch == '\n' && ch == '%')
        {   ch = getc(fq);
            switch (tolower(ch))
            {
    case 'o':   if (emit_tagdirs) fprintf(fe, "/*-TAGS-*/\n");
                action = squeeze_msgs ? MAP : LEAVE;
                break;
    case 's':   if (emit_tagdirs) fprintf(fe, "/*-NOT TAGS-*/\n");
                action = LEAVE;
                break;
    case 'z':   if (emit_tagdirs) fprintf(fe, "/*-TAGS-*/\n");
                action = LEAVE;
                break;
    default:    fprintf(stderr, "\n%%%c unrecognised\n", ch);
                break;
            }
            continue;
        }
        if (state == NORMAL && prevch == '/' && ch == '*')
            state = COMMENT;
        else if (state == COMMENT && prevch == '*' && ch == '/')
            state = NORMAL;
        else if (state == NORMAL && ch == '\"')
        {   state = STRSTART;
            if (number_msgs)
            {   ++msgnumber;
                efilecol += fprintf(fe, "%d /* ", msgnumber);
                tfilecol += fprintf(ft, "/* %3d */ ", msgnumber);
            }
            if (action == MAP)
            {   if (stringcount >= MAXSTRINGS) stringcount = 0;
                if (number_msgs)
                    tfilecol = print_squashed_msg(strings[stringcount],
                        stringlength[stringcount], ft, tfilecol);
                else
                {   efilecol = print_squashed_msg(strings[stringcount],
                        stringlength[stringcount], fe, efilecol);
/*
 * I wanted to put a newline here to improve the format of the converted
 * error file - but that caused confusion in places where I had line
 * continuation in the header prototype files.  Hence I will end up
 * with longer lines here than I like.
 */
                    efilecol+=fprintf(fe, "    /* ");
                }
                stringcount++;
            }
        }
        else if (state == STRING)
        {   for (;;)
            {   if (ch == '\\')
                {   switch (ch = getc(fq))
                    {
            case '\n':
                        putc('\\', fe), putc(ch, fe), efilecol = 0;
                        if (ft != NULL)
                            putc('\\', ft), putc(ch, ft), tfilecol = 0;
                        ch = getc(fq);
                        continue;
            case 'n': case 'N':
                        putc('\\', fe), efilecol++;
                        if (ft != NULL) putc('\\', ft), tfilecol++;
                        break;
            case 'a': case 'A':
            case 'b': case 'B':
            case 'f': case 'F':
            case 'r': case 'R':
            case 't': case 'T':
            case 'v': case 'V':
            case 'x': case 'X':
            case '0':   fprintf(stderr,
                        "%s: Bad escape found in header prototype\n", SELF);

            default:
                        putc('\\', fe), efilecol++;
                        if (ft != NULL)
                            putc('\\', ft), tfilecol++;
                        break;
                    }
                }
                else if (ch == '\"') state = STREND;
                break;
            }
        }
        putc(ch, fe);
        if (ft != NULL && action != MAP &&
            (state == STRSTART || state == STRING || state == STREND))
            putc(ch, ft), tfilecol++;
        if (ch == '\n')
            efilecol = tfilecol = 0;
        else
            efilecol++;
        if (state == STREND)
        {   state = NORMAL;
            if (action==MAP || number_msgs)
            {   efilecol += fprintf(fe, " */");
                if (ft != NULL) putc('\n', ft);
            }
        }
        if (state == STRSTART) state = STRING;
        prevch = ch;
    }
}

static int make_headers(char *headers, int argc, ArgvType *argv, char *viafile)
{   FILE *fh;
    int j, n_files = 0;

    fh = fopen(headers, "w");
    if (fh == NULL)
    {   fprintf(stderr, "%s: Unable to create output file %s\n", SELF, headers);
        return 1;
    }

    fprintf(stderr, "%s: Creating %s...\n", SELF, headers);

    {   time_t t0 = time(NULL);
        struct tm *tm = localtime(&t0);
        fprintf(fh, "/*\n");
        fprintf(fh, " * Copyright (C) Advanced RISC Machines Limited and\n");
        fprintf(fh, " *               Codemist Limited, 19%d.\n",
            tm->tm_year);
        fprintf(fh, " * All rights reserved.\n *\n");
        fprintf(fh, " * %s, created by %s on %s */\n\n",
            headers, SELF, ctime(&t0));
    }

    if (viafile)
    {   FILE *vv = fopen(viafile, "r");
        if (vv == NULL)
            fprintf(stderr, "File %s could not be opened\n", viafile);
        else
        {   char fname[64];
            int rc;
/* fscanf is magic here - it strips away whitespace just the way I want */
            while ((rc = fscanf(vv, "%s", fname)) == 1)
            {   copy_header(fname);
                ++n_files;
            }
            fclose(vv);
        }
    }
    else
    {   char *arg;
        for (j = 1;  j < argc;  ++j)
        {   arg = argv[j];
            if (arg[0] == '-')
            {   int flag;
                if (arg[2] == 0 &&
                    (flag = tolower(arg[1])) != 'n' && flag != 's') ++j;
               continue;
            }
            copy_header(arg);
            ++n_files;
        }
    }

    if (nerrors == 0)
    {   int sk =  compress_strings(0, digraph_counts, (char *)NULL);
        int where, n;

        if (n_files == 0)
        {   fprintf(fh, "#ifndef NO_INSTORE_FILES\n");
            fprintf(fh, "#  define NO_INSTORE_FILES 1\n");
            fprintf(fh, "#endif\n");
        }
        else
        {   fprintf(fh, "static const unsigned short int compression_info[256] = {");
            for (j=0; j<256; j++)
            {   if ((j & 0x7) == 0) fprintf(fh, "\n    ");
                fprintf(fh, "0x%.4x", compression[j]);
                if (j == 255) fprintf(fh, "};\n\n");
                else fprintf(fh, ", ");
            }


            fprintf(fh, "typedef struct {\n");
            fprintf(fh, "    const char *name;\n");
            fprintf(fh, "    int content;\n");
            fprintf(fh, "} header_files;\n");


            fprintf(fh, "\nstatic const header_files builtin_headers[] = {\n");

            where = 0;
            for (j=0;  j < stringcount;  j++)
            {   fprintf(fh, "   {\"%s\", %d},\n", hdrname[j], where);
                where += stringlength[j];
            }
            fprintf(fh,"   {0, 0}};\n\n");

            fprintf(fh, "static const unsigned char string_data[] = {");
            n = 0;
            for (j = 0;  j < stringcount;  j++)
            {   unsigned char *s = strings[j];
                int l = stringlength[j], w;

                for (w = 0;  w < l;  ++w)
                {   if ((n & 0xf) == 0) fprintf(fh, "\n    ");
                    fprintf(fh, "%3d", s[w]);
                    n++;
                    if (n < where)
                    {   putc(',', fh);
                        continue;
                    }
                    else
                    {   fprintf(fh, "};\n\n");
                        break;
                    }
                }
            }
        }
        sk += 1;    /* Add 1 for safety */
        fprintf(fh, "#define HDRSTACKDEPTH  %d\n\n", sk);
        fprintf(fh,"/* end of %s */\n", headers);
        fprintf(stderr, "%s: %s created successfully.\n", SELF, headers);
    }
    else
    {   fclose(fh);
        fh = fopen(headers, "w");  /* to truncate it to zero length */
        fprintf(fh, "#error \"%s is empty because of %s errors.\"\n",
            headers, SELF);
        fprintf(stderr, "\n%s: %s is junk because of errors.\n\n",
            SELF, headers);
    }
    fclose(fh);

    return nerrors != 0;
}

static int make_messages(char *messages, int squeeze_msgs, char *text)
{   FILE *fe, *ft;
    int j, esk = 0;

    fe = fopen(messages, "w");
    if (fe == NULL)
    {   fprintf(stderr, "%s: Unable to create output file %s\n",
            SELF, messages);
        return 1;
    }

    if (text != NULL)
    {   ft = fopen(text, "w");
        if (ft == NULL)
        {   fprintf(stderr, "%s: Unable to create output file %s\n",
                SELF, messages);
            fclose(fe);
            return 1;
        }
    }
    else
        ft = NULL;

    fprintf(stderr, "%s: Creating %s...\n", SELF, messages);

    if (squeeze_msgs)
    {   /* Here a pass over the error files is needed to collect */
        /* information for the compression process               */
        for (j = 0;  j < nepaths;  j++)
        {   FILE *fq = fopen(epaths[j], "r");
            if (fq == NULL)
            {   fprintf(stderr,
                    "%s: Unable to open error prototype file %s\n",
                    SELF, epaths[j]);
                ++nerrors;
                continue;
            }
            fprintf(stderr, "Scanning error file %s\n", epaths[j]);
            scan_msg_file(fq);
            fclose(fq);
        }
        /* Preclude any characters of the C character set being used */
        /* to compress an error message in case of subsequent chaos. */
        esk = compress_strings(1, digraph_counts,
                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
0123456789 _!\"#$%&'()=-~^{[+;*:}]|\\<,>.?/");
        charpoint = stringcount = 0;
    }

    {   time_t t0 = time(NULL);
        struct tm *tm = localtime(&t0);
        fprintf(fe, "/*\n");
        fprintf(fe, " * Copyright (C) Advanced RISC Machines Limited and\n");
        fprintf(fe, " *               Codemist Limited, 19%d.\n",
            tm->tm_year);
        fprintf(fe, " * All rights reserved.\n *\n");
        fprintf(fe, " * %s, created by %s on %s */\n\n",
            messages, SELF, ctime(&t0));
#ifdef __CC_NORCROFT
        fprintf(fe, "#ifdef __CC_NORCROFT\n#  pragma force_top_level\n");
        fprintf(fe, "#  pragma include_only_once\n#endif\n\n");
#endif
        fprintf(fe, "#ifndef _msgs_LOADED\n#define _msgs_LOADED 1\n\n");
    }

    if (number_msgs)
        fprintf(fe, "typedef int syserr_message_type;\n\n");
    else
        fprintf(fe, "typedef char *syserr_message_type;\n");

#ifdef __CC_NORCROFT
/* We really need a magic name like __check_ncc_internal_formats for -v3. */
    fprintf(fe, "#ifdef __CC_NORCROFT\n#  pragma -v3\n#endif\n\n");
#endif

    fprintf(fe, "extern void syserr(syserr_message_type errcode, ...);\n\n");

    if (number_msgs)          /* nothing yet */
    {   msgnumber = 0;
    }
    else
#ifdef __CC_NORCROFT
/* We really need a magic name like __check_ncc_internal_formats for -v3. */
        fprintf(fe, "#ifdef __CC_NORCROFT\n#  pragma -v0\n#endif\n\n");
#endif

    if (nerrors == 0) for (j = 0;  j < nepaths;  j++)
    {   FILE *fq = fopen(epaths[j], "r");
        if (fq == NULL)
        {   fprintf(stderr, "%s: Unable to open error prototype file %s\n",
                    SELF, epaths[j]);
            ++nerrors;
            continue;
        }
        fprintf(stderr, "Copying error file %s\n", epaths[j]);
        copy_msg_file(fe, fq, ft);
        fclose(fq);
    }

    charpoint = stringcount = 0;

    if (nerrors != 0)
        fprintf(fe, "\n#error \"%s failed to create %s\"\n", SELF, messages);

    if (number_msgs)
        fprintf(fe, "\n#define NUMBERED_MESSAGES 1\n");

    if (squeeze_msgs)
    {   fprintf(fe, "\n#define COMPRESSED_MESSAGES 1\n");
        fprintf(fe, "\n#define MSGSTACKDEPTH %d\n", esk+1);
        fprintf(fe, "\n#ifdef DEFINE_MSG_COMPRESSION_TABLE\n");
        fprintf(fe, "\nstatic unsigned short int ecompression_info[256] = {");
        for (j = 0;  j < 256;  j++)
        {   if ((j & 0x7) == 0) fprintf(fe, "\n    ");
            fprintf(fe, "0x%.4x", compression[j]);
            if (j == 255) fprintf(fe, "};\n\n");
            else fprintf(fe, ", ");
        }
        fprintf(fe, "\n#endif\n");
    }

    fprintf(fe, "\n#endif  /* _msgs_LOADED */\n");
    fprintf(fe, "/* end of %s */\n", messages);
    fclose(fe);
    if (ft != NULL) fclose(ft);

    return nerrors != 0;
}

int main(int argc, ArgvType *argv)
{
    int j, filecount = 0, rc;
    char *arg, *viafile, *headers = NULL, *messages = NULL, *text = NULL;

    nerrors = npaths = filecount = 0;
    viafile = NULL;

    for (j = 1;  j < argc;  ++j)
    {   arg = argv[j];
        if (arg[0] == '-')
        {   switch (arg[1])
            {
/*
 * -n           get error messages mapped onto numeric codes
 */
    case 'n':
    case 'N':   number_msgs = 1;
                break;

/*
 * -d           Emit directives for the mktags program for syserr messages
 */
    case 'd':
    case 'D':   emit_tagdirs = 1;
                break;

/*
 * -t           get text of messages mapped onto numeric codes
 */
    case 't':
    case 'T':   arg += 2;
                if (*arg == 0) arg = argv[++j];
                text = arg;
                break;
/*
 * -s           squeeze error messages (as individual strings)
 */
    case 's':
    case 'S':   squeeze_msgs = 1;
                break;
/*
 * -i           specify a search path for finding ordinary files to be
 *              copied to create in-store headers.  E.g. -i /usr/include/
 *              might make sense on a Unix system.
 */
    case 'i':
    case 'I':   if (npaths >= MAXPATHS)
                {
                    fprintf(stderr,
                        "%s: Too many paths - only %u allowed\n",
                        SELF, MAXPATHS);
                    exit(1);
                }
                else
                {
                    arg += 2;
                    if (*arg == 0) arg = argv[++j];
                    paths[npaths++] = arg;
                }
                break;
/*
 * -o           File to put bulk of compressed text into. This file
 *              will be compiled (as "headers.c") to form part of the
 *              compiler.
 */
    case 'o':
    case 'O':   arg += 2;
                if (*arg == 0) arg = argv[++j];
                headers = arg;
                break;
/*
 * -e           Error header output file ("errors.h") gets created by
 *              scanning error prototype file and assigning numeric error
 *              codes to messages, inserting compressed text for the
 *              messages into headers.c
 */
    case 'e':
    case 'E':   arg += 2;
                if (*arg == 0) arg = argv[++j];
                messages = arg;
                break;
/*
 * -q           prototype for "errors.h" contains raw form of error messages
 *              with various marker strings (%Z, %O and %S) to show what
 *              form of transformation is needed.
 *              %O   define a numeric error code & arrange decoding tables,
 *              %S   define numeric code but do not provide for decoding,
 *              %Z   leave error text as a literal string (not numeric).
 *
 *              There will normally be (at least) two error prototype files,
 *              one the generic one (mip/errproto.h) and one that is target
 *              specific (xxx/mcerrs.h).  It may also prove necessary to
 *              have language (i.e. front-end) specific error files and
 *              even host-system specific ones - hence this utility allows
 *              for MAXPATHS inclusions here.
 */
    case 'q':
    case 'Q':   if (npaths >= MAXPATHS)
                {
                    fprintf(stderr,
                        "%s: Too many paths - only %u allowed\n",
                        SELF, MAXPATHS);
                    exit(1);
                }
                else
                {
                    arg += 2;
                    if (*arg == 0) arg = argv[++j];
                    epaths[nepaths++] = arg;
                }
                break;

/*
 * -v           Provide the list of files for scanning through an indirection.
 */
    case 'v':
    case 'V':   arg += 2;
                if (*arg == 0) arg = argv[++j];
                viafile = arg;
                break;
    case 'u':
    case 'U':   safe_compress = 0;
                break;
default:
                fprintf(stderr, "%s: Unknown option %s\n", SELF, arg);
                ++nerrors;
                break;
            }
        }
        else
            ++filecount;
    }

    if (number_msgs && text == NULL)
    {   fprintf(stderr, "%s: Use of -n requires use of -t<textfile>\n", SELF);
        return 1;
    }

    if (!number_msgs && text != NULL)
    {   fprintf(stderr, "%s: Assuming -n (implied by -t<textfile>)\n", SELF);
        number_msgs = 1;
    }

    if (nerrors != 0) return 1;

    chardata = (unsigned char *)malloc(MAXCHARS);
    digraph_counts = (int *)malloc(256*256*sizeof(int));

    if (chardata == NULL || digraph_counts == NULL)
    {   fprintf(stderr, "%s: Not enough free memory\n", SELF);
        return 1;
    }

    rc = 0;
    if (headers != NULL)
    {   charpoint = stringcount = 0;
        rc |= make_headers(headers, argc, argv, viafile);
    }

    nerrors = 0;
    if (messages != NULL)
    {   charpoint = stringcount = 0;
        rc |= make_messages(messages, squeeze_msgs, text);
    }

    free(chardata);
    free(digraph_counts);

    return rc;
}

/* End of genhdrs.c */
