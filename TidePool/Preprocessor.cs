/* ----------------------------------------------------------------------------
TidePool - a C# port of TinyC
Copyright (C) 2018  George E Greaney
 
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
----------------------------------------------------------------------------*/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace TidePool
{
    public enum TPTOKEN
    {
        TOK_TWODOTS = 0xa8,         /* C++ token ? */
        TOK_CCHAR = 0xb3,           /* char constant in tokc */
        TOK_LCHAR = 0xb4,
        TOK_CINT = 0xb5,            /* number in tokc */
        TOK_CUINT = 0xb6,           /* unsigned int constant */
        TOK_CLLONG = 0xb7,          /* long long constant */
        TOK_CULLONG = 0xb8,         /* unsigned long long constant */
        TOK_STR = 0xb9,             /* pointer to string in tokc */
        TOK_LSTR = 0xba,
        TOK_CFLOAT = 0xbb,          /* float constant */
        TOK_CDOUBLE = 0xbc,         /* double constant */
        TOK_CLDOUBLE = 0xbd,        /* long double constant */
        TOK_PPNUM = 0xbe,           /* preprocessor number */
        TOK_PPSTR = 0xbf,           /* preprocessor string */
        TOK_LINENUM = 0xc0,         /* line number info */

        TOK_LAST = Preprocessor.TOK_IDENT - 1,

        TOK_INT,
        TOK_VOID,
        TOK_CHAR,
        TOK_IF,
        TOK_ELSE,
        TOK_WHILE,
        TOK_BREAK,
        TOK_RETURN,
        TOK_FOR,
        TOK_EXTERN,
        TOK_STATIC,
        TOK_UNSIGNED,
        TOK_GOTO,
        TOK_DO,
        TOK_CONTINUE,
        TOK_SWITCH,
        TOK_CASE,

        TOK_CONST1,
        TOK_CONST2, /* gcc keyword */
        TOK_CONST3, /* gcc keyword */
        TOK_VOLATILE1,
        TOK_VOLATILE2, /* gcc keyword */
        TOK_VOLATILE3, /* gcc keyword */
        TOK_LONG,
        TOK_REGISTER,
        TOK_SIGNED1,
        TOK_SIGNED2, /* gcc keyword */
        TOK_SIGNED3, /* gcc keyword */
        TOK_AUTO,
        TOK_INLINE1,
        TOK_INLINE2, /* gcc keyword */
        TOK_INLINE3, /* gcc keyword */
        TOK_RESTRICT1,
        TOK_RESTRICT2,
        TOK_RESTRICT3,
        TOK_EXTENSION, /* gcc keyword */

        TOK_GENERIC,

        TOK_FLOAT,
        TOK_DOUBLE,
        TOK_BOOL,
        TOK_SHORT,
        TOK_STRUCT,
        TOK_UNION,
        TOK_TYPEDEF,
        TOK_DEFAULT,
        TOK_ENUM,
        TOK_SIZEOF,
        TOK_ATTRIBUTE1,
        TOK_ATTRIBUTE2,
        TOK_ALIGNOF1,
        TOK_ALIGNOF2,
        TOK_TYPEOF1,
        TOK_TYPEOF2,
        TOK_TYPEOF3,
        TOK_LABEL,
        TOK_ASM1,
        TOK_ASM2,
        TOK_ASM3,

        /*********************************************************************/
        /* the following are not keywords. They are included to ease parsing */
        /* preprocessor only */
        TOK_DEFINE,
        TOK_INCLUDE,
        TOK_INCLUDE_NEXT,
        TOK_IFDEF,
        TOK_IFNDEF,
        TOK_ELIF,
        TOK_ENDIF,
        TOK_DEFINED,
        TOK_UNDEF,
        TOK_ERROR,
        TOK_WARNING,
        TOK_LINE,
        TOK_PRAGMA,
        TOK___LINE__,
        TOK___FILE__,
        TOK___DATE__,
        TOK___TIME__,
        TOK___FUNCTION__,
        TOK___VA_ARGS__,
        TOK___COUNTER__,

        /* special identifiers */
        TOK___FUNC__,

        /* special floating point values */
        TOK___NAN__,
        TOK___SNAN__,
        TOK___INF__,

        /* attribute identifiers */
        /* XXX: handle all tokens generically since speed is not critical */
        TOK_SECTION1,
        TOK_SECTION2,
        TOK_ALIGNED1,
        TOK_ALIGNED2,
        TOK_PACKED1,
        TOK_PACKED2,
        TOK_WEAK1,
        TOK_WEAK2,
        TOK_ALIAS1,
        TOK_ALIAS2,
        TOK_UNUSED1,
        TOK_UNUSED2,
        TOK_CDECL1,
        TOK_CDECL2,
        TOK_CDECL3,
        TOK_STDCALL1,
        TOK_STDCALL2,
        TOK_STDCALL3,
        TOK_FASTCALL1,
        TOK_FASTCALL2,
        TOK_FASTCALL3,
        TOK_REGPARM1,
        TOK_REGPARM2,

        TOK_MODE,
        TOK_MODE_QI,
        TOK_MODE_DI,
        TOK_MODE_HI,
        TOK_MODE_SI,
        TOK_MODE_word,

        TOK_DLLEXPORT,
        TOK_DLLIMPORT,
        TOK_NORETURN1,
        TOK_NORETURN2,
        TOK_VISIBILITY1,
        TOK_VISIBILITY2,

        TOK_builtin_types_compatible_p,
        TOK_builtin_choose_expr,
        TOK_builtin_constant_p,
        TOK_builtin_frame_address,
        TOK_builtin_return_address,
        TOK_builtin_expect,

        /* pragma */
        TOK_pack,
        TOK_comment,
        TOK_lib,
        TOK_push_macro,
        TOK_pop_macro,
        TOK_once,
        TOK_option
    }

    public class Preprocessor
    {
        public const int TOK_EOF = -1;
        public const int TOK_LINEFEED = 10;             /* line feed */

        public TidePool tp;

        public int tokenFlags;
        public int parseFlags;

        public BufferedFile curFile;
        public int ch;
        public int tok;
        public int macro_ptr;
        public String tokcstr;					/* current parsed string, if any */

        public int total_lines;
        public int total_bytes;

        public int tok_ident;
        public List<TokenSym> table_ident;

        public const int TOK_IDENT = 256;

        public int SYM_FIRST_ANOM = 0x10000000;                 /* first anonymous sym */

        public const int TOK_FLAG_BOL = 0x0001;		            /* beginning of line before */
        public const int TOK_FLAG_BOF = 0x0002;		            /* beginning of file before */
        public const int TOK_FLAG_ENDIF = 0x0004;		        /* a endif was found matching starting #ifdef */
        public const int TOK_FLAG_EOF = 0x0008;		            /* end of file */

        public const int PARSE_FLAG_PREPROCESS = 0x0001;	    /* activate preprocessing */
        public const int PARSE_FLAG_TOK_NUM = 0x0002;	        /* return numbers instead of TOK_PPNUM */
        public const int PARSE_FLAG_LINEFEED = 0x0004;	        /* line feed is returned as a token. line feed is also returned at eof */
        public const int PARSE_FLAG_ASM_FILE = 0x0008;	        /* we processing an asm file: '#' can be used for line comment, etc. */
        public const int PARSE_FLAG_SPACES = 0x0010;	        /* next() returns space tokens (for -E) */
        public const int PARSE_FLAG_ACCEPT_STRAYS = 0x0020;     /* next() returns '\\' token */
        public const int PARSE_FLAG_TOK_STR = 0x0040;	        /* return parsed strings instead of TOK_PPSTR */

        public const int TOK_HASH_SIZE = 16384;                 /* must be a power of two */
        static List<TokenSym>[] hash_ident;			            //symbol tbl

        public int[] isidnum_table;

        /* isidnum_table flags: */
        public const int IS_SPC = 1;
        public const int IS_ID = 2;
        public const int IS_NUM = 4;

        public string[] tcc_keywords = { "int", "void", "char", "if", "else", "while", "break",
                "return", "for", "extern", "static", "unsigned", "goto", "do", "continue", "switch", "case"};


        //---------------------------------------------------------------------

        public Preprocessor(TidePool _tp)
        {
            tp = _tp;

            macro_ptr = 0;

            table_ident = new List<TokenSym>();

            hash_ident = new List<TokenSym>[TOK_HASH_SIZE];

            //add keywords to symbol tbl
            tok_ident = TOK_IDENT;
            foreach (String keystr in tcc_keywords)
            {
                tok_alloc(keystr, keystr.Length);      //add keyword to sym tbl
            }

            // init isid table 
            isidnum_table = new int[256 - BufferedFile.CH_EOF];
            for (int i = BufferedFile.CH_EOF; i < 128; i++)
                set_idnum(i,
                    is_space(i) ? IS_SPC
                    : isid(i) ? IS_ID
                    : isnum(i) ? IS_NUM
                    : 0);

            for (int i = 128; i < 256; i++)
                set_idnum(i, IS_ID);


        }

        public bool is_space(int ch)
        {
            return (ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f' || ch == '\r');
        }

        public bool isid(int c)
        {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
        }

        public bool isnum(int c)
        {
            return c >= '0' && c <= '9';
        }

        public bool isoct(int c)
        {
            return c >= '0' && c <= '7';
        }

        public char toup(char c)
        {
            return (c >= 'a' && c <= 'z') ? (char)((int)c - 'a' + 'A') : c;
        }

        public void skip() { }
        public void expect() { }

        public void cstr_realloc() { }
        public void cstr_ccat() { }
        public void cstr_cat() { }
        public void cstr_wccat() { }
        public void cstr_new() { }
        public void cstr_free() { }
        public void cstr_reset() { }
        public void add_char() { }

        //- tokens ------------------------------------------------------------

        /* allocate a new token */
        public TokenSym tok_alloc_new(uint hash, String idstr, int len)
        {
            TokenSym ts;
            int i;

            if (tok_ident >= SYM_FIRST_ANOM)
            {
                tp.tp_error("memory full (symbols)");
            }

            i = tok_ident - TOK_IDENT;
            ts = new TokenSym(idstr, len);          //alloc new token rec
            table_ident.Add(ts);					//and store in ident tbl
            ts.tok = tok_ident++;					//token num is ident tbl idx

            List<TokenSym> pts = hash_ident[hash];
            if (pts == null)
            {
                pts = new List<TokenSym>();
                hash_ident[hash] = pts;
            }
            pts.Add(ts);                	    //add ident to sym tbl
            return ts;
        }

        public uint TOK_HASH_FUNC(uint h, int c)
        {
            return ((h) + ((h) << 5) + ((h) >> 27) + (uint)(c));
        }

        /* find a token and add it if not found */
        public TokenSym tok_alloc(string idstr, int len)
        {
            TokenSym ts;

            List<TokenSym> pts;
            int i;
            uint h;

            //hash token str
            h = 1;
            for (i = 0; i < len; i++)
            {
                h = TOK_HASH_FUNC(h, (int)idstr[i]);
            }
            h &= (TOK_HASH_SIZE - 1);

            pts = hash_ident[h];		//get sym tbl slot
            if (pts != null)
            {
                for (i = 0; i < pts.Count; i++)
                {
                    ts = pts[i];
                    if ((ts.len == len) && (ts.str.Equals(idstr)))
                        return ts;
                }
            }

            //if not found, alloc new token symbol & add it to sym table
            return tok_alloc_new(h, idstr, len);
        }

        public string get_tok_str(int tok)
        {
            return "foo";
        }

        public int handle_eob()
        {
            int len;

            /* only tries to read if really end of buffer */
            if (curFile.buf_ptr >= curFile.buf_end)
            {
                if ((curFile.fs != null) && (curFile.fs.Length > curFile.bytesRead))
                {
                    len = TidePool.IO_BUF_SIZE;
                    len = curFile.fs.Read(curFile.buffer, curFile.bytesRead, len);      //read source file -> buf
                    if (len < 0)
                        len = 0;
                }
                else
                {
                    len = 0;
                }

                total_bytes += len;
                curFile.bytesRead += len;
                curFile.buf_ptr = 0;
                curFile.buf_end = len;			                                //reset buf start & end ptrs
                curFile.buffer[curFile.buf_end] = BufferedFile.CH_EOB;			//mark buf end
            }

            if (curFile.buf_ptr < curFile.buf_end)
            {
                return curFile.buffer[0];				//ret first char
            }
            else
            {
                curFile.buf_ptr = curFile.buf_end;
                return BufferedFile.CH_EOF;						//or eof
            }

        }

        public void inp() { }

        public bool handle_stray_noerror()
        {
            return true;
        }

        public void handle_stray() { }

        public int handle_stray1(int p)
        {
            int c;

            curFile.buf_ptr = p;
            if (p >= curFile.buf_end)
            {
                c = handle_eob();
                if (c != '\\')
                    return c;
                p = curFile.buf_ptr;
            }
            ch = curFile.buffer[p];
            if (handle_stray_noerror())
            {
                if ((parseFlags & PARSE_FLAG_ACCEPT_STRAYS) == 0)
                    tp.tp_error("stray '\\' in program");
                curFile.buffer[--curFile.buf_ptr] = (byte)'\\';
            }
            p = curFile.buf_ptr;
            c = curFile.buffer[p];
            return c;
        }

        /* handle the complicated stray case */
        public void PEEKC(ref int c, ref int p)
        {
            p++;
            c = curFile.buffer[p];
            if (c == '\\')
            {
                c = handle_stray1(p);
                p = curFile.buf_ptr;
            }
        }

        public void minp() { }
        public void parse_line_comment() { }
        public void parse_comment() { }

        public int set_idnum(int c, int val)
        {
            int idx = c - BufferedFile.CH_EOF;
            int prev = isidnum_table[idx];
            isidnum_table[idx] = val;
            return prev;
        }

        public void skip_spaces() { }
        public void check_space() { }
        public void parse_pp_string() { }
        public void preprocess_skip() { }
        public void tok_str_new() { }
        public void tok_str_alloc() { }
        public void tok_str_dup() { }
        public void tok_str_free_str() { }
        public void tok_str_free() { }
        public void tok_str_realloc() { }
        public void tok_str_add() { }
        public void begin_macro() { }
        public void end_macro() { }
        public void tok_str_add2() { }
        public void tok_str_add_tok() { }
        public void TOK_GET() { }
        public void macro_is_equal() { }
        public void define_push() { }
        public void define_undef() { }
        public void define_find() { }
        public void free_defines() { }
        public void label_find() { }
        public void label_push() { }
        public void label_pop() { }
        public void maybe_run_test() { }
        public void expr_preprocess() { }
        public void parse_define() { }
        public void search_cached_include() { }
        public void pragma_parse() { }
        public void preprocess() { }
        public void parse_escape_string() { }
        public void parse_string() { }
        public void bn_lshift() { }
        public void bn_zero() { }
        public void parse_number() { }

        //- scanning & macro subs -----------------------------------------------------

        public void next_nomacro1()
        {
            TokenSym ts = null;
            int t;

            int p = curFile.buf_ptr;

        redo_no_start:
            int c = curFile.buffer[p];

            switch (c)
            {
                case ' ':
                case '\t':
                    tok = c;
                    p++;
                    if ((parseFlags & PARSE_FLAG_SPACES) != 0)
                        goto keep_tok_flags;

                    while ((isidnum_table[curFile.buffer[p]] & IS_SPC) != 0)
                        ++p;
                    goto redo_no_start;

                case '\f':
                case '\v':
                case '\r':
                    p++;
                    goto redo_no_start;

                case '\\':
                    /* first look if it is in fact an end of buffer */
                    c = handle_stray1(p);
                    p = curFile.buf_ptr;
                    if (c == '\\')
                        goto parse_simple;
                    if (c != BufferedFile.CH_EOF)
                        goto redo_no_start;
                            {
                    //            TCCState *s1 = tcc_state;
                                if (((parseFlags & PARSE_FLAG_LINEFEED) != 0) && !((tokenFlags & TOK_FLAG_EOF) != 0)) {
                                    tokenFlags |= TOK_FLAG_EOF;
                                    tok = TOK_LINEFEED;
                                    goto keep_tok_flags;
                                } else if (!((parseFlags & PARSE_FLAG_PREPROCESS) != 0)) {
                                    tok = TOK_EOF;
                                } else if (tp.ifdef_stack_ptr != 0) {
                                    tp.tp_error("missing #endif");
                                } else if (tp.include_stack_ptr == 0) {
                                    /* no include left : end of file. */
                                    tok = TOK_EOF;
                                } else {
                                    tokenFlags &= ~TOK_FLAG_EOF;
                                    /* pop include file */

                                    /* test if previous '#endif' was after a #ifdef at start of file */
                    //                if (tok_flags & TOK_FLAG_ENDIF) {
                    //#ifdef INC_DEBUG
                    //                    printf("#endif %s\n", get_tok_str(file->ifndef_macro_saved, NULL));
                    //#endif
                    //                    search_cached_include(s1, file->filename, 1)
                    //                        ->ifndef_macro = file->ifndef_macro_saved;
                    //                    tok_flags &= ~TOK_FLAG_ENDIF;
                    //                }

                    //                /* add end of include file debug info */
                    //                if (tcc_state->do_debug) {
                    //                    put_stabd(N_EINCL, 0, 0);
                    //                }
                    //                /* pop include stack */
                    //                tcc_close();
                    //                s1->include_stack_ptr--;
                    //                p = file->buf_ptr;
                    //                if (p == file->buffer)
                    //                    tok_flags = TOK_FLAG_BOF|TOK_FLAG_BOL;
                                    goto redo_no_start;
                                }
                            }
                    break;

                case '\n':
                    curFile.line_num++;
                    tokenFlags |= TOK_FLAG_BOL;
                    p++;
                maybe_newline:
                    if ((parseFlags & PARSE_FLAG_LINEFEED) == 0)
                        goto redo_no_start;
                    tok = TOK_LINEFEED;
                    goto keep_tok_flags;

                case '#':

                //identifiers
                case '$':

                case 'a':
                case 'b':
                case 'c':
                case 'd':
                case 'e':
                case 'f':
                case 'g':
                case 'h':
                case 'i':
                case 'j':
                case 'k':
                case 'l':
                case 'm':
                case 'n':
                case 'o':
                case 'p':
                case 'q':
                case 'r':
                case 's':
                case 't':
                case 'u':
                case 'v':
                case 'w':
                case 'x':
                case 'y':
                case 'z':
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'E':
                case 'F':
                case 'G':
                case 'H':
                case 'I':
                case 'J':
                case 'K':
                case 'M':
                case 'N':
                case 'O':
                case 'P':
                case 'Q':
                case 'R':
                case 'S':
                case 'T':
                case 'U':
                case 'V':
                case 'W':
                case 'X':
                case 'Y':
                case 'Z':
                case '_':
                    {
                        int p1 = p;

                        //hash ident str
                        uint h = 1;
                        h = TOK_HASH_FUNC(h, c);
                        String idstr = "" + (char)c;
                        c = curFile.buffer[++p];
                        while (((isidnum_table[c - BufferedFile.CH_EOF] & (IS_ID | IS_NUM)) != 0))
                        {
                            h = TOK_HASH_FUNC(h, c);
                            idstr += (char)c;
                            c = curFile.buffer[++p];
                        }
                        int len = p - p1;

                        if (c != '\\')
                        {
                            List<TokenSym> pts;

                            /* fast case : no stray found, so we have the full token and we have already hashed it */
                            h &= (TOK_HASH_SIZE - 1);
                            pts = hash_ident[h];
                            if (pts != null)
                            {
                                for (int i = 0; i < pts.Count; i++)
                                {
                                    ts = pts[i];
                                    if ((ts.len == len) && (ts.str.Equals(idstr)))
                                        goto token_found;
                                }
                            }
                            ts = tok_alloc_new(h, idstr, len);
                        token_found: ;
                        }
                        else
                        {
                            //    /* slower case */
                            //    cstr_reset(&tokcstr);
                            //    cstr_cat(&tokcstr, (char *) p1, len);
                            //    p--;
                            //    PEEKC(c, p);
                            //parse_ident_slow:
                            //    while (isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
                            //    {
                            //        cstr_ccat(&tokcstr, c);
                            //        PEEKC(c, p);
                            //    }
                            //    ts = tok_alloc(tokcstr.data, tokcstr.size);
                        }
                        tok = ts.tok;
                        break;
                    }

                case 'L':

//digits
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    t = c;
                    PEEKC(ref c, ref p);
                /* after the first digit, accept digits, alpha, '.' or sign if prefixed by 'eEpP' */
                parse_num:
                    tokcstr = "";		//clear buf to hold num str
                    for (; ; )
                    {
                        tokcstr += (char)t;             //add digit
                        if (!(((isidnum_table[c - BufferedFile.CH_EOF] & (IS_ID | IS_NUM)) != 0)
                              || (c == '.')
                              || ((c == '+' || c == '-')
                                  && (((t == 'e' || t == 'E')
                                        && !((parseFlags & PARSE_FLAG_ASM_FILE) != 0)
                            /* 0xe+1 is 3 tokens in asm */
                                            && ((tokcstr[0] == '0') && (toup(tokcstr[1]) == 'X'))
                                      || t == 'p' || t == 'P')))))
                            break;
                        t = c;
                        PEEKC(ref c, ref p);		//get next char
                    }
                    /* We add a trailing '\0' to ease parsing */
                    tokcstr += '\0';
                    tok = (int)TPTOKEN.TOK_PPNUM;
                    break;

                case '.':

                case '\'':
                case '\"':

                case '<':

                case '>':

                case '&':

                case '|':

                case '+':

                case '-':

                case '/':

                /* simple tokens */
                case '(':
                case ')':
                case '[':
                case ']':
                case '{':
                case '}':
                case ',':
                case ';':
                case ':':
                case '?':
                case '~':

                parse_simple:
                    tok = c;
                    p++;
                    break;

                default:
                    break;

            }

            tokenFlags = 0;

        keep_tok_flags:
            curFile.buf_ptr = p;			//update pos buf

            Console.Out.WriteLine("token = {0} {1}", tok.ToString("X2"), get_tok_str(tok)); //, tokc));
        }

        public void next_nomacro_spc()
        {
            if (macro_ptr > 0)
            {
                //    redo:
                //        tok = *macro_ptr;
                //        if (tok) {
                //            TOK_GET(&tok, &macro_ptr, &tokc);
                //            if (tok == TOK_LINENUM) {
                //                file->line_num = tokc.i;
                //                goto redo;
                //            }
                //        }
            }
            else
            {
                next_nomacro1();
            }
        }

        public void next_nomacro() 
        {
            do
            {
                next_nomacro_spc();
            } while (tok < 256 && ((isidnum_table[tok - BufferedFile.CH_EOF] & IS_SPC) != 0));
        }

        public void macro_arg_subst() { }
        public void paste_tokens() { }
        public void macro_twosharps() { }
        public void next_argstream() { }
        public void macro_subst_tok() { }
        public void macro_subst() { }

        public void next()
        {
            if ((parseFlags & PARSE_FLAG_SPACES) != 0)
            {
                next_nomacro_spc();
            }
            else
            {
                next_nomacro();
            }
            //    if (macro_ptr) {
            //        if (tok == TOK_NOSUBST || tok == TOK_PLCHLDR) {
            //        /* discard preprocessor markers */
            //            goto redo;
            //        } else if (tok == 0) {
            //            /* end of macro or unget token string */
            //            end_macro();
            //            goto redo;
            //        }
            //    } else if (tok >= TOK_IDENT && (parse_flags & PARSE_FLAG_PREPROCESS)) {
            //        Sym *s;
            //        /* if reading from file, try to substitute macros */
            //        s = define_find(tok);
            //        if (s) {
            //            Sym *nested_list = NULL;
            //            tokstr_buf.len = 0;
            //            macro_subst_tok(&tokstr_buf, &nested_list, s);
            //            tok_str_add(&tokstr_buf, 0);
            //            begin_macro(&tokstr_buf, 2);
            //            goto redo;
            //        }
            //    }
            //    /* convert preprocessor tokens into C tokens */
            //    if (tok == TOK_PPNUM) {
            //        if  (parse_flags & PARSE_FLAG_TOK_NUM)
            //            parse_number((char *)tokc.str.data);
            //    } else if (tok == TOK_PPSTR) {
            //        if (parse_flags & PARSE_FLAG_TOK_STR)
            //            parse_string((char *)tokc.str.data, tokc.str.size - 1);
            //    }
        }

        public void unget_tok() { }

        public void preprocess_start(bool isAsm)
        {
            curFile = tp.infiles[tp.infiles.Count - 1];
            total_lines = 0;
            tokenFlags = TOK_FLAG_BOL | TOK_FLAG_BOF;
        }

        public void preprocess_end() { }

        public void pp_delete() { }
        public void tok_print() { }

        public void pp_line(BufferedFile f, int level)
        {
        }

        public void define_print() { }
        public void pp_debug_defines() { }
        public void pp_debug_builtins() { }
        public void pp_need_space() { }
        public void pp_check_he0xE() { }

        public void tp_preprocess()
        {
            parseFlags = PARSE_FLAG_PREPROCESS
            | (parseFlags & PARSE_FLAG_ASM_FILE)
            | PARSE_FLAG_LINEFEED
            | PARSE_FLAG_SPACES
            | PARSE_FLAG_ACCEPT_STRAYS
            ;

            pp_line(curFile, 0);
            while (true)
            {
                next();
                if (tok == TOK_EOF)
                    break;
            }
        }
    }

    //-----------------------------------------------------------------------------

    public class BufferedFile
    {
        public const int CH_EOB = '\\';
        public const int CH_EOF = -1;

        public int buf_ptr;
        public int buf_end;
        public FileStream fs;
        public int bytesRead;

        public int line_num;
        public int line_ref;
        public int ifndef_macro;
        public int ifndef_macro_saved;
        public int ifdef_stack_ptr;
        public int include_next_index;

        public string filename;
        public string true_filename;

        public char[] unget;

        public byte[] buffer;

        public BufferedFile(TidePool tp, String fname, int initlen, int buflen)
        {
            buffer = new byte[buflen + 1];
            buf_ptr = 0;
            buf_end = initlen;
            buffer[buf_end] = CH_EOB;

            filename = String.Copy(fname);
            true_filename = fname;
            line_num = 1;
            ifdef_stack_ptr = tp.ifdef_stack_ptr;

            fs = null;
            bytesRead = 0;
        }
    }

    //-------------------------------------------------------------------------

    public class TokenSym
    {
        public Sym sym_define; 	        /* direct pointer to define */
        public Sym sym_label; 	            /* direct pointer to label */
        public Sym sym_struct; 	        /* direct pointer to structure */
        public Sym sym_identifier;         /* direct pointer to identifier */

        public int tok; 			                /* token number */
        public int len;
        public string str;

        public TokenSym(string _str, int _len)
        {
            str = _str;
            len = _len;

            sym_define = null;
            sym_label = null;
            sym_struct = null;
            sym_identifier = null;

        }
    }

}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");