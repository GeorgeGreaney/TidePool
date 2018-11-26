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

        public TidePool tp;

        public BufferedFile curFile;

        public int ch;
        public int tok;

        public int parseFlags;

        public int total_lines;
        public int total_bytes;

        public const int PARSE_FLAG_PREPROCESS = 0x0001;	    /* activate preprocessing */
        public const int PARSE_FLAG_TOK_NUM = 0x0002;	        /* return numbers instead of TOK_PPNUM */
        public const int PARSE_FLAG_LINEFEED = 0x0004;	        /* line feed is returned as a token. line feed is also returned at eof */
        public const int PARSE_FLAG_ASM_FILE = 0x0008;	        /* we processing an asm file: '#' can be used for line comment, etc. */
        public const int PARSE_FLAG_SPACES = 0x0010;	        /* next() returns space tokens (for -E) */
        public const int PARSE_FLAG_ACCEPT_STRAYS = 0x0020;     /* next() returns '\\' token */
        public const int PARSE_FLAG_TOK_STR = 0x0040;	        /* return parsed strings instead of TOK_PPSTR */

        public int[] isidnum_table;

        /* isidnum_table flags: */
        public const int IS_SPC = 1;
        public const int IS_ID = 2;
        public const int IS_NUM = 4;

        //---------------------------------------------------------------------

        public Preprocessor(TidePool _tp)
        {
            tp = _tp;

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

        public int toup(int c)
        {
            return (c >= 'a' && c <= 'z') ? c - 'a' + 'A' : c;
        }

        public uint TOK_HASH_FUNC(uint h, int c)
        {
            return ((h) + ((h) << 5) + ((h) >> 27) + (uint)(c));
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

        public void tok_alloc_new() { }
        public void tok_alloc() { }

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
                if (curFile.fs != null)
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

        public void next_nomacro1()
        {
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
                    //        {
                    //            TCCState *s1 = tcc_state;
                    //            if ((parse_flags & PARSE_FLAG_LINEFEED)
                    //                && !(tok_flags & TOK_FLAG_EOF)) {
                    //                tok_flags |= TOK_FLAG_EOF;
                    //                tok = TOK_LINEFEED;
                    //                goto keep_tok_flags;
                    //            } else if (!(parse_flags & PARSE_FLAG_PREPROCESS)) {
                    //                tok = TOK_EOF;
                    //            } else if (s1->ifdef_stack_ptr != file->ifdef_stack_ptr) {
                    //                tcc_error("missing #endif");
                    //            } else if (s1->include_stack_ptr == s1->include_stack) {
                    //                /* no include left : end of file. */
                    //                tok = TOK_EOF;
                    //            } else {
                    //                tok_flags &= ~TOK_FLAG_EOF;
                    //                /* pop include file */

                    //                /* test if previous '#endif' was after a #ifdef at
                    //                   start of file */
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
                    //                goto redo_no_start;
                    //            }
                    //        }
                    break;

                case '\n':

                case '#':

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
                        c = curFile.buffer[++p];
                        while (((isidnum_table[c - BufferedFile.CH_EOF] & (IS_ID | IS_NUM)) != 0))
                        {
                            h = TOK_HASH_FUNC(h, c);
                            c = curFile.buffer[++p];
                        }
                        int len = p - p1;

                        //if (c != '\\') {
                        //    TokenSym **pts;

                        //    /* fast case : no stray found, so we have the full token and we have already hashed it */
                        //    h &= (TOK_HASH_SIZE - 1);
                        //    pts = &hash_ident[h];
                        //    for(;;) {
                        //        ts = *pts;
                        //        if (!ts)
                        //            break;
                        //        if (ts->len == len && !memcmp(ts->str, p1, len))
                        //            goto token_found;
                        //        pts = &(ts->hash_next);
                        //    }
                        //    ts = tok_alloc_new(pts, (char *) p1, len);
                        //token_found: ;
                        //} else {
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
                        //}
                        //tok = ts->tok;
                        break;
                    }

                case 'L':

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

       keep_tok_flags:
            curFile.buf_ptr = p;			//update pos buf

            Console.Out.WriteLine("token = {0} {1}\n", tok, get_tok_str(tok)); //, tokc));
        }

        public void next_nomacro_spc()
        {
            next_nomacro1();
        }

        public void next_nomacro() { }
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
        }

        public void unget_tok() { }

        public void preprocess_start(bool isAsm)
        {
            curFile = tp.infiles[tp.infiles.Count - 1];
            total_lines = 0;
        }

        public void preprocess_end() { }

        public void pp_new() { }
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
}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");