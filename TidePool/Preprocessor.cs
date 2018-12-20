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
        TOK_ULT = 0x92,
        TOK_UGE = 0x93,
        TOK_EQ = 0x94,
        TOK_NE = 0x95,
        TOK_ULE = 0x96,
        TOK_UGT = 0x97,
        TOK_Nset = 0x98,
        TOK_Nclear = 0x99,
        TOK_LT = 0x9c,
        TOK_GE = 0x9d,
        TOK_LE = 0x9e,
        TOK_GT = 0x9f,

        TOK_LAND = 0xa0,
        TOK_LOR = 0xa1,
        TOK_DEC = 0xa2,
        TOK_MID = 0xa3,/* inc/dec, to void constant */
        TOK_INC = 0xa4,
        TOK_UDIV = 0xb0,/* unsigned division */
        TOK_UMOD = 0xb1, /* unsigned modulo */
        TOK_PDIV = 0xb2, /* fast division with undefined rounding for pointers */

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
        TOK_TWODOTS = 0xa8,         /* C++ token ? */

        TOK_UMULL = 0xc2,           /* unsigned 32x32 -> 64 mul */
        TOK_ADDC1 = 0xc3,           /* add with carry generation */
        TOK_ADDC2 = 0xc4,           /* add with carry use */
        TOK_SUBC1 = 0xc5,           /* add with carry generation */
        TOK_SUBC2 = 0xc6,           /* add with carry use */
        TOK_ARROW = 0xc7,
        TOK_DOTS = 0xc8,            /* three dots */
        TOK_SHR = 0xc9,             /* unsigned shift right */
        TOK_TWOSHARPS = 0xca,       /* ## preprocessing token */
        TOK_PLCHLDR = 0xcb,         /* placeholder token as defined in C99 */
        TOK_NOSUBST = 0xcc,         /* means following token has already been pp'd */
        TOK_PPJOIN = 0xcd,          /* A '##' in the right position to mean pasting */
        TOK_CLONG = 0xce,           /* long constant */
        TOK_CULONG = 0xcf,          /* unsigned long constant */

        TOK_SHL = 0x01,             /* shift left */
        TOK_SAR = 0x02,             /* signed shift right */

        /* assignment operators : normal operator or 0x80 */
        TOK_A_MOD = 0xa5,
        TOK_A_AND = 0xa6,
        TOK_A_MUL = 0xaa,
        TOK_A_ADD = 0xab,
        TOK_A_SUB = 0xad,
        TOK_A_DIV = 0xaf,
        TOK_A_XOR = 0xde,
        TOK_A_OR = 0xfc,
        TOK_A_SHL = 0x81,
        TOK_A_SAR = 0x82,

        TOK_EOF = -1,
        TOK_LINEFEED = 10,             /* line feed */

        TOK_LAST = 255,
        TOK_IDENT = 256,

        TOK_INT = 256,
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
        TOK_UIDENT = TOK_DEFINE,
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

        public TidePool tp;
        public Compiler comp;

        public BufferedFile curFile;
        public int ch;
        public int tok;
        public CValue tokc;
        public int macro_ptr;
        public int parseFlags;
        public int tokenFlags;
        public String tokcstr;					/* current parsed string, if any */

        public int total_lines;
        public int total_bytes;

        public int tok_ident;
        public List<TokenSym> table_ident;


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
        public List<TokenSym>[] hash_ident;			            //symbol tbl
        public string token_buf;

        //static CString cstr_buf;							//for token spelling
        //static CString macro_equal_buf;
        //static TokenString tokstr_buf;							//for defines

        public int[] isidnum_table;

        /* isidnum_table flags: */
        public const int IS_SPC = 1;
        public const int IS_ID = 2;
        public const int IS_NUM = 4;

        public int pp_debug_tok;
        public int pp_debug_symv;
        public int pp_once;
        public int pp_expr;
        public int pp_counter;

        //static TokenString *macro_stack;

        public string[] tcc_keywords = { "int", "void", "char", "if", "else", "while", "break", "return", 
                                         "for", "extern", "static", "unsigned", "goto", "do", "continue", "switch", "case",
                                         "const", "__const", "__const__", "volatile", "__volatile", "__volatile__",
                                         "long", "register", "signed", "__signed", "__signed__",
                                         "auto", "inline", "__inline", "__inline__",
                                         "restrict", "__restrict", "__restrict__", "__extension__", "_Generic",
                                         "float", "double", "_Bool", "short",
                                         "struct", "union", "typedef", "default", "enum", "sizeof",
                                         "__attribute", "__attribute__", "__alignof", "__alignof__",
                                         "typeof", "__typeof", "__typeof__", "__label__", "asm", "__asm", "__asm__",
                                         "define", "include", "include_next", "ifdef", "ifndef", "elif", "endif", "defined", "undef",
                                         "error", "warning", "line", "pragma", 
                                         "__LINE__", "__FILE__", "__DATE__", "__TIME__", "__FUNCTION__", "__VA_ARGS__", "__COUNTER__",
                                         "__func__", "__nan__", "__snan__", "__inf__",
                                         "section", "__section__", "aligned", "__aligned__", "packed", "__packed__",
                                         "weak", "__weak__", "alias", "__alias__", "unused", "__unused__", "cdecl", "__cdecl", "__cdecl__",
                                         "stdcall", "__stdcall", "__stdcall__", "fastcall", "__fastcall", "__fastcall__", "regparm", "__regparm__",
                                         "__mode__", "__QI__", "__DI__", "__HI__", "__SI__", "__word__",
                                         "dllexport", "dllimport", "noreturn", "__noreturn__", "visibility", "__visibility__",
                                         "__builtin_types_compatible_p", "__builtin_choose_expr", "__builtin_constant_p",
                                         "__builtin_frame_address", "__builtin_return_address", "__builtin_expect",
                                         "pack", "comment", "lib", "push_macro", "pop_macro", "once", "option"
                                       };

        //---------------------------------------------------------------------

        public Preprocessor(TidePool _tp)
        {
            tp = _tp;

            tokc = new CValue();
            macro_ptr = 0;

            table_ident = new List<TokenSym>();

            hash_ident = new List<TokenSym>[TOK_HASH_SIZE];

            //add keywords to symbol tbl
            tok_ident = (int)TPTOKEN.TOK_IDENT;
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

        public void skip(int c)
        {
            if (tok != c)
                tp.tp_error("'{0}' expected (got \"{1}\")", c.ToString(), get_tok_str(tok, tokc));
            next();
        }

        public void expect(string msg)
        {
            tp.tp_error("{0} expected", msg);
        }

        //- tokens ------------------------------------------------------------

        /* allocate a new token */
        public TokenSym tok_alloc_new(uint hash, String idstr, int len)
        {
            TokenSym ts;
            int i;

            if (tok_ident >= Compiler.SYM_FIRST_ANOM)
            {
                tp.tp_error("memory full (symbols)");
            }

            i = tok_ident - (int)TPTOKEN.TOK_IDENT;
            ts = new TokenSym(idstr, len);              //alloc new token rec
            table_ident.Add(ts);					    //and store in ident tbl
            ts.tok = tok_ident++;					    //token num is ident tbl idx

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

        /* XXX: buffer overflow */
        /* XXX: float tokens */
        public string get_tok_str(int v, CValue cv)
        {
            string p = "";

            switch ((TPTOKEN)v)
            {
                case TPTOKEN.TOK_CINT:
                case TPTOKEN.TOK_CUINT:
                case TPTOKEN.TOK_CLONG:
                case TPTOKEN.TOK_CULONG:
                case TPTOKEN.TOK_CLLONG:
                case TPTOKEN.TOK_CULLONG:
                    /* XXX: not quite exact, but only useful for testing  */
                    p = string.Format("{0}", cv.i);
                    break;

                case TPTOKEN.TOK_LCHAR:
                case TPTOKEN.TOK_CCHAR:
                    p = "\'" + (char)cv.i + "\'";
                    if (v == (int)TPTOKEN.TOK_LCHAR)
                    {
                        p = "L" + p;
                    }
                    break;

                case TPTOKEN.TOK_PPNUM:
                case TPTOKEN.TOK_PPSTR:
                    return cv.str;

                case TPTOKEN.TOK_LSTR:
                case TPTOKEN.TOK_STR:
                    if (v == (int)TPTOKEN.TOK_STR)
                    {
                        p = "\'" + cv.str;
                    }
                    else
                    {
                        p = "L\'";
                        //            len = (cv->str.size / sizeof(nwchar_t)) - 1;
                        //            for(i=0;i<len;i++)
                        //                add_char(&cstr_buf, ((nwchar_t *)cv->str.data)[i]);
                    }
                    p = p + "\'";
                    break;

                case TPTOKEN.TOK_CFLOAT:
                    p = "<float>";
                    break;

                case TPTOKEN.TOK_CDOUBLE:
                    p = "<double>";
                    break;

                case TPTOKEN.TOK_CLDOUBLE:
                    p = "<long double>";
                    break;

                case TPTOKEN.TOK_LINENUM:
                    p = "<linenumber>";
                    break;

                /* above tokens have value, the ones below don't */
                case TPTOKEN.TOK_LT:
                    p = "<";
                    break;

                case TPTOKEN.TOK_GT:
                    p = ">";
                    break;

                case TPTOKEN.TOK_DOTS:
                    return "...";

                case TPTOKEN.TOK_A_SHL:
                    return "<<=";

                case TPTOKEN.TOK_A_SAR:
                    return ">>=";

                case TPTOKEN.TOK_EOF:
                    return "<eof>";

                default:
                    if (v < (int)TPTOKEN.TOK_IDENT)
                    {
                        /* search in two bytes table */
                        //            const unsigned char *q = tok_two_chars;
                        //            while (*q) {
                        //                if (q[2] == v) {
                        //                    *p++ = q[0];
                        //                    *p++ = q[1];
                        //                    *p = '\0';
                        //                    return cstr_buf.data;
                        //                }
                        //                q += 3;
                        //            }
                        if (v >= 127)
                        {
                            return string.Format("<{0}>", v.ToString("X2"));
                        }
                        if (v >= 0x20)
                        {
                            p = ((char)v).ToString();
                        }
                        else
                        {
                            p = v.ToString();
                        }
                    }
                    else if (v < tok_ident)
                    {
                        return table_ident[(v - (int)TPTOKEN.TOK_IDENT)].str;
                    }
                    else if (v >= Compiler.SYM_FIRST_ANOM)
                    {
                        /* special name for anonymous symbol */
                        p = string.Format("L.{0}", (v - Compiler.SYM_FIRST_ANOM));
                    }
                    else
                    {
                        /* should never happen */
                        return null;
                    }
                    break;
            }

            return p;
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

        /* parse number in null terminated string 'p' and return it in the current token */
        public void parse_number(string p)
        {
            int b;
            int t;
            int shift;
            int frac_bits;
            int s;
            int exp_val;
            int ch;
            char[] q = new char[1025];
            uint[] bn = new uint[2];
            double d;

            /* number */
            int qpos = 0;
            int ppos = 0;
            ch = p[ppos++];
            t = ch;
            ch = p[ppos++];
            q[qpos++] = (char)t;
            b = 10;
            if (t == '.')
            {
                //goto float_frac_parse;
            }
            else if (t == '0')
            {
                if (ch == 'x' || ch == 'X')
                {
                    qpos--;
                    ch = p[ppos++];
                    b = 16;
                }
                else if (ch == 'b' || ch == 'B')
                {
                    qpos--;
                    ch = p[ppos++];
                    b = 2;
                }
            }

            /* parse all digits. cannot check octal numbers at this stage because of floating point constants */
            while (true)
            {
                if (ch >= 'a' && ch <= 'f')
                    t = ch - 'a' + 10;
                else if (ch >= 'A' && ch <= 'F')
                    t = ch - 'A' + 10;
                else if (isnum(ch))
                    t = ch - '0';
                else
                    break;
                if (t >= b)
                    break;
                if (qpos >= 1024)
                {
                num_too_long:
                    tp.tp_error("number too long");
                }
                q[qpos++] = (char)ch;
                ch = p[ppos++];
            }

            if (ch == '.' ||
                ((ch == 'e' || ch == 'E') && b == 10) ||
                ((ch == 'p' || ch == 'P') && (b == 16 || b == 2)))
            {
            //            if (b != 10) {
            /* NOTE: strtox should support that for hexa numbers, but
            non ISOC99 libcs do not support it, so we prefer to do it by hand */
            /* hexadecimal or binary floats */
            /* XXX: handle overflows */
            //                *q = '\0';
            //                if (b == 16)
            //                    shift = 4;
            //                else 
            //                    shift = 1;
            //                bn_zero(bn);
            //                q = token_buf;
            //                while (1) {
            //                    t = *q++;
            //                    if (t == '\0') {
            //                        break;
            //                    } else if (t >= 'a') {
            //                        t = t - 'a' + 10;
            //                    } else if (t >= 'A') {
            //                        t = t - 'A' + 10;
            //                    } else {
            //                        t = t - '0';
            //                    }
            //                    bn_lshift(bn, shift, t);
            //                }
            //                frac_bits = 0;
            //                if (ch == '.') {
            //                    ch = *p++;
            //                    while (1) {
            //                        t = ch;
            //                        if (t >= 'a' && t <= 'f') {
            //                            t = t - 'a' + 10;
            //                        } else if (t >= 'A' && t <= 'F') {
            //                            t = t - 'A' + 10;
            //                        } else if (t >= '0' && t <= '9') {
            //                            t = t - '0';
            //                        } else {
            //                            break;
            //                        }
            //                        if (t >= b)
            //                            tcc_error("invalid digit");
            //                        bn_lshift(bn, shift, t);
            //                        frac_bits += shift;
            //                        ch = *p++;
            //                    }
            //                }
            //                if (ch != 'p' && ch != 'P')
            //                    expect("exponent");
            //                ch = *p++;
            //                s = 1;
            //                exp_val = 0;
            //                if (ch == '+') {
            //                    ch = *p++;
            //                } else if (ch == '-') {
            //                    s = -1;
            //                    ch = *p++;
            //                }
            //                if (ch < '0' || ch > '9')
            //                    expect("exponent digits");
            //                while (ch >= '0' && ch <= '9') {
            //                    exp_val = exp_val * 10 + ch - '0';
            //                    ch = *p++;
            //                }
            //                exp_val = exp_val * s;

                //                /* now we can generate the number */
            //                /* XXX: should patch directly float number */
            //                d = (double)bn[1] * 4294967296.0 + (double)bn[0];
            //                d = ldexp(d, exp_val - frac_bits);
            //                t = toup(ch);
            //                if (t == 'F') {
            //                    ch = *p++;
            //                    tok = TOK_CFLOAT;
            //                    /* float : should handle overflow */
            //                    tokc.f = (float)d;
            //                } else if (t == 'L') {
            //                    ch = *p++;
            //#ifdef TCC_TARGET_PE
            //                    tok = TOK_CDOUBLE;
            //                    tokc.d = d;
            //#else
            //                    tok = TOK_CLDOUBLE;
            //                    /* XXX: not large enough */
            //                    tokc.ld = (long double)d;
            //#endif
            //                } else {
            //                    tok = TOK_CDOUBLE;
            //                    tokc.d = d;
            //                }
            //            } else {
            //                /* decimal floats */
            //                if (ch == '.') {
            //                    if (q >= token_buf + STRING_MAX_SIZE)
            //                        goto num_too_long;
            //                    *q++ = ch;
            //                    ch = *p++;
            float_frac_parse:
                //                    while (ch >= '0' && ch <= '9') {
                //                        if (q >= token_buf + STRING_MAX_SIZE)
                //                            goto num_too_long;
                //                        *q++ = ch;
                //                        ch = *p++;
                //                    }
                //                }
                //                if (ch == 'e' || ch == 'E') {
                //                    if (q >= token_buf + STRING_MAX_SIZE)
                //                        goto num_too_long;
                //                    *q++ = ch;
                //                    ch = *p++;
                //                    if (ch == '-' || ch == '+') {
                //                        if (q >= token_buf + STRING_MAX_SIZE)
                //                            goto num_too_long;
                //                        *q++ = ch;
                //                        ch = *p++;
                //                    }
                //                    if (ch < '0' || ch > '9')
                //                        expect("exponent digits");
                //                    while (ch >= '0' && ch <= '9') {
                //                        if (q >= token_buf + STRING_MAX_SIZE)
                //                            goto num_too_long;
                //                        *q++ = ch;
                //                        ch = *p++;
                //                    }
                //                }
                //                *q = '\0';
                //                t = toup(ch);
                //                errno = 0;
                //                if (t == 'F') {
                //                    ch = *p++;
                //                    tok = TOK_CFLOAT;
                //                    tokc.f = strtof(token_buf, NULL);
                //                } else if (t == 'L') {
                //                    ch = *p++;
                //#ifdef TCC_TARGET_PE
                tok = (int)TPTOKEN.TOK_CDOUBLE;
                tokc.d = Convert.ToDouble(token_buf);
                //#else
                //                    tok = TOK_CLDOUBLE;
                //                    tokc.ld = strtold(token_buf, NULL);
                //#endif
                //                } else {
                //                    tok = TOK_CDOUBLE;
                //                    tokc.d = strtod(token_buf, NULL);
                //                }
                //            }
            }
            else
            {
                ulong n;
                ulong n1;
                int lcount;
                int ucount;
                bool ov = false;
                string p1;

                /* integer number */
                q[qpos] = '\0';
                qpos = 0;
                if (b == 10 && q[qpos] == '0')
                {
                    b = 8;
                    qpos++;
                }
                n = 0;
                while (true)
                {
                    t = q[qpos++];
                    /* no need for checks except for base 10 / 8 errors */
                    if (t == '\0')
                        break;
                    else if (t >= 'a')
                        t = t - 'a' + 10;
                    else if (t >= 'A')
                        t = t - 'A' + 10;
                    else
                        t = t - '0';
                    if (t >= b)
                        tp.tp_error("invalid digit");
                    n1 = n;
                    n = (n * (ulong)b) + (ulong)t;
                    /* detect overflow */
                    if ((n1 >= 0x1000000000000000UL) && ((n / (ulong)b) != n1))
                        ov = true;
                }

                /* Determine the characteristics (unsigned and/or 64bit) the type of
                the constant must have according to the constant suffix(es) */
                lcount = ucount = 0;
                p1 = p;
                for (; ; )
                {
                    t = toup((char)ch);
                    if (t == 'L')
                    {
                        if (lcount >= 2)
                            tp.tp_error("three 'l's in integer constant");
                        if ((lcount > 0) && (p[ppos - 1] != ch))
                            tp.tp_error("incorrect integer suffix: {0}", p1);
                        lcount++;
                        ch = p[ppos++];
                    }
                    else if (t == 'U')
                    {
                        if (ucount >= 1)
                            tp.tp_error("two 'u's in integer constant");
                        ucount++;
                        ch = p[ppos++];
                    }
                    else
                    {
                        break;
                    }
                }

                /* Determine if it needs 64 bits and/or unsigned in order to fit */
                if (ucount == 0 && b == 10)
                {
                    if (lcount <= 1)
                    {
                        if (n >= 0x80000000U)
                            lcount = 1 + 1;
                    }
                    if (n >= 0x8000000000000000UL)
                    {
                        ov = true;
                        ucount = 1;
                    }
                }
                else
                {
                    if (lcount <= 1)
                    {
                        if (n >= 0x100000000UL)
                            lcount = 1 + 1;
                        else if (n >= 0x80000000U)
                            ucount = 1;
                    }
                    if (n >= 0x8000000000000000UL)
                        ucount = 1;
                }

                if (ov)
                    tp.tp_warning("integer constant overflow");

                tok = (int)TPTOKEN.TOK_CINT;
                if (lcount != 0)
                {
                    tok = (int)TPTOKEN.TOK_CLONG;
                    if (lcount == 2)
                        tok = (int)TPTOKEN.TOK_CLLONG;
                }
                if (ucount != 0)
                    ++tok; /* TOK_CU... */
                tokc.i = n;
            }
            if (ch != 0)
                tp.tp_error("invalid number\n");
        }

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
                        if (((parseFlags & PARSE_FLAG_LINEFEED) != 0) && !((tokenFlags & TOK_FLAG_EOF) != 0))
                        {
                            tokenFlags |= TOK_FLAG_EOF;
                            tok = (int)TPTOKEN.TOK_LINEFEED;
                            goto keep_tok_flags;
                        }
                        else if (!((parseFlags & PARSE_FLAG_PREPROCESS) != 0))
                        {
                            tok = (int)TPTOKEN.TOK_EOF;
                        }
                        else if (tp.ifdef_stack_ptr != 0)
                        {
                            tp.tp_error("missing #endif");
                        }
                        else if (tp.include_stack_ptr == 0)
                        {
                            /* no include left : end of file. */
                            tok = (int)TPTOKEN.TOK_EOF;
                        }
                        else
                        {
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
                    tok = (int)TPTOKEN.TOK_LINEFEED;
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
                    tokc.str = tokcstr;             //set const data
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

            Console.Out.WriteLine("token = {0} {1}", tok.ToString("X2"), get_tok_str(tok, tokc));
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

            /* convert preprocessor tokens into C tokens */
            if (tok == (int)TPTOKEN.TOK_PPNUM)
            {
                if ((parseFlags & PARSE_FLAG_TOK_NUM) != 0)
                    parse_number(tokc.str);
            }
            else if (tok == (int)TPTOKEN.TOK_PPSTR)
            {
                //        if (parse_flags & PARSE_FLAG_TOK_STR)
                //            parse_string((char *)tokc.str.data, tokc.str.size - 1);
            }
        }

        public void unget_tok() { }

        //---------------------------------------------------------------------

        public void preprocess_start(bool isAsm)
        {
            comp = tp.comp;
            total_lines = 0;

            //clear stacks
            //s1->include_stack_ptr = s1->include_stack;
            //s1->ifdef_stack_ptr = s1->ifdef_stack;
            //file->ifdef_stack_ptr = s1->ifdef_stack_ptr;

            pp_expr = 0;
            pp_counter = 0;
            pp_debug_tok = pp_debug_symv = 0;

            //pp_once++;
            comp.pvtop = comp.vtop = 0;
            //s1->pack_stack[0] = 0;
            //s1->pack_stack_ptr = s1->pack_stack;

            //set_idnum('$', s1->dollars_in_identifiers ? IS_ID : 0);
            //set_idnum('.', is_asm ? IS_ID : 0);

            //cstr_new(&cstr);
            //cstr_cat(&cstr, "\"", -1);
            //cstr_cat(&cstr, file->filename, -1);
            //cstr_cat(&cstr, "\"", 0);
            //tcc_define_symbol(s1, "__BASE_FILE__", cstr.data);

            //cstr_reset(&cstr);
            //for (i = 0; i < s1->nb_cmd_include_files; i++)
            //{
            //    cstr_cat(&cstr, "#include \"", -1);
            //    cstr_cat(&cstr, s1->cmd_include_files[i], -1);
            //    cstr_cat(&cstr, "\"\n", -1);
            //}

            //if (cstr.size)
            //{
            //    *s1->include_stack_ptr++ = file;

            //    //read source code from cmd line?
            //    tcc_open_bf(s1, "<command line>", cstr.size);
            //    memcpy(file->buffer, cstr.data, cstr.size);
            //}
            //cstr_free(&cstr);

            //if (is_asm)
            //    tcc_define_symbol(s1, "__ASSEMBLER__", NULL);

            parseFlags = isAsm ? PARSE_FLAG_ASM_FILE : 0;
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

        public int tp_preprocess()
        {
            //                BufferedFile **iptr;
            //    int token_seen, spcs, level;
            //    const char *p;
            //    char white[400];

            parseFlags = PARSE_FLAG_PREPROCESS
            | (parseFlags & PARSE_FLAG_ASM_FILE)
            | PARSE_FLAG_LINEFEED
            | PARSE_FLAG_SPACES
            | PARSE_FLAG_ACCEPT_STRAYS
            ;

            /* Credits to Fabrice Bellard's initial revision to demonstrate its
            capability to compile and run itself, provided all numbers are
            given as decimals. tcc -E -P10 will do. */
            //    if (s1->Pflag == LINE_MACRO_OUTPUT_FORMAT_P10)
            //        parse_flags |= PARSE_FLAG_TOK_NUM, s1->Pflag = 1;

            //#ifdef PP_BENCH
            //    /* for PP benchmarks */
            //    do next(); while (tok != TOK_EOF);
            //    return 0;
            //#endif

            //    if (s1->dflag & 1) {
            //        pp_debug_builtins(s1);
            //        s1->dflag &= ~1;
            //    }


            //    token_seen = TOK_LINEFEED, spcs = 0;
            for (; ; )
            {
                //        iptr = s1->include_stack_ptr;
                next();
                if (tok == (int)TPTOKEN.TOK_EOF)
                    break;

                //        level = s1->include_stack_ptr - iptr;
                //        if (level) {
                //            if (level > 0)
                //                pp_line(s1, *iptr, 0);
                //            pp_line(s1, file, level);
                //        }
                //        if (s1->dflag & 7) {
                //            pp_debug_defines(s1);
                //            if (s1->dflag & 4)
                //                continue;
                //        }

                //        if (is_space(tok)) {
                //            if (spcs < sizeof white - 1)
                //                white[spcs++] = tok;
                //            continue;
                //        } else if (tok == TOK_LINEFEED) {
                //            spcs = 0;
                //            if (token_seen == TOK_LINEFEED)
                //                continue;
                //            ++file->line_ref;
                //        } else if (token_seen == TOK_LINEFEED) {
                //            pp_line(s1, file, 0);
                //        } else if (spcs == 0 && pp_need_space(token_seen, tok)) {
                //            white[spcs++] = ' ';
                //        }

                //        white[spcs] = 0, fputs(white, s1->ppfp), spcs = 0;
                //        fputs(p = get_tok_str(tok, &tokc), s1->ppfp);
                //        token_seen = pp_check_he0xE(tok, p);
            }
            return 0;

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
        public Sym sym_label; 	        /* direct pointer to label */
        public Sym sym_struct; 	        /* direct pointer to structure */
        public Sym sym_identifier;      /* direct pointer to identifier */

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

    //-------------------------------------------------------------------------

    //constant value
    public class CValue
    {
        public double ld;       //double & long double are the same in VC++ - 8 bytes
        public double d;
        public float f;         //4 bytes
        public ulong i;         //8 bytes
        public string str;
        public int[] tab;

        public CValue(CValue that)
        {
            this.ld = that.ld;
            this.d = that.d;
            this.f = that.f;
            this.i = that.i;
            this.str = String.Copy(that.str);
            this.tab = (int[])that.tab.Clone();
        }
    }
}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");