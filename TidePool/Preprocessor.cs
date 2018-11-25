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
    class Preprocessor
    {
        public int tok;

        bool parseFlagSpaces;

        List<BufferedFile> files;

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
        public void get_tok_str() { }
        public void handle_eob() { }
        public void inp() { }
        public void handle_stray_noerror() { }
        public void handle_stray() { }
        public void handle_stray1() { }
        public void minp() { }
        public void parse_line_comment() { }
        public void parse_comment() { }
        public void set_idnum() { }
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
            BufferedFile file = files[files.Count-1];
            int p = file.buf_ptr;
            char c = file.buffer[p];
            switch (c)
            {
                case ' ':
                case '\t':
                case '\f':
                case '\v':
                case '\r':

                case '\\':

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

                    tok = c;
                    p++;
                    break;

                default:
                    break;
            

            }
            file.buf_ptr = p;			//update pos buf

            Console.Out.WriteLine("token = {0} {1}\n", tok); //get_tok_str(tok, &tokc));

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
            if (parseFlagSpaces)
            {
                next_nomacro_spc();
            }
            else
            {
                next_nomacro();
            }
        }

        public void unget_tok() { }

        public void preprocess_start() { }
        public void preprocess_end() { }

        public void tccpp_new() { }
        public void tccpp_delete() { }
        public void tok_print() { }
        public void pp_line() { }
        public void define_print() { }
        public void pp_debug_defines() { }
        public void pp_debug_builtins() { }
        public void pp_need_space() { }
        public void pp_check_he0xE() { }
        public void tcc_preprocess() { }
    }

//-----------------------------------------------------------------------------

    public class BufferedFile
    {
        public int buf_ptr;
        public int buf_end;
        //public File file;

        public int line_num;
        public int line_ref;
        public int ifndef_macro;
        public int ifndef_macro_saved;
        public int ifdef_stack_ptr;
        public int include_next_index;

        public string filename;
        public string true_filename;

        public char[] unget;

        public char[] buffer;
    }
}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");