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

    public class TidePool
    {
        static TidePool tidepool;

        public Preprocessor prep;
        public Compiler comp;

        public List<FileSpec> files;
        public OUTPUTTYPE output_type;
        public string outFilename;
        public FileStream outfile;
        public FILETYPE filetype;

        public List<BufferedFile> infiles;

        /* compilation */
        public int INCLUDE_STACK_SIZE = 32;
        public BufferedFile[] include_stack;
        public int include_stack_ptr;

        public int ifdef_stack_ptr;

        public int verbose;

        public int nb_errors;

        public const int AFF_PRINT_ERROR = 0x10;        /* print error if file not found */
        public const int AFF_REFERENCED_DLL = 0x20;     /* load a referenced dll from another dll */
        public const int AFF_TYPE_BIN = 0x40;           /* file to add is binary */


        static void Main(string[] args)
        {
            tidepool = new TidePool();
            tidepool.compile(args);
            Console.Out.WriteLine("done");
        }

        public TidePool()
        {
            prep = new Preprocessor(this);
            comp = new Compiler(this);

            files = new List<FileSpec>();
            infiles = new List<BufferedFile>();

            include_stack = new BufferedFile[INCLUDE_STACK_SIZE];
            include_stack_ptr = 0;

            ifdef_stack_ptr = 0;

            verbose = 0;
        }

        private void compile(string[] args)
        {
            Options.parseOptions(this, args);         //parse options

            if (files.Count == 0)
            {
                tp_error("no input files\n");
            }
            if ((output_type == OUTPUTTYPE.TP_OUTPUT_PREPROCESS) && (outFilename != null))
            {
                try
                {
                    outfile = File.Open(outFilename, FileMode.Create);      //open output file
                }
                catch (Exception e)
                {
                    tp_error("could not write '{0}'", outFilename);
                }
            }

            foreach (FileSpec filespec in files)
            {
                filetype = filespec.ftype;
                if (verbose == 1)
                {
                    Console.Out.WriteLine("-> {0}\n", filespec.name);
                }
                tp_add_file(filespec.name);
            }
        }


        public void normalize_slashes() { }
        public void tp_set_lib_path_w32() { }
        public void tp_add_systemdir() { }
        public void pstrcpy() { }
        public void pstrcat() { }
        public void pstrncpy() { }
        public void tp_basename() { }
        public void tp_fileextension() { }

        public void tp_malloc() { }
        public void tp_mallocz() { }
        public void tp_realloc() { }
        public void dynarray_add() { }
        public void dynarray_reset() { }
        public void tp_split_path() { }
        public void strcat_vprintf() { }
        public void strcat_printf() { }

        //- error handling ----------------------------------------------------

        public void error1() { }

        public void tp_set_error_func() { }

        public void tp_error_noabort(string msg, params string[] values)
        {
            Console.Out.WriteLine(msg, values);
        }

        public void tp_error(string msg, params string[] values)
        {
            Console.Out.WriteLine(msg, values);
            System.Environment.Exit(1);
        }

        public void tp_warning(string msg, params string[] values) 
        {
            Console.Out.WriteLine(msg, values);
        }

        //- file i/o ----------------------------------------------------------

        public const int IO_BUF_SIZE = 8192;

        public void tp_open_bf(string filename, int initlen)
        {
            int buflen = (initlen != 0) ? initlen : IO_BUF_SIZE;
            BufferedFile bf = new BufferedFile(this, filename, initlen, buflen);
            infiles.Add(bf);
        }

        public void tp_close()
        {
        }

        public int tp_open(string filename)
        {
            FileStream fs = null;
            try
            {
                fs = File.Open(filename, FileMode.Open, FileAccess.Read);
            }
            catch (Exception e)
            {
                return -1;
            }
            tp_open_bf(filename, 0);
            infiles[infiles.Count - 1].fs = fs;

            return 0;
        }

        public int tp_compile()
        {
            bool is_asm = (filetype == FILETYPE.ASM) || (filetype == FILETYPE.ASMPP);
            nb_errors = 0;

            prep.preprocess_start(is_asm);
            if (output_type == OUTPUTTYPE.TP_OUTPUT_PREPROCESS)
            {
                prep.tp_preprocess();
            }
            else if (is_asm)
            {
                //tcc_assemble(s1, filetype == AFF_TYPE_ASMPP);
            }
            else
            {
                comp.compile();
            }

            return nb_errors;
        }

        public void tp_compile_string() { }

        public void tp_define_symbol() { }
        public void tp_undefine_symbol() { }

        public void tp_cleanup() { }
        public void tp_new() { }
        public void tp_delete() { }

        public void tp_set_output_type() { }
        public void tp_add_include_path() { }
        public void tp_add_sysinclude_path() { }

        public int tp_add_file_internal(String filename, int flags)
        {
            int ret = tp_open(filename);
            if (ret < 0)
            {
                if ((flags & AFF_PRINT_ERROR) != 0)
                    tp_error_noabort("file '%s' not found", filename);
                return ret;
            }
            ret = tp_compile();
            tp_close();
            return ret;
        }

        public void tp_add_file(String filename)
        {
            FILETYPE ftype = this.filetype;
            int flags = AFF_PRINT_ERROR;
            if (ftype == FILETYPE.NONE)
            {
                string ext = Path.GetExtension(filename);
                switch (ext)
                {
                    case ".c":
                        ftype = FILETYPE.C;
                        break;

                    default:
                        ftype = FILETYPE.C;
                        break;
                }
                this.filetype = ftype;
            }
            tp_add_file_internal(filename, flags);
        }

        public void tp_add_library_path() { }
        public void tp_add_library_internal() { }
        public void tp_add_dll() { }
        public void tp_add_crt() { }
        public void tp_add_library() { }
        public void tp_add_library_err() { }
        public void tp_add_pragma_libs() { }
        public void tp_add_symbol() { }
        public void tp_set_lib_path() { }

        //options
        public void no_flag() { }
        public void set_flag() { }
        public void strstart() { }
        public void link_option() { }
        public void skip_linker_arg() { }
        public void copy_linker_arg() { }
        public void tp_set_linker() { }
        public void parse_option_D() { }
        public void args_parser_add_file() { }
        public void args_parser_make_argv() { }
        public void args_parser_listfile() { }
        public void tp_parse_args() { }
        public void tp_set_options() { }
        public void tp_print_stats() { }

    }

    public enum FILETYPE { NONE, C, ASM, ASMPP, LIB }

    public class FileSpec
    {
        public string name;
        public FILETYPE ftype;

        public FileSpec(string _name, FILETYPE _ftype)
        {
            name = _name;
            ftype = _ftype;
        }
    }
}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");