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
        public Generator gen;

        public List<BufferedFile> infiles;

        public int ifdef_stack_ptr;

        public int verbose;			/* if true, display some information during compilation */
        public int nostdinc;			/* if true, no standard headers are added */
        public int nostdlib;			/* if true, no standard libraries are added */
        public int nocommon;			/* if true, do not use common symbols for .bss data */
        public int static_link;		/* if true, static linking is performed */
        public int rdynamic;			/* if true, all symbols are exported */
        public int symbolic;			/* if true, resolve symbols in the current module first */
        public bool alacarte_link;		/* if true, only link in referenced objects from archive */

        public string tcc_lib_path;		/* CONFIG_TCCDIR or -B option */
        public string soname;			/* as specified on the command line (-soname) */
        public string rpath;			/* as specified on the command line (-Wl,-rpath=) */
        public int enable_new_dtags;	/* ditto, (-Wl,--enable-new-dtags) */

        /* output type, see TCC_OUTPUT_XXX */
        public OUTPUTTYPE output_type;

        /* output format, see TCC_OUTPUT_FORMAT_xxx */
        public OUTPUTFORMAT output_format;

        /* C language options */
        public bool char_is_unsigned;
        public bool leading_underscore;
        public bool ms_extensions;			/* allow nested named struct w/o identifier behave like unnamed */
        public bool dollars_in_identifiers;	/* allows '$' char in identifiers */
        public bool ms_bitfields;			/* if true, emulate MS algorithm for aligning bitfields */

        /* warning switches */
        public int warn_write_strings;
        public int warn_unsupported;
        public int warn_error;
        public int warn_none;
        public int warn_implicit_function_declaration;
        public int warn_gcc_compat;

        /* compile with debug symbol (and use them if error during execution) */
        public int do_debug;

        /* compile with built-in memory and bounds checker */
        public int do_bounds_check;

        //#ifdef TCC_TARGET_ARM
        //    enum float_abi float_abi; /* float ABI of the generated code*/
        //#endif
        //    int run_test; /* nth test to run with -dt -run */

        public int text_addr;           /* address of text section */
        public int has_text_addr;

        public int section_align; /* section alignment */

        //    char *init_symbol; /* symbols to call at load-time (not used currently) */
        //    char *fini_symbol; /* symbols to call at unload-time (not used currently) */

        //#ifdef TCC_TARGET_I386
        //    int seg_size; /* 32. Can be 16 with i386 assembler (.code16) */
        //#endif
        //#ifdef TCC_TARGET_X86_64
        //    int nosse; /* For -mno-sse support. */
        //#endif

        /* array of all loaded dlls (including those referenced by loaded dlls) */
        //    DLLReference **loaded_dlls;
        //    int nb_loaded_dlls;

        public static char[] PATHSEP = { ';' };

        /* include paths */
        public List<string> include_paths;
        public int nb_include_paths;

        public List<string> sysinclude_paths;
        public int nb_sysinclude_paths;

        /* library paths */
        public List<string> library_paths;
        public int nb_library_paths;

        /* crt?.o object path */
        public List<string> crt_paths;
        public int nb_crt_paths;

        /* -include files */
        public List<string> cmd_include_files;
        public int nb_cmd_include_files;

        /* error handling */
        //    void *error_opaque;
        //    void (*error_func)(void *opaque, const char *msg);
        //    int error_set_jmp_enabled;
        //    jmp_buf error_jmp_buf;
        public int nb_errors;

        //    /* output file for preprocessing (-E) */
        //    FILE *ppfp;
        //    enum {
        //    LINE_MACRO_OUTPUT_FORMAT_GCC,
        //    LINE_MACRO_OUTPUT_FORMAT_NONE,
        //    LINE_MACRO_OUTPUT_FORMAT_STD,
        //    LINE_MACRO_OUTPUT_FORMAT_P10 = 11
        //    } Pflag; /* -P switch */

        public int dflag; /* -dX value */

        //    /* for -MD/-MF: collected dependencies for this compilation */
        //    char **target_deps;
        //    int nb_target_deps;

        /* compilation */
        public int INCLUDE_STACK_SIZE = 32;
        public BufferedFile[] include_stack;
        public int include_stack_ptr;


        //    int ifdef_stack[IFDEF_STACK_SIZE];
        //    int *ifdef_stack_ptr;

        //    /* included files enclosed with #ifndef MACRO */
        //    int cached_includes_hash[CACHED_INCLUDES_HASH_SIZE];
        //    CachedInclude **cached_includes;
        //    int nb_cached_includes;

        //    /* #pragma pack stack */
        //    int pack_stack[PACK_STACK_SIZE];
        //    int *pack_stack_ptr;
        //    char **pragma_libs;
        //    int nb_pragma_libs;

        //    /* inline functions are stored as token lists and compiled last only if referenced */
        //    struct InlineFunc **inline_fns;
        //    int nb_inline_fns;

        /* sections */
        public List<Section> sections;
        public int nb_sections;                 /* number of sections, including first dummy section */

        public List<Section> priv_sections;
        public int nb_priv_sections;                   /* number of private sections */

        //    /* got & plt handling */
        //    Section *got;
        //    Section *plt;

        //    /* temporary dynamic symbol sections (for dll loading) */
        //    Section *dynsymtab_section;

        //    /* exported dynamic symbol section */
        //    Section *dynsym;

        /* copy of the global symtab_section variable */
        public Section symtab;

        //    /* extra attributes (eg. GOT/PLT value) for symtab symbols */
        //    struct sym_attr *sym_attrs;
        //    int nb_sym_attrs;

        //#ifdef TCC_TARGET_PE
        //    /* PE info */
        //    int pe_subsystem;
        //    unsigned pe_characteristics;
        //    unsigned pe_file_align;
        //    unsigned pe_stack_size;
        //    addr_t pe_imagebase;
        //# ifdef TCC_TARGET_X86_64
        //    Section *uw_pdata;
        //    int uw_sym;
        //    unsigned uw_offs;
        //# endif
        //#endif

        //#ifdef TCC_IS_NATIVE
        //    const char *runtime_main;
        //    void **runtime_mem;
        //    int nb_runtime_mem;
        //#endif

        /* used by main and tcc_parse_args only */
        public List<FileSpec> files;        /* files seen on command line */
        public int nb_files;				        /* number thereof */
        public int nb_libraries;			        /* number of libs thereof */
        public FILETYPE filetype;
        public FileStream outfile;
        public string outFilename;          /* output filename */
        public int option_r;				        /* option -r */
        public int do_bench;				    /* option -bench */
        public int gen_deps;				    /* option -MD  */
        public string deps_outfile;			/* option -MF */
        public int option_pthread;			    /* -pthread option */
        //    int argc;
        //    char **argv;

        public const int AFF_PRINT_ERROR = 0x10;        /* print error if file not found */
        public const int AFF_REFERENCED_DLL = 0x20;     /* load a referenced dll from another dll */
        public const int AFF_TYPE_BIN = 0x40;           /* file to add is binary */

        //- tcc.c -------------------------------------------------------------

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
            gen = new Generator(this);


            files = new List<FileSpec>();
            infiles = new List<BufferedFile>();

            include_stack = new BufferedFile[INCLUDE_STACK_SIZE];
            include_stack_ptr = 0;

            ifdef_stack_ptr = 0;

            verbose = 0;

            output_type = OUTPUTTYPE.TP_OUTPUT_NONE;

            do_debug = 0;
            section_align = 0;

            include_paths = new List<string>();
            nb_include_paths = 0;
            sysinclude_paths = new List<string>();
            nb_sysinclude_paths = 0;
            library_paths = new List<string>();
            nb_library_paths = 0;
            crt_paths = new List<string>();
            nb_crt_paths = 0;
            cmd_include_files = new List<string>();
            nb_cmd_include_files = 0;

            sections = new List<Section>();
            nb_sections = 0;
            priv_sections = new List<Section>();
            nb_priv_sections = 0;
            Section.initSection(this);
        }

        public void print_dirs() { }
        public void print_search_dirs() { }
        public void set_environment() { }
        public void default_outputfile() { }

        public int getclock_ms()
        {
            return Environment.TickCount;
        }

        //tcc's main
        public int compile(string[] args)
        {
            int ret = 0;
            Options.RETCODE opt;
            int n = 0;
            int t = 0;
            int start_time = 0;
            //    const char *first_file;
            //    int argc; char **argv;
            //    FILE *ppfp = stdout;

            //redo:
            opt = Options.tp_parse_args(this, args);         //parse options


            //    if ((n | t) == 0) 
            //    {
            //        if (opt == OPT_HELP)
            //            return printf(help), 1;
            //        if (opt == OPT_HELP2)
            //            return printf(help2), 1;

            //                if (opt == OPT_M32 || opt == OPT_M64)
            //                    tcc_tool_cross(s, argv, opt); /* never returns */

            //        if (s->verbose)
            //            printf(version);

            //                if (opt == OPT_AR)
            //                    return tcc_tool_ar(s, argc, argv);
            //        #ifdef TCC_TARGET_PE
            //                if (opt == OPT_IMPDEF)
            //                    return tcc_tool_impdef(s, argc, argv);
            //        #endif

            //        if (opt == OPT_V)
            //            return 0;

            //        if (opt == OPT_PRINT_DIRS) {
            //            /* initialize search dirs */
            //            set_environment(s);
            //            tcc_set_output_type(s, TCC_OUTPUT_MEMORY);
            //            print_search_dirs(s);
            //            return 0;
            //        }

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

            //        } else if (s->output_type == TCC_OUTPUT_OBJ && !s->option_r) {
            //            if (s->nb_libraries)
            //                tcc_error("cannot specify libraries with -c");
            //            if (n > 1 && s->outfile)
            //                tcc_error("cannot specify output file with -c many files");
            //        } else {
            //            if (s->option_pthread)
            //                tcc_set_options(s, "-lpthread");
            //        }

            if (do_bench != 0)
                start_time = getclock_ms();			//get start time
            //    }

            //    set_environment(s);
            //    if (s->output_type == 0)
            //        s->output_type = TCC_OUTPUT_EXE;
            //    tcc_set_output_type(s, s->output_type);
            //    s->ppfp = ppfp;

            //    if ((s->output_type == TCC_OUTPUT_MEMORY
            //        || s->output_type == TCC_OUTPUT_PREPROCESS) && (s->dflag & 16))
            //        s->dflag |= t ? 32 : 0, s->run_test = ++t, n = s->nb_files;


            /* compile or add each files or library */
            foreach (FileSpec filespec in files)
            {
                filetype = filespec.ftype;


                //        s->alacarte_link = f->alacarte;

                //        if (f->type == AFF_TYPE_LIB) {
                //            if (tcc_add_library_err(s, f->name) < 0)
                //                ret = 1;
                //        } else {

                if (verbose == 1)
                {
                    Console.Out.WriteLine("-> {0}\n", filespec.name);
                }

                //            if (!first_file)
                //                first_file = f->name;

                if (tp_add_file(filespec.name) < 0)
                {
                    ret = 1;
                }
                //        }
                //        s->filetype = 0;
                //        s->alacarte_link = 1;
                //        if (--n == 0 || ret || (s->output_type == TCC_OUTPUT_OBJ && !s->option_r))
                //            break;
            }

            //        if (s->run_test) {
            //            t = 0;
            //        } else if (s->output_type == TCC_OUTPUT_PREPROCESS) {
            //            ;
            //        } else if (0 == ret) {
            //            if (s->output_type == TCC_OUTPUT_MEMORY) {
            //    #ifdef TCC_IS_NATIVE
            //                ret = tcc_run(s, argc, argv);
            //    #endif
            //            } else {
            //                if (!s->outfile)
            //                    s->outfile = default_outputfile(s, first_file);
            if (Section.tp_output_file(this, outFilename) != 0)
                ret = 1;
            //                else if (s->gen_deps)
            //                    gen_makedeps(s, s->outfile, s->deps_outfile);
            //            }
            //        }

            //    if (s->do_bench && (n | t | ret) == 0)
            //        tcc_print_stats(s, getclock_ms() - start_time);		//get end time, dump stats
            //    tcc_delete(s);

            //    if (ret == 0 && n)
            //        goto redo; /* compile more files with -c */
            //    if (t)
            //        goto redo; /* run more tests with -dt -run */

            //    if (ppfp && ppfp != stdout)
            //        fclose(ppfp);

            return ret;

        }

        //libtcc.c
        public void normalize_slashes() { }
        public void tp_set_lib_path_w32() { }
        public void tp_add_systemdir() { }

        //---------------------------------------------------------------------

        /* copy a string and truncate it. */
        public string pstrcpy(string buf, int buf_size, string s)
        {
            buf = s.Substring(0, buf_size);
            return buf;
        }

        /* strcat and truncate. */
        public string pstrcat(string buf, int buf_size, string s)
        {
            buf += s;
            return buf;
        }

        public void pstrncpy() { }
        public void tp_fileextension() { }

        public void tp_realloc() { }
        
        public void tp_split_path(List<string> pathList, ref int pathCount, string instr)
        { 
            string[] paths = instr.Split(PATHSEP);
            pathList.AddRange(paths);
            pathCount = pathList.Count;
        }

        //- messages ------------------------------------------------------------------

        public void strcat_vprintf() { }
        public void strcat_printf() { }
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
            bf.prev = prep.curFile;
            prep.curFile = bf;
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
            prep.curFile.fs = fs;

            return 0;
        }

        public void tp_close()
        {
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
                comp.tp_compile();
            }

            return nb_errors;
        }

        public void tp_compile_string() { }

        public void tp_define_symbol() { }
        public void tp_undefine_symbol() { }

        public void tp_cleanup() { }
        public void tp_new() { }
        public void tp_delete() { }

        //- file mgmt -----------------------------------------------------------------

        public void tp_set_output_type() 
        {
        }

        public int tp_add_include_path(string pathname)
        {
            tp_split_path(include_paths, ref nb_include_paths, pathname);
            return 0;
        }

        public int tp_add_sysinclude_path(string pathname)
        {
            tp_split_path(sysinclude_paths, ref nb_sysinclude_paths, pathname);
            return 0;
        }

        public int tp_add_file_internal(String filename, int flags)
        {
            /* open the file */
            int ret = tp_open(filename);
            if (ret < 0)
            {
                if ((flags & AFF_PRINT_ERROR) != 0)
                    tp_error_noabort("file '%s' not found", filename);
                return ret;
            }


            //    /* update target deps */
            //    dynarray_add(&s1->target_deps, &s1->nb_target_deps, tcc_strdup(filename));

            //    if (flags & AFF_TYPE_BIN) {
            //        ElfW(Ehdr) ehdr;
            //        int fd, obj_type;

            //        fd = file->fd;
            //        obj_type = tcc_object_type(fd, &ehdr);
            //        lseek(fd, 0, SEEK_SET);

            //#ifdef TCC_TARGET_MACHO
            //        if (0 == obj_type && 0 == strcmp(tcc_fileextension(filename), ".dylib"))
            //            obj_type = AFF_BINTYPE_DYN;
            //#endif

            //        switch (obj_type) {
            //        case AFF_BINTYPE_REL:
            //            ret = tcc_load_object_file(s1, fd, 0);
            //            break;
            //#ifndef TCC_TARGET_PE
            //        case AFF_BINTYPE_DYN:
            //            if (s1->output_type == TCC_OUTPUT_MEMORY) {
            //                ret = 0;
            //#ifdef TCC_IS_NATIVE
            //                if (NULL == dlopen(filename, RTLD_GLOBAL | RTLD_LAZY))
            //                    ret = -1;
            //#endif
            //            } else {
            //                ret = tcc_load_dll(s1, fd, filename,
            //                    (flags & AFF_REFERENCED_DLL) != 0);
            //            }
            //            break;
            //#endif
            //        case AFF_BINTYPE_AR:
            //            ret = tcc_load_archive(s1, fd);
            //            break;
            //#ifdef TCC_TARGET_COFF
            //        case AFF_BINTYPE_C67:
            //            ret = tcc_load_coff(s1, fd);
            //            break;
            //#endif
            //        default:
            //#ifdef TCC_TARGET_PE
            //            ret = pe_load_file(s1, filename, fd);
            //#else
            //            /* as GNU ld, consider it is an ld script if not recognized */
            //            ret = tcc_load_ldscript(s1);
            //#endif
            //            if (ret < 0)
            //                tcc_error_noabort("unrecognized file type");
            //            break;
            //        }
            //    } else {
            ret = tp_compile();
            //    }

            tp_close();
            return ret;
        }

        public int tp_add_file(String filename)
        {
            FILETYPE ftype = this.filetype;
            int flags = AFF_PRINT_ERROR;
            if (ftype == FILETYPE.NONE)
            {
                /* use a file extension to detect a filetype */
                string ext = Path.GetExtension(filename);
                switch (ext)
                {
                    //        if (!strcmp(ext, "S"))
                    //            filetype = AFF_TYPE_ASMPP;
                    //        else if (!strcmp(ext, "s"))
                    //            filetype = AFF_TYPE_ASM;
                    //        else if (!PATHCMP(ext, "c") || !PATHCMP(ext, "i"))
                    //            filetype = AFF_TYPE_C;
                    //        else
                    //            flags |= AFF_TYPE_BIN;

                    case ".c":
                        ftype = FILETYPE.C;
                        break;

                    default:
                        ftype = FILETYPE.C;
                        break;
                }
                this.filetype = ftype;
            }
            return tp_add_file_internal(filename, flags);
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
    }

    //-------------------------------------------------------------------------

    public enum FILETYPE
    {
        NONE,
        C,
        ASM,
        ASMPP,
        LIB
    }

    public class FileSpec
    {
        public FILETYPE ftype;
        public bool alacarte;
        public string name;

        public FileSpec(string _name, bool _alacarte, FILETYPE _ftype)
        {
            name = _name;
            alacarte = _alacarte;
            ftype = _ftype;
        }
    }
}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");