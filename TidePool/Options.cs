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
    public class Options
    {
        //public static TidePool tp;

        /* tcc_parse_args return codes: */
        public enum RETCODE
        {
            OPT_HELP = 1,
            OPT_HELP2 = 2,
            OPT_V = 3,
            OPT_PRINT_DIRS = 4,
            OPT_AR = 5,
            OPT_IMPDEF = 6,
            OPT_M32 = 32,
            OPT_M64 = 64
        }

        //from libtcc.h
        public const int TP_OPTION_HAS_ARG = 1;
        public const int TP_OPTION_NOSEP = 2;

        public static TPOption[] optionsTable = new TPOption[] { 
                new TPOption( "h", TPOPTIONS.TP_OPTION_HELP, 0 ),
                new TPOption( "-help", TPOPTIONS.TP_OPTION_HELP, 0 ),
                new TPOption( "?", TPOPTIONS.TP_OPTION_HELP, 0 ),
                new TPOption( "hh", TPOPTIONS.TP_OPTION_HELP2, 0 ),
                new TPOption( "v", TPOPTIONS.TP_OPTION_v, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "I", TPOPTIONS.TP_OPTION_I, TP_OPTION_HAS_ARG ),
                new TPOption( "D", TPOPTIONS.TP_OPTION_D, TP_OPTION_HAS_ARG ),
                new TPOption( "U", TPOPTIONS.TP_OPTION_U, TP_OPTION_HAS_ARG ),
                new TPOption( "P", TPOPTIONS.TP_OPTION_P, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "L", TPOPTIONS.TP_OPTION_L, TP_OPTION_HAS_ARG ),
                new TPOption( "B", TPOPTIONS.TP_OPTION_B, TP_OPTION_HAS_ARG ),
                new TPOption( "l", TPOPTIONS.TP_OPTION_l, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "bench", TPOPTIONS.TP_OPTION_bench, 0 ),
                new TPOption( "bt", TPOPTIONS.TP_OPTION_bt, TP_OPTION_HAS_ARG ),
                new TPOption( "b", TPOPTIONS.TP_OPTION_b, 0 ),
                new TPOption( "g", TPOPTIONS.TP_OPTION_g, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "c", TPOPTIONS.TP_OPTION_c, 0 ),
                new TPOption( "dumpversion", TPOPTIONS.TP_OPTION_dumpversion, 0),
                new TPOption( "d", TPOPTIONS.TP_OPTION_d, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "static", TPOPTIONS.TP_OPTION_static, 0 ),
                new TPOption( "std", TPOPTIONS.TP_OPTION_std, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "shared", TPOPTIONS.TP_OPTION_shared, 0 ),
                new TPOption( "soname", TPOPTIONS.TP_OPTION_soname, TP_OPTION_HAS_ARG ),
                new TPOption( "o", TPOPTIONS.TP_OPTION_o, TP_OPTION_HAS_ARG ),
                new TPOption( "-param", TPOPTIONS.TP_OPTION_param, TP_OPTION_HAS_ARG ),
                new TPOption( "pedantic", TPOPTIONS.TP_OPTION_pedantic, 0),
                new TPOption( "pthread", TPOPTIONS.TP_OPTION_pthread, 0),
                new TPOption( "run", TPOPTIONS.TP_OPTION_run, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "rdynamic", TPOPTIONS.TP_OPTION_rdynamic, 0 ),
                new TPOption( "r", TPOPTIONS.TP_OPTION_r, 0 ),
                new TPOption( "s", TPOPTIONS.TP_OPTION_s, 0 ),
                new TPOption( "traditional", TPOPTIONS.TP_OPTION_traditional, 0 ),
                new TPOption( "Wl,", TPOPTIONS.TP_OPTION_Wl, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "Wp,", TPOPTIONS.TP_OPTION_Wp, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "W", TPOPTIONS.TP_OPTION_W, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "O", TPOPTIONS.TP_OPTION_O, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "m", TPOPTIONS.TP_OPTION_m, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "f", TPOPTIONS.TP_OPTION_f, TP_OPTION_HAS_ARG | TP_OPTION_NOSEP ),
                new TPOption( "isystem", TPOPTIONS.TP_OPTION_isystem, TP_OPTION_HAS_ARG ),
                new TPOption( "include", TPOPTIONS.TP_OPTION_include, TP_OPTION_HAS_ARG ),
                new TPOption( "nostdinc", TPOPTIONS.TP_OPTION_nostdinc, 0 ),
                new TPOption( "nostdlib", TPOPTIONS.TP_OPTION_nostdlib, 0 ),
                new TPOption( "print-search-dirs", TPOPTIONS.TP_OPTION_print_search_dirs, 0 ),
                new TPOption( "w", TPOPTIONS.TP_OPTION_w, 0 ),
                new TPOption( "pipe", TPOPTIONS.TP_OPTION_pipe, 0),
                new TPOption( "E", TPOPTIONS.TP_OPTION_E, 0),
                new TPOption( "MD", TPOPTIONS.TP_OPTION_MD, 0),
                new TPOption( "MF", TPOPTIONS.TP_OPTION_MF, TP_OPTION_HAS_ARG ),
                new TPOption( "x", TPOPTIONS.TP_OPTION_x, TP_OPTION_HAS_ARG ),
                new TPOption( "ar", TPOPTIONS.TP_OPTION_ar, 0),
                new TPOption( "impdef", TPOPTIONS.TP_OPTION_impdef, 0),
            };

        //options
        public void no_flag() { }
        public void set_flag() { }
        public void strstart() { }
        public void link_option() { }
        public void skip_linker_arg() { }
        public void copy_linker_arg() { }
        public void tp_set_linker() { }
        public void parse_option_D() { }

        public static void args_parser_add_file(TidePool tp, string filename, FILETYPE filetype)
        {
            FileSpec fspec = new FileSpec(filename, tp.alacarte_link, filetype);
            tp.files.Add(fspec);
            tp.nb_files = tp.files.Count;
        }

        //read contents of list file into a string list
        public static List<string> args_parser_make_argv(byte[] buf)
        {
            char c;
            int bufpos = 0;
            string argstr;
            bool inquote;
            List<string> args = new List<string>();

            for (; ; )
            {
                c = (char)buf[bufpos];
                while ((c != '\0' && c <= ' '))         //skip leading spaces
                {
                    ++bufpos;
                    c = (char)buf[bufpos];
                }
                if (c == 0)             //if at end of buf
                    break;

                inquote = false;
                argstr = "";
                c = (char)buf[bufpos];
                while (c != 0)
                {
                    ++bufpos;
                    if (c == '\\' && (buf[bufpos] == '"' || buf[bufpos] == '\\'))
                    {
                        c = (char)buf[bufpos++];        //handle escaped chars
                    }
                    else if (c == '"')
                    {
                        inquote = !inquote;             //at start or end of quoted str
                        continue;
                    }
                    else if (!inquote && c <= ' ')      //args are delim by spaces, unless inside a quote
                    {
                        break;
                    }
                    argstr += c;
                    c = (char)buf[bufpos];
                }

                Console.Out.WriteLine("<{0}>", argstr);
                args.Add(argstr);                           //add to arg list
            }

            return args;
        }

        /* read list file */
        public static void args_parser_listfile(TidePool tp, string filename, int optind, ref int pargc, ref string[] pargv)
        {
            FileStream fd = null;

            //read list file into buf
            try
            {
                fd = File.Open(filename, FileMode.Open);
            }
            catch (Exception e)
            {
                tp.tp_error("listfile '{0}' not found", filename);
            }

            int len = (int)fd.Length;
            byte[] buf = new byte[len + 1];         //extra byte for null terminator
            fd.Read(buf, 0, len);
            buf[len] = 0;
            fd.Close();

            //copy list file contents into arg list
            List<string> args = new List<string>();         //new args array
            for (int i = 0; i < pargc; ++i)
            {
                if (i == optind)
                {
                    args.AddRange(args_parser_make_argv(buf));        //replace @listfile arg with contents
                }
                else
                {
                    args.Add(pargv[i]);         //copy rest of args to new array
                }
            }

            pargc = args.Count;
            pargv = args.ToArray();
        }

        public static RETCODE tp_parse_args(TidePool tp, string[] args)
        {
            int optind = 0;         //in libtcc.c, this allows you to skip argv[0], but C# doesn't store the exe path there
            int optidx;
            TPOption popt = null;
            string optarg = null;
            string r;
            string run = null;
            int last_o = -1;
            int x;
            int tool = 0;
            int arg_start = 0;
            int noaction = optind;
            //    char **argv = *pargv;
            int argc = args.Length;

            //    CString linker_arg; /* collect -Wl options */
            //    cstr_new(&linker_arg);

            while (optind < argc)
            {
                r = args[optind];
                if (r[0] == '@' && r.Length > 1)
                {
                    string fname = r.Substring(1);
                    args_parser_listfile(tp, fname, optind, ref argc, ref args);
                    continue;
                }
                optind++;
                if (tool != 0)
                {
                    //            if (r[0] == '-' && r[1] == 'v' && r[2] == 0)
                    //                ++s->verbose;
                    continue;
                }

            reparse:
                if (r[0] != '-' || r.Length == 1)       //get source filename OR '-' for stdin
                {
                    if (r[0] != '@')								/* allow "tcc file(s) -run @ args ..." */
                        args_parser_add_file(tp, r, tp.filetype);
                    if (run != null)
                    {
                        //                tcc_set_options(s, run);
                        //                arg_start = optind - 1;
                        break;
                    }
                    continue;
                }

                /* find option in table */
                for (optidx = 0; optidx < optionsTable.Length; ++optidx)
                {
                    popt = optionsTable[optidx];
                    string p1 = popt.name;
                    string r1 = r.Substring(1);				//skip '-'

                    if (!r1.StartsWith(p1))                 //if no match
                        continue;

                    r1 = r1.Substring(p1.Length);
                    optarg = r1;                                        //option arg is rest of arg str
                    if ((popt.flags & TP_OPTION_HAS_ARG) != 0)
                    {
                        if ((r1.Length == 0) && ((popt.flags & TP_OPTION_NOSEP) == 0))      //unless option arg is separated
                        {
                            if (optind >= argc)
                            {
                                tp.tp_error("argument to '{0}' is missing", r);
                            }
                            optarg = args[optind++];            //then it's the next arg in the list
                        }
                    }
                    else if (r1.Length > 0)         //if option doesn't have an arg, and we didn't match the whole arg str, keep looking
                    {
                        continue;
                    }
                    break;
                }
                if (optidx == optionsTable.Length)
                {
                    tp.tp_error("invalid option -- '{0}'", r);      //if at end of opt tbl
                }

                switch (popt.index)
                {
                    //        case TCC_OPTION_HELP:
                    //            return OPT_HELP;

                    //        case TCC_OPTION_HELP2:
                    //            return OPT_HELP2;

                    case TPOPTIONS.TP_OPTION_I:
                        tp.tp_add_include_path(optarg);
                        break;

                    //        case TCC_OPTION_D:
                    //            parse_option_D(s, optarg);
                    //            break;

                    //        case TCC_OPTION_U:
                    //            tcc_undefine_symbol(s, optarg);
                    //            break;

                    //        case TCC_OPTION_L:
                    //            tcc_add_library_path(s, optarg);
                    //            break;

                    //        case TCC_OPTION_B:
                    //            /* set tcc utilities path (mainly for tcc development) */
                    //            tcc_set_lib_path(s, optarg);
                    //            break;

                    //        case TCC_OPTION_l:
                    //            args_parser_add_file(s, optarg, AFF_TYPE_LIB);
                    //            s->nb_libraries++;
                    //            break;

                    //        case TCC_OPTION_pthread:
                    //            parse_option_D(s, "_REENTRANT");
                    //            s->option_pthread = 1;
                    //            break;

                    //        case TCC_OPTION_bench:
                    //            s->do_bench = 1;
                    //            break;

                    //#ifdef CONFIG_TCC_BACKTRACE
                    //        case TCC_OPTION_bt:
                    //            tcc_set_num_callers(atoi(optarg));
                    //            break;
                    //#endif

                    //#ifdef CONFIG_TCC_BCHECK
                    //        case TCC_OPTION_b:
                    //            s->do_bounds_check = 1;
                    //            s->do_debug = 1;
                    //            break;
                    //#endif

                    //        case TCC_OPTION_g:
                    //            s->do_debug = 1;
                    //            break;

                    case TPOPTIONS.TP_OPTION_c:
                        if (tp.output_type != OUTPUTTYPE.TP_OUTPUT_NONE)
                            tp.tp_warning("-{0}: overriding compiler action already specified", popt.name);
                        tp.output_type = OUTPUTTYPE.TP_OUTPUT_OBJ;
                        break;

                    //called from other options
                    //            x = TCC_OUTPUT_OBJ;
                    //set_output_type:
                    //            if (s->output_type)
                    //                tcc_warning("-%s: overriding compiler action already specified", popt->name);
                    //            s->output_type = x;
                    //            break;

                    //        case TCC_OPTION_d:
                    //            if (*optarg == 'D')
                    //                s->dflag = 3;
                    //            else if (*optarg == 'M')
                    //                s->dflag = 7;
                    //            else if (*optarg == 't')
                    //                s->dflag = 16;
                    //            else if (isnum(*optarg))
                    //                g_debug = atoi(optarg);
                    //            else
                    //                goto unsupported_option;
                    //            break;

                    //        case TCC_OPTION_static:
                    //            s->static_link = 1;
                    //            break;

                    case TPOPTIONS.TP_OPTION_std:
                        /* silently ignore, a current purpose: allow to use a tcc as a reference compiler for "make test" */
                        break;

                    //        case TCC_OPTION_shared:
                    //            x = TCC_OUTPUT_DLL;
                    //            goto set_output_type;

                    //        case TCC_OPTION_soname:
                    //            s->soname = tcc_strdup(optarg);
                    //            break;

                    case TPOPTIONS.TP_OPTION_o:
                        if (tp.outFilename != null)
                        {
                            tp.tp_warning("multiple -o option");
                            tp.outFilename = null;
                        }
                        tp.outFilename = String.Copy(optarg);
                        break;

                    //        case TCC_OPTION_r:
                    //            /* generate a .o merging several output files */
                    //            s->option_r = 1;
                    //            x = TCC_OUTPUT_OBJ;
                    //            goto set_output_type;

                    case TPOPTIONS.TP_OPTION_isystem:
                        tp.tp_add_sysinclude_path(optarg);
                        break;

                    //        case TCC_OPTION_include:
                    //            dynarray_add(&s->cmd_include_files,
                    //                &s->nb_cmd_include_files, tcc_strdup(optarg));
                    //            break;

                    //        case TCC_OPTION_nostdinc:
                    //            s->nostdinc = 1;
                    //            break;

                    //        case TCC_OPTION_nostdlib:
                    //            s->nostdlib = 1;
                    //            break;

                    //        case TCC_OPTION_run:
                    //#ifndef TCC_IS_NATIVE
                    //            tcc_error("-run is not available in a cross compiler");
                    //#endif
                    //            run = optarg;
                    //            x = TCC_OUTPUT_MEMORY;
                    //            goto set_output_type;

                    case TPOPTIONS.TP_OPTION_v:
                        int k = -1;
                        do
                        {
                            tp.verbose++;
                            k++;
                        }
                        while ((k < optarg.Length) && (optarg[k] == 'v'));
                        ++noaction;
                        break;

                    //        case TCC_OPTION_f:
                    //            if (set_flag(s, options_f, optarg) < 0)
                    //                goto unsupported_option;
                    //            break;

                    //#ifdef TCC_TARGET_ARM
                    //        case TCC_OPTION_mfloat_abi:
                    //            /* tcc doesn't support soft float yet */
                    //            if (!strcmp(optarg, "softfp")) {
                    //                s->float_abi = ARM_SOFTFP_FLOAT;
                    //                tcc_undefine_symbol(s, "__ARM_PCS_VFP");
                    //            } else if (!strcmp(optarg, "hard"))
                    //                s->float_abi = ARM_HARD_FLOAT;
                    //            else
                    //                tcc_error("unsupported float abi '%s'", optarg);
                    //            break;
                    //#endif
                    //        case TCC_OPTION_m:
                    //            if (set_flag(s, options_m, optarg) < 0) {
                    //                if (x = atoi(optarg), x != 32 && x != 64)
                    //                    goto unsupported_option;
                    //                if (PTR_SIZE != x/8)
                    //                    return x;
                    //                ++noaction;
                    //            }
                    //            break;

                    //        case TCC_OPTION_W:
                    //            if (set_flag(s, options_W, optarg) < 0)
                    //                goto unsupported_option;
                    //            break;

                    //        case TCC_OPTION_w:
                    //            s->warn_none = 1;
                    //            break;

                    //        case TCC_OPTION_rdynamic:
                    //            s->rdynamic = 1;
                    //            break;

                    //        case TCC_OPTION_Wl:
                    //            if (linker_arg.size)
                    //                --linker_arg.size, cstr_ccat(&linker_arg, ',');
                    //            cstr_cat(&linker_arg, optarg, 0);
                    //            if (tcc_set_linker(s, linker_arg.data))
                    //                cstr_free(&linker_arg);
                    //            break;

                    //        case TCC_OPTION_Wp:
                    //            r = optarg;
                    //            goto reparse;

                    case TPOPTIONS.TP_OPTION_E:
                        if (tp.output_type != OUTPUTTYPE.TP_OUTPUT_NONE)
                            tp.tp_warning("-{0}: overriding compiler action already specified", popt.name);
                        tp.output_type = OUTPUTTYPE.TP_OUTPUT_PREPROCESS;
                        break;

                    //        case TCC_OPTION_P:
                    //            s->Pflag = atoi(optarg) + 1;
                    //            break;

                    //        case TCC_OPTION_MD:
                    //            s->gen_deps = 1;
                    //            break;

                    //        case TCC_OPTION_MF:
                    //            s->deps_outfile = tcc_strdup(optarg);
                    //            break;

                    //        case TCC_OPTION_dumpversion:
                    //            printf ("%s\n", TCC_VERSION);
                    //            exit(0);
                    //            break;

                    //        case TCC_OPTION_x:
                    //            if (*optarg == 'c')
                    //                s->filetype = AFF_TYPE_C;
                    //            else if (*optarg == 'a')
                    //                s->filetype = AFF_TYPE_ASMPP;
                    //            else if (*optarg == 'n')
                    //                s->filetype = AFF_TYPE_NONE;
                    //            else
                    //                tcc_warning("unsupported language '%s'", optarg);
                    //            break;

                    //        case TCC_OPTION_O:
                    //            last_o = atoi(optarg);
                    //            break;

                    //        case TCC_OPTION_print_search_dirs:
                    //            x = OPT_PRINT_DIRS;
                    //            goto extra_action;

                    //        case TCC_OPTION_impdef:
                    //            x = OPT_IMPDEF;
                    //            goto extra_action;

                    //        case TCC_OPTION_ar:
                    //            x = OPT_AR;
                    //extra_action:
                    //            arg_start = optind - 1;
                    //            if (arg_start != noaction)
                    //                tcc_error("cannot parse %s here", r);
                    //            tool = x;
                    //            break;
                    //        case TCC_OPTION_traditional:
                    //        case TCC_OPTION_pedantic:
                    //        case TCC_OPTION_pipe:
                    //        case TCC_OPTION_s:
                    //            /* ignored */
                    //            break;

                    default:
                        //unsupported_option:
                        if (tp.warn_unsupported != 0)
                            tp.tp_warning("unsupported option '{0}'", r);
                        break;
                }
            }

            //    if (last_o > 0)
            //        tcc_define_symbol(s, "__OPTIMIZE__", NULL);

            //    if (linker_arg.size) {
            //        r = linker_arg.data;
            //        tp.tp_error("argument to '{0}' is missing",r);
            //    }

            //    *pargc = argc - arg_start;
            //    *pargv = argv + arg_start;

            //    if (tool)
            //        return tool;

            //    if (optind != noaction)
            //        return 0;

            //    if (s->verbose == 2)
            //        return OPT_PRINT_DIRS;

            //    if (s->verbose)
            //        return OPT_V;

            return RETCODE.OPT_HELP;
        }

        public void tp_set_options() { }
        public void tp_print_stats() { }
    }

    //-------------------------------------------------------------------------

    public class TPOption
    {
        public string name;		//opt str to match on
        public TPOPTIONS index;			//enum int val
        public int flags;

        public TPOption(string _name, TPOPTIONS _index, int _flags)
        {
            name = _name;
            index = _index;
            flags = _flags;
        }
    }

    //-------------------------------------------------------------------------

    public enum TPOPTIONS
    {
        TP_OPTION_HELP,
        TP_OPTION_HELP2,
        TP_OPTION_v,
        TP_OPTION_I,
        TP_OPTION_D,
        TP_OPTION_U,
        TP_OPTION_P,
        TP_OPTION_L,
        TP_OPTION_B,
        TP_OPTION_l,
        TP_OPTION_bench,
        TP_OPTION_bt,
        TP_OPTION_b,
        TP_OPTION_g,
        TP_OPTION_c,
        TP_OPTION_dumpversion,
        TP_OPTION_d,
        TP_OPTION_static,
        TP_OPTION_std,
        TP_OPTION_shared,
        TP_OPTION_soname,
        TP_OPTION_o,
        TP_OPTION_r,
        TP_OPTION_s,
        TP_OPTION_traditional,
        TP_OPTION_Wl,
        TP_OPTION_Wp,
        TP_OPTION_W,
        TP_OPTION_O,
        TP_OPTION_mfloat_abi,
        TP_OPTION_m,
        TP_OPTION_f,
        TP_OPTION_isystem,
        TP_OPTION_iwithprefix,
        TP_OPTION_include,
        TP_OPTION_nostdinc,
        TP_OPTION_nostdlib,
        TP_OPTION_print_search_dirs,
        TP_OPTION_rdynamic,
        TP_OPTION_param,
        TP_OPTION_pedantic,
        TP_OPTION_pthread,
        TP_OPTION_run,
        TP_OPTION_w,
        TP_OPTION_pipe,
        TP_OPTION_E,
        TP_OPTION_MD,
        TP_OPTION_MF,
        TP_OPTION_x,
        TP_OPTION_ar,
        TP_OPTION_impdef
    }

    //-------------------------------------------------------------------------

    public enum OUTPUTTYPE
    {
        TP_OUTPUT_NONE,
        TP_OUTPUT_MEMORY,
        TP_OUTPUT_EXE,
        TP_OUTPUT_DLL,
        TP_OUTPUT_OBJ,
        TP_OUTPUT_PREPROCESS
    }
}
