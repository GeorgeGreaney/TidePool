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

namespace TidePool
{
    public class Options
    {
        public static TidePool tp;

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

        public static void parseOptions(TidePool _tp, string[] args)
        {
            tp = _tp;

            for (int i = 0; i < args.Length; i++)
            {
                string arg = args[i];
                if (arg[0] != '-' || arg.Length == 1)
                {
                    args_parser_(arg, tp.filetype);
                    continue;
                }

                arg = arg.Substring(1);
                TPOption opt = null;
                String optarg = null;
                for (int j = 0; j < optionsTable.Length; j++)
                {
                    if (arg.StartsWith(optionsTable[j].name))
                    {
                        opt = optionsTable[j];
                        if ((opt.flags & TP_OPTION_HAS_ARG) != 0)
                        {
                            if ((opt.flags & TP_OPTION_NOSEP) != 0)
                            {
                                optarg = arg.Substring(opt.name.Length);
                            }
                            else
                            {
                                if (i < args.Length - 1)
                                {
                                    optarg = args[++i];
                                }
                            }
                            if (optarg == null)
                            {
                                tp.tp_error("argument to '{0}' is missing", arg);
                            }
                        }
                        break;
                    }
                }
                if (opt == null)
                {
                    tp.tp_error("invalid option -- '{0}'", arg);
                }

                switch (opt.index)
                {
                    case TPOPTIONS.TP_OPTION_v:
                        int k = -1;
                        do
                        {
                            tp.verbose++;
                            k++;
                        }
                        while ((k < optarg.Length) && (optarg[k] == 'v'));
                        break;

                    case TPOPTIONS.TP_OPTION_E:
                        tp.output_type = OUTPUTTYPE.TP_OUTPUT_PREPROCESS;
                        break;

                    case TPOPTIONS.TP_OPTION_o:
                        if (tp.outFilename != null)
                        {
                            tp.tp_warning("multiple -o option");
                            tp.outFilename = null;
                        }
                        tp.outFilename = String.Copy(optarg);
                        break;

                }
            }
        }

        public static void args_parser_(string filename, FILETYPE ftype)
        {
            FileSpec fspec = new FileSpec(filename, ftype);
            tp.files.Add(fspec);
        }
    }

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


    public enum OUTPUTTYPE
    {
        TP_OUTPUT_MEMORY,
        TP_OUTPUT_EXE,
        TP_OUTPUT_DLL,
        TP_OUTPUT_OBJ,
        TP_OUTPUT_PREPROCESS
    }
}
