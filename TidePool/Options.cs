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
        List<FileSpec> files;

        public Options(string[] args)
        {
            files = new List<FileSpec>();

            for (int i = 0; i < args.Length; i++)
            {
                string arg = args[i];
                if (arg[0] != '-')
                {
                    addFile(arg, FILETYPE.NONE);
                }
            }
        }

        public void addFile(string filename, FILETYPE ftype)
        {
            FileSpec fspec = new FileSpec(filename, ftype);
            files.Add(fspec);
        }
    }

    public enum FILETYPE {NONE, C, ASM, ASMPP, LIB}

    public class FileSpec
    {
        string name;
        FILETYPE ftype;

        public FileSpec(string _name, FILETYPE _ftype)
        {
            name = _name;
            ftype = _ftype;
        }
    }
}
