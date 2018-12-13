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
    public class Win32PE
    {
        public void pe_export_name() { }
        public void pe_find_import() { }
        public void dynarray_assoc() { }
        public void umax() { }
        public void pe_file_align() { }
        public void pe_virtual_align() { }
        public void pe_align_section() { }
        public void pe_set_datadir() { }
        public void pe_fwrite() { }
        public void pe_fpad() { }
        public void pe_write() { }
        public void pe_add_import() { }
        public void pe_free_imports() { }
        public void pe_build_imports() { }
        public void sym_cmp() { }
        public void pe_build_exports() { }
        public void pe_build_reloc() { }
        public void pe_section_class() { }
        public void pe_assign_addresses() { }
        public void pe_isafunc() { }
        public void pe_check_symbols() { }
        public void pe_print_section() { }
        public void pe_getimport() { }
        public void pe_putimport() { }
        public void add_dllref() { }
        public void read_mem() { }
        public void tcc_get_dllexports() { }
        public void pe_load_res() { }
        public void trimfront() { }
        public void trimback() { }
        public void pe_load_def() { }
        public void pe_load_dll() { }
        public void pe_load_file() { }
        public void pe_add_runtime() { }
        public void pe_set_options() { }

        public static int pe_output_file(TidePool tp, string filename)
        {
            return 0;
        }
    }
}
