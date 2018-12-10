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
    class Generator
    {
        /* number of available registers */
        public const int NB_REGS = 5;
        public const int NB_ASM_REGS = 8;
        //#define CONFIG_TCC_ASM

        /* a register can belong to several classes. The classes must be
        sorted from more general to more precise (see gv2() code which does
        assumptions on it). */
        public const int RC_INT = 0x0001; /* generic integer register */
        public const int RC_FLOAT = 0x0002; /* generic float register */
        public const int RC_EAX = 0x0004;
        public const int RC_ST0 = 0x0008;
        public const int RC_ECX = 0x0010;
        public const int RC_EDX = 0x0020;
        public const int RC_EBX = 0x0040;

        public const int RC_IRET = RC_EAX; /* function return: integer register */
        public const int RC_LRET = RC_EDX; /* function return: second integer register */
        public const int RC_FRET = RC_ST0; /* function return: float register */

    }
}
