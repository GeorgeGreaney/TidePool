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
    public class Generator
    {
        public TidePool tp;
        public Compiler comp;

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

        //        /* pretty names for the registers */
        //enum {
        //    TREG_EAX = 0,
        //    TREG_ECX,
        //    TREG_EDX,
        //    TREG_EBX,
        //    TREG_ST0,
        //    TREG_ESP = 4
        //};

        /* return registers for function */
        //#define REG_IRET TREG_EAX /* single word int return register */
        //#define REG_LRET TREG_EDX /* second word return register (for long long) */
        //#define REG_FRET TREG_ST0 /* float return register */

        ///* defined if function parameters must be evaluated in reverse order */
        //#define INVERT_FUNC_PARAMS

        /* defined if structures are passed as pointers. Otherwise structures are directly pushed on stack. */
        ///* #define FUNC_STRUCT_PARAM_AS_PTR */

        /* pointer size, in bytes */
        public const int PTR_SIZE = 4;

        /* long double size and alignment, in bytes */
        //#define LDOUBLE_SIZE  12
        //#define LDOUBLE_ALIGN 4

        /* maximum alignment (for aligned attribute support) */
        //#define MAX_ALIGN     8


        /* define to 1/0 to [not] have EBX as 4th register */
        public const int USE_EBX = 1;

        public int[] reg_classes;

        public int func_sub_sp_offset;
        public int func_ret_sub;

        //#ifdef CONFIG_TCC_BCHECK
        //static addr_t func_bound_offset;
        //static unsigned long func_bound_ind;
        //#endif


        public Generator(TidePool _tp)
        {
            tp = _tp;

            reg_classes = new int[NB_REGS] 
                {
                    /* eax */ RC_INT | RC_EAX,
                    /* ecx */ RC_INT | RC_ECX,
                    /* edx */ RC_INT | RC_EDX,
                    /* ebx */ (RC_INT | RC_EBX) * USE_EBX,
                    /* st0 */ RC_FLOAT | RC_ST0,
                };
        }

        public void g(byte c)
        {
            int ind1;
            if (comp.nocode_wanted != 0)
                return;
            ind1 = comp.ind + 1;
            if (ind1 > Section.curTextSection.data_allocated)
                Section.curTextSection.section_realloc(ind1);
            Section.curTextSection.data[comp.ind] = c;
            comp.ind = ind1;
        }

        public void o(uint c)
        {
            while (c != 0)
            {
                g((byte)c);
                c = c >> 8;
            }
        }

        public void gen_le16() { }

        public void gen_le32(int c)
        {
            g((byte)c);
            g((byte)(c >> 8));
            g((byte)(c >> 16));
            g((byte)(c >> 24));
        }

        public void gsym_addr() { }
        public void gsym() { }
        public void oad() { }

        public void gen_addr32(int r, Sym sym, int c)
        {
            if ((r & Compiler.VT_SYM) != 0)
                comp.greloc(Section.curTextSection, sym, comp.ind, I386RELOCS.R_386_32);
            gen_le32(c);

        }

        public void gen_addrpc32() { }
        public void gen_modrm() { }

        public void load(int r, SValue sv)
        {
            int v;
            int t;
            int ft;
            int fc;
            int fr;
            SValue v1;
            SValue v2;
            //    sv = pe_getimport(sv, &v2);

                fr = sv.r;
                ft = sv.type.t & ~Compiler.VT_DEFSIGN;
                fc = (int)sv.c.i;

                ft &= ~(Compiler.VT_VOLATILE | Compiler.VT_CONSTANT);

                v = fr & Compiler.VT_VALMASK;
                if ((fr & Compiler.VT_LVAL) != 0)
                {
            //        if (v == VT_LLOCAL) {
            //            v1.type.t = VT_INT;
            //            v1.r = VT_LOCAL | VT_LVAL;
            //            v1.c.i = fc;
            //            fr = r;
            //            if (!(reg_classes[fr] & RC_INT))
            //                fr = get_reg(RC_INT);
            //            load(fr, &v1);
            //        }
            //        if ((ft & VT_BTYPE) == VT_FLOAT) {
            //            o(0xd9); /* flds */
            //            r = 0;
            //        } else if ((ft & VT_BTYPE) == VT_DOUBLE) {
            //            o(0xdd); /* fldl */
            //            r = 0;
            //        } else if ((ft & VT_BTYPE) == VT_LDOUBLE) {
            //            o(0xdb); /* fldt */
            //            r = 5;
            //        } else if ((ft & VT_TYPE) == VT_BYTE || (ft & VT_TYPE) == VT_BOOL) {
            //            o(0xbe0f);   /* movsbl */
            //        } else if ((ft & VT_TYPE) == (VT_BYTE | VT_UNSIGNED)) {
            //            o(0xb60f);   /* movzbl */
            //        } else if ((ft & VT_TYPE) == VT_SHORT) {
            //            o(0xbf0f);   /* movswl */
            //        } else if ((ft & VT_TYPE) == (VT_SHORT | VT_UNSIGNED)) {
            //            o(0xb70f);   /* movzwl */
            //        } else {
            //            o(0x8b);     /* movl */
            //        }
            //        gen_modrm(r, fr, sv->sym, fc);
                } else {
                    if (v == Compiler.VT_CONST)
                    {
                        o((uint)(0xb8 + r));                    /* mov $xx, r */
                        gen_addr32(fr, sv.sym, fc);
                    }
                    else if (v == Compiler.VT_LOCAL)
                    {
            //            if (fc) {
            //                o(0x8d); /* lea xxx(%ebp), r */
            //                gen_modrm(r, VT_LOCAL, sv->sym, fc);
            //            } else {
            //                o(0x89);
            //                o(0xe8 + r); /* mov %ebp, r */
            //            }
            //        } else if (v == VT_CMP) {
            //            oad(0xb8 + r, 0); /* mov $0, r */
            //            o(0x0f); /* setxx %br */
            //            o(fc);
            //            o(0xc0 + r);
            //        } else if (v == VT_JMP || v == VT_JMPI) {
            //            t = v & 1;
            //            oad(0xb8 + r, t); /* mov $1, r */
            //            o(0x05eb); /* jmp after */
            //            gsym(fc);
            //            oad(0xb8 + r, t ^ 1); /* mov $0, r */
            //        } else if (v != r) {
            //            o(0x89);
            //            o(0xc0 + r + v * 8); /* mov v, r */
                    }
                }

        }

        public void store() { }
        public void gadd_sp() { }
        public void gen_static_call() { }
        public void gcall_or_jmp() { }
        public void gfunc_sret() { }
        public void gfunc_call() { }

        public const int FUNC_PROLOG_SIZE = (10 + USE_EBX);

        /* generate function prolog of type 't' */
        public void gfunc_prolog(CType func_type)
        {
            int addr;
            int align;
            int size;
            FuncAttr.FUNCCALL func_call;
            int fastcall_nb_regs;
            int param_index;
            int param_addr;
            int[] fastcall_regs_ptr;
            Sym sym;
            CType type;

            sym = func_type.reff;
            func_call = sym.f.func_call;
            addr = 8;
            comp.loc = 0;
            comp.func_vc = 0;

                //if (func_call >= FUNC_FASTCALL1 && func_call <= FUNC_FASTCALL3) {
            //        fastcall_nb_regs = func_call - FUNC_FASTCALL1 + 1;
            //        fastcall_regs_ptr = fastcall_regs;
                //} else if (func_call == FUNC_FASTCALLW) {
            //        fastcall_nb_regs = 2;
            //        fastcall_regs_ptr = fastcallw_regs;
                //} else {
                    fastcall_nb_regs = 0;
                    fastcall_regs_ptr = null;
            //    }
                param_index = 0;

                comp.ind += FUNC_PROLOG_SIZE;
                func_sub_sp_offset = comp.ind;

            /* if the function returns a structure, then add an implicit pointer parameter */
                comp.func_vt = sym.type;
            //    func_var = (sym->f.func_type == FUNC_ELLIPSIS);
            //    size = type_size(&func_vt,&align);
            //    if (((func_vt.t & VT_BTYPE) == VT_STRUCT)
            //        && (size > 8 || (size & (size - 1)))) {
            //        /* XXX: fastcall case ? */
            //        func_vc = addr;
            //        addr += 4;
            //        param_index++;
            //    }

            /* define parameters */
            //    while ((sym = sym->next) != NULL) {
            //        type = &sym->type;
            //        size = type_size(type, &align);
            //        size = (size + 3) & ~3;
            //#ifdef FUNC_STRUCT_PARAM_AS_PTR
            //        /* structs are passed as pointer */
            //        if ((type->t & VT_BTYPE) == VT_STRUCT) {
            //            size = 4;
            //        }
            //#endif
            //        if (param_index < fastcall_nb_regs) {
            //            /* save FASTCALL register */
            //            loc -= 4;
            //            o(0x89);     /* movl */
            //            gen_modrm(fastcall_regs_ptr[param_index], VT_LOCAL, NULL, loc);
            //            param_addr = loc;
            //        } else {
            //            param_addr = addr;
            //            addr += size;
            //        }
            //        sym_push(sym->v & ~SYM_FIELD, type,
            //            VT_LOCAL | lvalue_type(type->t), param_addr);
            //        param_index++;
            //    }
            //    func_ret_sub = 0;

            /* pascal type call or fastcall ? */
            //    if (func_call == FUNC_STDCALL || func_call == FUNC_FASTCALLW)
            //        func_ret_sub = addr - 8;
            //#ifndef TCC_TARGET_PE
            //    else if (func_vc)
            //        func_ret_sub = 4;
            //#endif

            //#ifdef CONFIG_TCC_BCHECK
            //    /* leave some room for bound checking code */
            //    if (tcc_state->do_bounds_check) {
            //        func_bound_offset = lbounds_section->data_offset;
            //        func_bound_ind = ind;
            //        oad(0xb8, 0); /* lbound section pointer */
            //        oad(0xb8, 0); /* call to function */
            //    }
            //#endif
        }

        public void gfunc_epilog()
        {
            //addr_t v, saved_ind;

            //#ifdef CONFIG_TCC_BCHECK
            //    if (tcc_state->do_bounds_check
            //        && func_bound_offset != lbounds_section->data_offset) {
            //            addr_t saved_ind;
            //            addr_t *bounds_ptr;
            //            Sym *sym_data;

            //            /* add end of table info */
            //            bounds_ptr = section_ptr_add(lbounds_section, sizeof(addr_t));
            //            *bounds_ptr = 0;

            //            /* generate bound local allocation */
            //            saved_ind = ind;
            //            ind = func_bound_ind;
            //            sym_data = get_sym_ref(&char_pointer_type, lbounds_section, 
            //                func_bound_offset, lbounds_section->data_offset);
            //            greloc(cur_text_section, sym_data,
            //                ind + 1, R_386_32);
            //            oad(0xb8, 0); /* mov %eax, xxx */
            //            gen_static_call(TOK___bound_local_new);
            //            ind = saved_ind;

            //            /* generate bound check local freeing */
            //            o(0x5250); /* save returned value, if any */
            //            greloc(cur_text_section, sym_data, ind + 1, R_386_32);
            //            oad(0xb8, 0); /* mov %eax, xxx */
            //            gen_static_call(TOK___bound_local_delete);
            //            o(0x585a); /* restore returned value, if any */
            //    }
            //#endif

            //    /* align local size to word & save local variables */
            //    v = (-loc + 3) & -4;

            //#if USE_EBX
            //    o(0x8b);
            //    gen_modrm(TREG_EBX, VT_LOCAL, NULL, -(v+4));
            //#endif

            //    o(0xc9); /* leave */
            //    if (func_ret_sub == 0) {
            //        o(0xc3); /* ret */
            //    } else {
            //        o(0xc2); /* ret n */
            //        g(func_ret_sub);
            //        g(func_ret_sub >> 8);
            //    }
            //    saved_ind = ind;
            //    ind = func_sub_sp_offset - FUNC_PROLOG_SIZE;
            //#ifdef TCC_TARGET_PE
            //    if (v >= 4096) {
            //        oad(0xb8, v); /* mov stacksize, %eax */
            //        gen_static_call(TOK___chkstk); /* call __chkstk, (does the stackframe too) */
            //    } else
            //#endif
            //    {
            //        o(0xe58955);  /* push %ebp, mov %esp, %ebp */
            //        o(0xec81);  /* sub esp, stacksize */
            //        gen_le32(v);
            //#ifdef TCC_TARGET_PE
            //        o(0x90);  /* adjust to FUNC_PROLOG_SIZE */
            //#endif
            //    }
            //    o(0x53 * USE_EBX); /* push ebx */
            //    ind = saved_ind;

        }

        public void gjmp() { }
        public void gjmp_addr() { }
        public void gtst_addr() { }
        public void gtst() { }
        public void gen_opi() { }
        public void gen_opf() { }
        public void gen_cvt_itof() { }
        public void gen_cvt_ftoi() { }
        public void gen_cvt_ftof() { }
        public void ggoto() { }
        public void gen_bounded_ptr_add() { }
        public void gen_bounded_ptr_deref() { }
        public void gen_vla_sp_save() { }
        public void gen_vla_sp_restore() { }
        public void gen_vla_alloc() { }
    }
}
