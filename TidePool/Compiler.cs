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
    public class Compiler
    {
        public TidePool tp;
        public Preprocessor prep;
        public Generator gen;

        public const int LONG_SIZE = 4;

        public const int SYM_STRUCT = 0x40000000;         /* struct/union/enum symbol space */
        public const int SYM_FIELD = 0x20000000;          /* struct/union field symbol space */
        public const int SYM_FIRST_ANOM = 0x10000000;     /* first anonymous sym */

        /* stored in 'Sym->f.func_type' field */
        public const int FUNC_NEW = 1;                    /* ansi function prototype */
        public const int FUNC_OLD = 2;                    /* old function prototype */
        public const int FUNC_ELLIPSIS = 3;               /* ansi function prototype with ... */

        /* type_decl() types */
        public const int TYPE_ABSTRACT = 1;         /* type without variable */
        public const int TYPE_DIRECT = 2;           /* type with variable */

        public const int VT_VALMASK = 0x003f;       /* mask for value location, register or: */
        public const int VT_CONST = 0x0030;         /* constant in vc (must be first non register value) */
        public const int VT_LLOCAL = 0x0031;        /* lvalue, offset on stack */
        public const int VT_LOCAL = 0x0032;         /* offset on stack */
        public const int VT_CMP = 0x0033;           /* the value is stored in processor flags (in vc) */
        public const int VT_JMP = 0x0034;           /* value is the consequence of jmp true (even) */
        public const int VT_JMPI = 0x0035;          /* value is the consequence of jmp false (odd) */
        public const int VT_LVAL = 0x0100;          /* var is an lvalue */
        public const int VT_SYM = 0x0200;           /* a symbol value is added */
        public const int VT_MUSTCAST = 0x0400;      /* value must be casted to be correct (used for char/short stored in integer registers) */
        public const int VT_MUSTBOUND = 0x0800;     /* bound checking must be done before dereferencing value */
        public const int VT_BOUNDED = 0x8000;       /* value is bounded. The address of the bounding function call point is in vc */
        public const int VT_LVAL_BYTE = 0x1000;     /* lvalue is a byte */
        public const int VT_LVAL_SHORT = 0x2000;    /* lvalue is a short */
        public const int VT_LVAL_UNSIGNED = 0x4000; /* lvalue is unsigned */

        public const int VT_LVAL_TYPE = (VT_LVAL_BYTE | VT_LVAL_SHORT | VT_LVAL_UNSIGNED);

        /* types */
        public const int VT_BTYPE = 0x000f;         /* mask for basic type */
        public const int VT_VOID = 0;               /* void type */
        public const int VT_BYTE = 1;               /* signed byte type */
        public const int VT_SHORT = 2;              /* short type */
        public const int VT_INT = 3;                /* integer type */
        public const int VT_LLONG = 4;              /* 64 bit integer */
        public const int VT_PTR = 5;                /* pointer */
        public const int VT_FUNC = 6;               /* function type */
        public const int VT_STRUCT = 7;             /* struct/union definition */
        public const int VT_FLOAT = 8;              /* IEEE float */
        public const int VT_DOUBLE = 9;             /* IEEE double */
        public const int VT_LDOUBLE = 10;           /* IEEE long double */
        public const int VT_BOOL = 11;              /* ISOC99 boolean type */
        public const int VT_QLONG = 13;             /* 128-bit integer. Only used for x86-64 ABI */
        public const int VT_QFLOAT = 14;            /* 128-bit float. Only used for x86-64 ABI */

        public const int VT_UNSIGNED = 0x0010;      /* unsigned type */
        public const int VT_DEFSIGN = 0x0020;       /* explicitly signed or unsigned */
        public const int VT_ARRAY = 0x0040;         /* array type (also has VT_PTR) */
        public const int VT_BITFIELD = 0x0080;      /* bitfield modifier */
        public const int VT_CONSTANT = 0x0100;      /* const modifier */
        public const int VT_VOLATILE = 0x0200;      /* volatile modifier */
        public const int VT_VLA = 0x0400;           /* VLA type (also has VT_PTR and VT_ARRAY) */
        public const int VT_LONG = 0x0800;          /* long type (also has VT_INT rsp. VT_LLONG) */

        /* storage */
        public const int VT_EXTERN = 0x00001000;    /* extern definition */
        public const int VT_STATIC = 0x00002000;    /* static variable */
        public const int VT_TYPEDEF = 0x00004000;  /* typedef definition */
        public const int VT_INLINE = 0x00008000;    /* inline definition */

        public const int VT_STRUCT_SHIFT = 20;     /* shift for bitfield shift values (32 - 2*6) */
        public const int VT_STRUCT_MASK = (((1 << (6 + 6)) - 1) << VT_STRUCT_SHIFT | VT_BITFIELD);
        //#define BIT_POS(t) (((t) >> VT_STRUCT_SHIFT) & 0x3f)
        //#define BIT_SIZE(t) (((t) >> (VT_STRUCT_SHIFT + 6)) & 0x3f)

        public const int VT_UNION = (1 << VT_STRUCT_SHIFT | VT_STRUCT);
        public const int VT_ENUM = (2 << VT_STRUCT_SHIFT);                  /* integral type is an enum really */
        public const int VT_ENUM_VAL = (3 << VT_STRUCT_SHIFT);              /* integral type is an enum constant really */

        public const int VT_STORAGE = (VT_EXTERN | VT_STATIC | VT_TYPEDEF | VT_INLINE);
        public const int VT_TYPE = (~(VT_STORAGE | VT_STRUCT_MASK));

        public int rsym;
        public int anon_sym;
        public int ind;
        public int loc;

        //symbol allocation
        //ST_DATA Sym *sym_free_first;
        //ST_DATA void **sym_pools;
        //ST_DATA int nb_sym_pools;

        public List<Sym> global_stack;
        public List<Sym> local_stack;
        public List<Sym> define_stack;
        public List<Sym> global_label_stack;
        public List<Sym> local_label_stack;
        public int local_scope;
        public int in_sizeof;
        public int section_sym;

        //ST_DATA int vlas_in_scope; /* number of VLAs that are currently in scope */
        //ST_DATA int vla_sp_root_loc; /* vla_sp_loc for SP before any VLAs were pushed */
        //ST_DATA int vla_sp_loc; /* Pointer to variable holding location to store stack pointer on the stack when modifying stack pointer */

        //        #define SYM_POOL_NB (8192 / sizeof(Sym))

        public const int VSTACK_SIZE = 256;
        public SValue[] vstack;
        public int vtop;
        public int pvtop;

        public bool const_wanted;       /* true if constant wanted */
        public uint nocode_wanted;      /* true if no code generation wanted for an expression */
        public bool global_expr;        /* true if compound literals must be allocated globally (used during initializers parsing */

        public CType func_vt;           /* current function return type (used by return instruction) */
        public bool func_var;           /* true if current function is variadic */
        public int func_vc;
        public int last_line_num;
        public int last_ind;
        public int func_ind; /* debug last line number and pc */
        public string funcname;
        public int g_debug;

        //#define NODATA_WANTED (nocode_wanted > 0) /* no static data output wanted either */
        //#define STATIC_DATA_WANTED (nocode_wanted & 0xC0000000) /* only static data output */

        //ST_DATA CType char_pointer_type, func_old_type, int_type, size_type, ptrdiff_type;

        public Compiler(TidePool _tp)
        {
            tp = _tp;

            global_stack = new List<Sym>();
            local_stack = null;
            vstack = new SValue[VSTACK_SIZE];
            vtop = 0;
            func_vt = new CType();
        }

        public bool is_float(int t)
        {
            int bt;
            bt = t & VT_BTYPE;
            return (bt == VT_LDOUBLE || bt == VT_DOUBLE || bt == VT_FLOAT || bt == VT_QFLOAT);
        }

        public void ieee_finite() { }
        public void test_lvalue() { }
        public void check_vstack() { }

        //---------------------------------------------------------------------

        public void tp_debug_start() 
        { 
    if (tp.do_debug != 0) {
//        char buf[512];

//        /* file info: full path + filename */
//        section_sym = put_elf_sym(symtab_section, 0, 0,
//            ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
//            text_section->sh_num, NULL);
//        getcwd(buf, sizeof(buf));
//#ifdef _WIN32
//        normalize_slashes(buf);
//#endif
//        pstrcat(buf, sizeof(buf), "/");
//        put_stabs_r(buf, N_SO, 0, 0,
//            text_section->data_offset, text_section, section_sym);
//        put_stabs_r(file->filename, N_SO, 0, 0,
//            text_section->data_offset, text_section, section_sym);
//        last_ind = 0;
//        last_line_num = 0;
    }

    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL symbols can be safely used */
    int elf32StInfo = ((int)STBIND.STB_LOCAL << 4) + ((int)STTYPE.STT_FILE & 0xf);
    Section.symtab_section.put_elf_sym(0, 0, elf32StInfo, 0, (int)SECTIONIDX.SHN_ABS, prep.curFile.filename);
        }

        public void tp_debug_end() { }
        public void tp_debug_line() { }
        public void tp_debug_funcstart() { }
        public void tp_debug_funcend() { }

        //---------------------------------------------------------------------

        public int tp_compile()
        {
            //link our processes together
            prep = tp.prep;
            gen = tp.gen;
            gen.comp = this;

            Section.curTextSection = null;
            //funcname = "";
            //anon_sym = SYM_FIRST_ANOM;
            //section_sym = 0;
            //const_wanted = 0;
            //nocode_wanted = 0x80000000;

            /* define some often used types */
            //    int_type.t = VT_INT;
            //    char_pointer_type.t = VT_BYTE;
            //    mk_pointer(&char_pointer_type);
            //#if PTR_SIZE == 4
            //    size_type.t = VT_INT | VT_UNSIGNED;
            //    ptrdiff_type.t = VT_INT;
            //#elif LONG_SIZE == 4
            //    size_type.t = VT_LLONG | VT_UNSIGNED;
            //    ptrdiff_type.t = VT_LLONG;
            //#else
            //    size_type.t = VT_LONG | VT_LLONG | VT_UNSIGNED;
            //    ptrdiff_type.t = VT_LONG | VT_LLONG;
            //#endif
            //    func_old_type.t = VT_FUNC;
            //    func_old_type.ref = sym_push(SYM_FIELD, &int_type, 0, 0);
            //    func_old_type.ref->f.func_call = FUNC_CDECL;
            //    func_old_type.ref->f.func_type = FUNC_OLD;

            tp_debug_start();

            //#ifdef INC_DEBUG
            //    printf("%s: **** new file\n", file->filename);
            //#endif

            prep.parseFlags = Preprocessor.PARSE_FLAG_PREPROCESS | Preprocessor.PARSE_FLAG_TOK_NUM | Preprocessor.PARSE_FLAG_TOK_STR;
            prep.next();
            decl(VT_CONST);
            //    gen_inline_functions(s1);
            //    check_vstack();

            /* end of translation unit info */
            //    tcc_debug_end(s1);
            return 0;
        }

        public Elf32_Sym elfsym(Sym s)
        {
            if (s == null || s.c == 0)
            {
                return null;
            }
            return Section.symtab_section.SymbolDict[s.c];
        }

        public void update_storage(Sym sym)
        {
            //ElfSym esym;
            //int sym_bind;
            //int old_sym_bind;

            //esym = elfsym(sym);
            //if (esym == null)
            return;

            //    if (sym->a.visibility)
            //        esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
            //            | sym->a.visibility;

            //    if (sym->type.t & VT_STATIC)
            //        sym_bind = STB_LOCAL;
            //    else if (sym->a.weak)
            //        sym_bind = STB_WEAK;
            //    else
            //        sym_bind = STB_GLOBAL;
            //    old_sym_bind = ELFW(ST_BIND)(esym->st_info);
            //    if (sym_bind != old_sym_bind) {
            //        esym->st_info = ELFW(ST_INFO)(sym_bind, ELFW(ST_TYPE)(esym->st_info));
            //    }

            //#ifdef TCC_TARGET_PE
            //    if (sym->a.dllimport)
            //        esym->st_other |= ST_PE_IMPORT;
            //    if (sym->a.dllexport)
            //        esym->st_other |= ST_PE_EXPORT;
            //#endif

            //#if 0
            //    printf("storage %s: bind=%c vis=%d exp=%d imp=%d\n",
            //        get_tok_str(sym->v, NULL),
            //        sym_bind == STB_WEAK ? 'w' : sym_bind == STB_LOCAL ? 'l' : 'g',
            //        sym->a.visibility,
            //        sym->a.dllexport,
            //        sym->a.dllimport
            //        );
            //#endif

        }

        /* update sym->c so that it points to an external symbol in section 'section' with value 'value' */
        public void put_extern_sym2(Sym sym, int sh_num, int value, int size, int can_add_underscore)
        {

            STTYPE sym_type;
            STBIND sym_bind;
            int info;
            int other;
            int t;
            Elf32_Sym esym;
            string name;
            char[] buf1 = new char[256];
            char[] buf = new char[32];

            if (sym.c == 0)
            {
                name = prep.get_tok_str(sym.v, null);
                        if (tp.do_bounds_check != 0) {
                            /* XXX: avoid doing that for statics ? */
                            /* if bound checking is activated, we change some function names by adding the "__bound" prefix */
                //            switch(sym->v) {
                                /* XXX: we rely only on malloc hooks */
                //            case TOK_malloc:
                //            case TOK_free:
                //            case TOK_realloc:
                //            case TOK_memalign:
                //            case TOK_calloc:
                //            case TOK_memcpy:
                //            case TOK_memmove:
                //            case TOK_memset:
                //            case TOK_strlen:
                //            case TOK_strcpy:
                //            case TOK_alloca:
                //                strcpy(buf, "__bound_");
                //                strcat(buf, name);
                //                name = buf;
                //                break;
                //            }
                        }
                        t = sym.type.t;
                        if ((t & VT_BTYPE) == VT_FUNC) {
                            sym_type = STTYPE.STT_FUNC;
                        } else if ((t & VT_BTYPE) == VT_VOID) {
                            sym_type = STTYPE.STT_NOTYPE;
                        } else {
                            sym_type = STTYPE.STT_OBJECT;
                        }
                        if ((t & VT_STATIC) != 0)
                        {
                            sym_bind = STBIND.STB_LOCAL;
                        }
                        else
                        {
                            sym_bind = STBIND.STB_GLOBAL;
                        }
                        other = 0;
                //        if (sym_type == STT_FUNC && sym->type.ref) {
                //            Sym *ref = sym->type.ref;
                //            if (ref->f.func_call == FUNC_STDCALL && can_add_underscore) {
                //                sprintf(buf1, "_%s@%d", name, ref->f.func_args * PTR_SIZE);
                //                name = buf1;
                //                other |= ST_PE_STDCALL;
                //                can_add_underscore = 0;
                //            }
                //        }
                //        if (tcc_state->leading_underscore && can_add_underscore) {
                //            buf1[0] = '_';
                //            pstrcpy(buf1 + 1, sizeof(buf1) - 1, name);
                //            name = buf1;
                //        }
                //        if (sym->asm_label)
                //            name = get_tok_str(sym->asm_label, NULL);
                        info = (((int)sym_bind << 4) + ((int)sym_type & 0xf));
                        sym.c = Section.symtab_section.put_elf_sym(value, size, info, other, sh_num, name);
            }
            else
            {
                //        esym = elfsym(sym);
                //        esym->st_value = value;
                //        esym->st_size = size;
                //        esym->st_shndx = sh_num;
            }
            update_storage(sym);
        }

        public void put_extern_sym(Sym sym, Section section, int value, int size)
        {
            int sh_num = (section != null) ? section.sh_num : (int)SECTIONIDX.SHN_UNDEF;
            put_extern_sym2(sym, sh_num, value, size, 1);
        }

        /* add a new relocation entry to symbol 'sym' in section 's' */
        public void greloca(Section s, Sym sym, int offset, I386RELOCS type, uint addend)
        {
            //int c = 0;

            //if (nocode_wanted && s == cur_text_section)
            //    return;

            //if (sym)
            //{
            //    if (0 == sym->c)
            //        put_extern_sym(sym, NULL, 0, 0);
            //    c = sym->c;
            //}

            ///* now we can add ELF relocation info */
            //put_elf_reloca(symtab_section, s, offset, type, c, addend);
        }

        public void greloc(Section s, Sym sym, int offset, I386RELOCS type)
        {
            greloca(s, sym, offset, type, 0);
        }

        //- symbols -----------------------------------------------------------

        public Sym sym_push2(List<Sym> ps, int v, int t, int c)
        {
            Sym s = new Sym();

            s.v = v;
            s.type.t = t;
            s.c = c;

            /* add in stack */
            ps.Add(s);
            return s;
        }

        public void sym_find2() { }
        public void struct_find() { }

        public Sym sym_find(int v)
        {
            if ((v < (int)TPTOKEN.TOK_IDENT) || (v > prep.tok_ident))
                return null;

            //v -= TOK_IDENT;
            //if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
            //    return NULL;
            return prep.table_ident[(v - (int)TPTOKEN.TOK_IDENT)].sym_identifier;
        }

        public Sym sym_push(int v, CType type, int r, int c)
        {
            Sym s;
            List<Sym> ps;
            TokenSym ts;

            if (local_stack != null)
                ps = local_stack;
            else
                ps = global_stack;

            s = sym_push2(ps, v, type.t, c);
            s.type.reff = type.reff;
            s.r = (short)r;

            /* don't record fields or anonymous symbols */
            /* XXX: simplify */
            if (!((v & SYM_FIELD) != 0) && ((v & ~SYM_STRUCT) < SYM_FIRST_ANOM))
            {
                /* record symbol in token array */
                //      ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
                //        if (v & SYM_STRUCT)
                //            ps = &ts->sym_struct;
                //        else
                //            ps = &ts->sym_identifier;
                //        s->prev_tok = *ps;
                //        *ps = s;
                //        s->sym_scope = local_scope;
                //        if (s->prev_tok && s->prev_tok->sym_scope == s->sym_scope)
                //            tcc_error("redeclaration of '%s'",
                //                get_tok_str(v & ~SYM_STRUCT, NULL));
            }
            return s;
        }

        public Sym global_identifier_push(int v, int t, int c)
        {
            Sym s = sym_push2(global_stack, v, t, c);

            /* don't record anonymous symbol */
            if (v < SYM_FIRST_ANOM)
            {
                Sym ps = prep.table_ident[v - (int)TPTOKEN.TOK_IDENT].sym_identifier;

                /* modify the top most local identifier, so that sym_identifier will point to 's' when popped */
                while ((ps != null) && (ps.sym_scope != 0))
                    ps = ps.prev_tok;
                s.prev_tok = ps;
                prep.table_ident[v - (int)TPTOKEN.TOK_IDENT].sym_identifier = s;
            }

            return s;
        }

        public void sym_pop() { }

        //- values ------------------------------------------------------------

        public void vsetc(CType type, int r, CValue vc)
        {
            int v;

            if (vtop >= (VSTACK_SIZE))
                tp.tp_error("memory full (vstack)");

            /* cannot let cpu flags if other instruction are generated. Also
            avoid leaving VT_JMP anywhere except on the top of the stack
            because it would complicate the code generator.

            Don't do this when nocode_wanted.  vtop might come from
            !nocode_wanted regions (see 88_codeopt.c) and transforming
            it to a register without actually generating code is wrong
            as their value might still be used for real.  All values
            we push under nocode_wanted will eventually be popped
            again, so that the VT_CMP/VT_JMP value will be in vtop
            when code is unsuppressed again.

            Same logic below in vswap(); */
            if (vtop >= 1 && (nocode_wanted == 0))
            {
                v = vstack[vtop].r & VT_VALMASK;
                if (v == VT_CMP || (v & ~1) == VT_JMP)
                    gv(Generator.RC_INT);
            }

            SValue sval = new SValue();
            sval.type = type;
            sval.r = r;
            sval.r2 = VT_CONST;
            sval.c = vc;
            sval.sym = null;
            vstack[++vtop] = sval;
        }

        public void vswap() { }
        public void vpop() { }
        public void vpush() { }
        public void vpushi() { }
        public void vpushs() { }
        public void vpush64() { }
        public void vpushll() { }
        public void vset() { }
        public void vseti() { }
        public void vpushv() { }
        public void vdup() { }
        public void vrotb() { }
        public void vrote() { }
        public void vrott() { }
        public void vpushsym() { }
        public void get_sym_ref() { }
        public void vpush_ref() { }

        public Sym external_global_sym(int v, CType type, int r)
        {
            Sym s = sym_find(v);

            if (s == null)
            {
                /* push forward reference */
                s = global_identifier_push(v, (type.t | VT_EXTERN), 0);
                s.type.reff = type.reff;
                s.r = (short)(r | VT_CONST | VT_SYM);
                //} else if (IS_ASM_SYM(s)) {
                //    s->type.t = type->t | (s->type.t & VT_EXTERN);
                //    s->type.ref = type->ref;
                //    update_storage(s);
            }
            return s;
        }

        /* Merge some type attributes.  */
        public void patch_type(Sym sym, CType type)
        {
            if (!((type.t & VT_EXTERN) != 0))
            {
                if (!((sym.type.t & VT_EXTERN) != 0))
                    tp.tp_error("redefinition of '{0}'", prep.get_tok_str(sym.v, null));
                sym.type.t &= ~VT_EXTERN;
            }

            //if (IS_ASM_SYM(sym)) {
            //    /* stay static if both are static */
            //    sym->type.t = type->t & (sym->type.t | ~VT_STATIC);
            //    sym->type.ref = type->ref;
            //}

            if (!is_compatible_types(sym.type, type))
            {
                tp.tp_error("incompatible types for redefinition of '{0}'", prep.get_tok_str(sym.v, null));

            }
            else if ((sym.type.t & VT_BTYPE) == VT_FUNC)
            {
                int static_proto = sym.type.t & VT_STATIC;
                /* warn if static follows non-static function declaration */
                //    if ((type->t & VT_STATIC) && !static_proto && !(type->t & VT_INLINE))
                //        tcc_warning("static storage ignored for redefinition of '%s'",
                //        get_tok_str(sym->v, NULL));

                if (0 == (type.t & VT_EXTERN))
                {
                    /* put complete type, use static from prototype */
                    sym.type.t = (type.t & ~VT_STATIC) | static_proto;
                    //        if (type->t & VT_INLINE)
                    //            sym->type.t = type->t;
                    sym.type.reff = type.reff;
                }

            }
            else
            {
                //    if ((sym->type.t & VT_ARRAY) && type->ref->c >= 0) {
                //        /* set array size if it was omitted in extern declaration */
                //        if (sym->type.ref->c < 0)
                //            sym->type.ref->c = type->ref->c;
                //        else if (sym->type.ref->c != type->ref->c)
                //            tcc_error("conflicting type for '%s'", get_tok_str(sym->v, NULL));
                //    }
                //    if ((type->t ^ sym->type.t) & VT_STATIC)
                //        tcc_warning("storage mismatch for redefinition of '%s'",
                //        get_tok_str(sym->v, NULL));
            }
        }

        /* Merge some storage attributes.  */
        public void patch_storage(Sym sym, AttributeDef ad, CType type)
        {
            if (type != null)
                patch_type(sym, type);

            if (sym.a.dllimport != ad.a.dllimport)
                tp.tp_error("incompatible dll linkage for redefinition of '{0}'", prep.get_tok_str(sym.v, null));
            sym.a.dllexport |= ad.a.dllexport;

            sym.a.weak |= ad.a.weak;
            //if (ad.a.visibility) {
            //    int vis = sym->a.visibility;
            //    int vis2 = ad->a.visibility;
            //    if (vis == STV_DEFAULT)
            //        vis = vis2;
            //    else if (vis2 != STV_DEFAULT)
            //        vis = (vis < vis2) ? vis : vis2;
            //    sym->a.visibility = vis;
            //}
            //if (ad->a.aligned)
            //    sym->a.aligned = ad->a.aligned;
            //if (ad->asm_label)
            //    sym->asm_label = ad->asm_label;
            update_storage(sym);
        }

        public void external_sym() { }
        public void vpush_global_sym() { }
        public void save_regs() { }
        public void save_reg() { }
        public void save_reg_upstack() { }

        /* find a free register of class 'rc'. If none, save one register */
        public int get_reg(int rc)
        {
            int r;
            int ppos;
            SValue p;

            /* find a free register */
            for (r = 0; r < Generator.NB_REGS; r++)
            {
                if ((gen.reg_classes[r] & rc) != 0)
                {
                    if (nocode_wanted != 0)
                        return r;
                    for (ppos = 1; ppos <= vtop; ppos++)
                    {
                        p = vstack[ppos];
                        if (((p.r & VT_VALMASK) == r) || ((p.r2 & VT_VALMASK) == r))
                            goto notfound;
                    }
                    return r;
                }
            notfound: ;
            }

            /* no register left : free the first one on the stack (VERY
            IMPORTANT to start from the bottom to ensure that we don't
            spill registers used in gen_opi()) */
            //for (p = vstack; p <= vtop; p++)
            //{
            //    /* look at second register (if long long) */
            //    r = p->r2 & VT_VALMASK;
            //    if (r < VT_CONST && (reg_classes[r] & rc))
            //        goto save_found;
            //    r = p->r & VT_VALMASK;
            //    if (r < VT_CONST && (reg_classes[r] & rc))
            //    {
            //    save_found:
            //        save_reg(r);
            //        return r;
            //    }
            //}

            /* Should never comes here */
            return -1;

        }

        public void move_reg() { }
        public void gaddrof() { }
        public void gbound() { }
        public void incr_bf_adr() { }
        public void load_packed_bf() { }
        public void store_packed_bf() { }
        public void adjust_bf() { }

        public int gv(int rc)
        {
            int r = 0;
            int bit_pos;
            int bit_size;
            int size;
            int align;
            int rc2;

            /* NOTE: get_reg can modify vstack[] */
            if ((vstack[vtop].type.t & VT_BITFIELD) != 0)
            {
                //        CType type;

                //        bit_pos = BIT_POS(vtop->type.t);
                //        bit_size = BIT_SIZE(vtop->type.t);
                //        /* remove bit field info to avoid loops */
                //        vtop->type.t &= ~VT_STRUCT_MASK;

                //        type.ref = NULL;
                //        type.t = vtop->type.t & VT_UNSIGNED;
                //        if ((vtop->type.t & VT_BTYPE) == VT_BOOL)
                //            type.t |= VT_UNSIGNED;

                //        r = adjust_bf(vtop, bit_pos, bit_size);

                //        if ((vtop->type.t & VT_BTYPE) == VT_LLONG)
                //            type.t |= VT_LLONG;
                //        else
                //            type.t |= VT_INT;

                //        if (r == VT_STRUCT) {
                //            load_packed_bf(&type, bit_pos, bit_size);
                //        } else {
                //            int bits = (type.t & VT_BTYPE) == VT_LLONG ? 64 : 32;
                //            /* cast to int to propagate signedness in following ops */
                //            gen_cast(&type);
                //            /* generate shifts */
                //            vpushi(bits - (bit_pos + bit_size));
                //            gen_op(TOK_SHL);
                //            vpushi(bits - bit_size);
                //            /* NOTE: transformed to SHR if unsigned */
                //            gen_op(TOK_SAR);
                //        }
                //        r = gv(rc);
            }
            else
            {
                //        if (is_float(vtop->type.t) && 
                //            (vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                //                unsigned long offset;
                //                /* CPUs usually cannot use float constants, so we store them
                //                generically in data segment */
                //                size = type_size(&vtop->type, &align);
                //                if (NODATA_WANTED)
                //                    size = 0, align = 1;
                //                offset = section_add(data_section, size, align);
                //                vpush_ref(&vtop->type, data_section, offset, size);
                //                vswap();
                //                init_putv(&vtop->type, data_section, offset);
                //                vtop->r |= VT_LVAL;
                //        }
                //#ifdef CONFIG_TCC_BCHECK
                //        if (vtop->r & VT_MUSTBOUND) 
                //            gbound();
                //#endif

                r = vstack[vtop].r & VT_VALMASK;
                rc2 = ((rc & Generator.RC_FLOAT) != 0) ? Generator.RC_FLOAT : Generator.RC_INT;
                if (rc == Generator.RC_IRET)
                    rc2 = Generator.RC_LRET;
                /* need to reload if:
                - constant
                - lvalue (need to dereference pointer)
                - already a register, but not in the right class */
                if (r >= VT_CONST
                    || ((vstack[vtop].r & VT_LVAL) != 0)
                    || !((gen.reg_classes[r] & rc) != 0)
                    || ((vstack[vtop].type.t & VT_BTYPE) == VT_LLONG && !((gen.reg_classes[vstack[vtop].r2] & rc2) != 0))
                    )
                {
                    r = get_reg(rc);
                    if ((vstack[vtop].type.t & VT_BTYPE) == VT_LLONG)
                    {
                        //                int addr_type = VT_INT, load_size = 4, load_type = VT_INT;
                        //                unsigned long long ll;
                        //                int r2, original_type;
                        //                original_type = vtop->type.t;
                        //                /* two register type load : expand to two words
                        //                temporarily */
                        //                if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                        //                    /* load constant */
                        //                    ll = vtop->c.i;
                        //                    vtop->c.i = ll; /* first word */
                        //                    load(r, vtop);
                        //                    vtop->r = r; /* save register value */
                        //                    vpushi(ll >> 32); /* second word */
                        //                } else
                        //                    if (vtop->r & VT_LVAL) {
                        //                        /* We do not want to modifier the long long
                        //                        pointer here, so the safest (and less
                        //                        efficient) is to save all the other registers
                        //                        in the stack. XXX: totally inefficient. */
                        /* lvalue_save: save only if used further down the stack */
                        //                        save_reg_upstack(vtop->r, 1);
                        /* load from memory */
                        //                        vtop->type.t = load_type;
                        //                        load(r, vtop);
                        //                        vdup();
                        //                        vtop[-1].r = r; /* save register value */
                        /* increment pointer to get second word */
                        //                        vtop->type.t = addr_type;
                        //                        gaddrof();
                        //                        vpushi(load_size);
                        //                        gen_op('+');
                        //                        vtop->r |= VT_LVAL;
                        //                        vtop->type.t = load_type;
                        //                    } else {
                        //                        /* move registers */
                        //                        load(r, vtop);
                        //                        vdup();
                        //                        vtop[-1].r = r; /* save register value */
                        //                        vtop->r = vtop[-1].r2;
                        //                    }
                        /* Allocate second register. Here we rely on the fact that
                        get_reg() tries first to free r2 of an SValue. */
                        //                    r2 = get_reg(rc2);
                        //                    load(r2, vtop);
                        //                    vpop();
                        /* write second register */
                        //                    vtop->r2 = r2;
                        //                    vtop->type.t = original_type;
                    }
                    else if (((vstack[vtop].r & VT_LVAL) != 0) && !is_float(vstack[vtop].type.t))
                    {
                        //                int t1, t;
                        //                /* lvalue of scalar type : need to use lvalue type
                        //                because of possible cast */
                        //                t = vtop->type.t;
                        //                t1 = t;
                        //                /* compute memory access type */
                        //                if (vtop->r & VT_LVAL_BYTE)
                        //                    t = VT_BYTE;
                        //                else if (vtop->r & VT_LVAL_SHORT)
                        //                    t = VT_SHORT;
                        //                if (vtop->r & VT_LVAL_UNSIGNED)
                        //                    t |= VT_UNSIGNED;
                        //                vtop->type.t = t;
                        //                load(r, vtop);
                        //                /* restore wanted type */
                        //                vtop->type.t = t1;
                    }
                    else
                    {
                        /* one register type load */
                        gen.load(r, vstack[vtop]);
                    }
                }
                vstack[vtop].r = r;
            }
            return r;
        }

        public void gv2() { }
        public void rc_fret() { }
        public void reg_fret() { }
        public void lexpand() { }
        public void lbuild() { }
        public void gv_dup() { }
        public void gvtst() { }
        public void gen_opl() { }
        public void gen_opic_sdiv() { }
        public void gen_opic_lt() { }
        public void gen_opic() { }
        public void gen_opif() { }
        public void pointed_size() { }
        public void vla_runtime_pointed_size() { }
        public void is_null_pointer() { }
        public void is_integer_btype() { }
        public void check_comparison_pointer_types() { }

        /* generic gen_op: handles types problems */
        public void gen_op(int op)
        {
            int u;
            int t1;
            int t2;
            int bt1;
            int bt2;
            int t;
            CType type1;

//redo:
            t1 = vstack[vtop - 1].type.t;
            t2 = vstack[vtop].type.t;
            bt1 = t1 & VT_BTYPE;
            bt2 = t2 & VT_BTYPE;

//    if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
//        tcc_error("operation on a struct");
//    } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
//        if (bt2 == VT_FUNC) {
//            mk_pointer(&vtop->type);
//            gaddrof();
//        }
//        if (bt1 == VT_FUNC) {
//            vswap();
//            mk_pointer(&vtop->type);
//            gaddrof();
//            vswap();
//        }
//        goto redo;
//    } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
//        /* at least one operand is a pointer */
//        /* relational op: must be both pointers */
//        if (op >= TOK_ULT && op <= TOK_LOR) {
//            check_comparison_pointer_types(vtop - 1, vtop, op);
//            /* pointers are handled are unsigned */
//#if PTR_SIZE == 8
//            t = VT_LLONG | VT_UNSIGNED;
//#else
//            t = VT_INT | VT_UNSIGNED;
//#endif
//            goto std_op;
//        }
//        /* if both pointers, then it must be the '-' op */
//        if (bt1 == VT_PTR && bt2 == VT_PTR) {
//            if (op != '-')
//                tcc_error("cannot use pointers here");
//            check_comparison_pointer_types(vtop - 1, vtop, op);
//            /* XXX: check that types are compatible */
//            if (vtop[-1].type.t & VT_VLA) {
//                vla_runtime_pointed_size(&vtop[-1].type);
//            } else {
//                vpushi(pointed_size(&vtop[-1].type));
//            }
//            vrott(3);
//            gen_opic(op);
//            vtop->type.t = ptrdiff_type.t;
//            vswap();
//            gen_op(TOK_PDIV);
//        } else {
//            /* exactly one pointer : must be '+' or '-'. */
//            if (op != '-' && op != '+')
//                tcc_error("cannot use pointers here");
//            /* Put pointer as first operand */
//            if (bt2 == VT_PTR) {
//                vswap();
//                t = t1, t1 = t2, t2 = t;
//            }
//#if PTR_SIZE == 4
//            if ((vtop[0].type.t & VT_BTYPE) == VT_LLONG)
//                /* XXX: truncate here because gen_opl can't handle ptr + long long */
//                gen_cast_s(VT_INT);
//#endif
//            type1 = vtop[-1].type;
//            type1.t &= ~VT_ARRAY;
//            if (vtop[-1].type.t & VT_VLA)
//                vla_runtime_pointed_size(&vtop[-1].type);
//            else {
//                u = pointed_size(&vtop[-1].type);
//                if (u < 0)
//                    tcc_error("unknown array element size");
//#if PTR_SIZE == 8
//                vpushll(u);
//#else
//                /* XXX: cast to int ? (long long case) */
//                vpushi(u);
//#endif
//            }
//            gen_op('*');
//#if 0
//            * #ifdef CONFIG_TCC_BCHECK
//                The main reason to removing this code:
//#include <stdio.h>
//            int main ()
//            {
//                int v[10];
//                int i = 10;
//                int j = 9;
//                fprintf(stderr, "v+i-j  = %p\n", v+i-j);
//                fprintf(stderr, "v+(i-j)  = %p\n", v+(i-j));
//            }
//            When this code is on. then the output looks like 
//                v+i-j = 0xfffffffe
//                v+(i-j) = 0xbff84000
//                */
//                /* if evaluating constant expression, no code should be
//                generated, so no bound check */
//                if (tcc_state->do_bounds_check && !const_wanted) {
//                    /* if bounded pointers, we generate a special code to
//                    test bounds */
//                    if (op == '-') {
//                        vpushi(0);
//                        vswap();
//                        gen_op('-');
//                    }
//                    gen_bounded_ptr_add();
//                } else
//#endif
//                {
//                    gen_opic(op);
//                }
//                /* put again type if gen_opic() swaped operands */
//                vtop->type = type1;
//        }
//    } else if (is_float(bt1) || is_float(bt2)) {
//        /* compute bigger type and do implicit casts */
//        if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
//            t = VT_LDOUBLE;
//        } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
//            t = VT_DOUBLE;
//        } else {
//            t = VT_FLOAT;
//        }
//        /* floats can only be used for a few operations */
//        if (op != '+' && op != '-' && op != '*' && op != '/' &&
//            (op < TOK_ULT || op > TOK_GT))
//            tcc_error("invalid operands for binary operation");
//        goto std_op;
//    } else if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL) {
//        t = bt1 == VT_LLONG ? VT_LLONG : VT_INT;
//        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (t | VT_UNSIGNED))
//            t |= VT_UNSIGNED;
//        t |= (VT_LONG & t1);
//        goto std_op;
//    } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
//        /* cast to biggest op */
//        t = VT_LLONG | VT_LONG;
//        if (bt1 == VT_LLONG)
//            t &= t1;
//        if (bt2 == VT_LLONG)
//            t &= t2;
//        /* convert to unsigned if it does not fit in a long long */
//        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
//            (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
//            t |= VT_UNSIGNED;
//        goto std_op;
//    } else {
//        /* integer operations */
//        t = VT_INT | (VT_LONG & (t1 | t2));
//        /* convert to unsigned if it does not fit in an integer */
//        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
//            (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
//            t |= VT_UNSIGNED;
//std_op:
//        /* XXX: currently, some unsigned operations are explicit, so
//        we modify them here */
//        if (t & VT_UNSIGNED) {
//            if (op == TOK_SAR)
//                op = TOK_SHR;
//            else if (op == '/')
//                op = TOK_UDIV;
//            else if (op == '%')
//                op = TOK_UMOD;
//            else if (op == TOK_LT)
//                op = TOK_ULT;
//            else if (op == TOK_GT)
//                op = TOK_UGT;
//            else if (op == TOK_LE)
//                op = TOK_ULE;
//            else if (op == TOK_GE)
//                op = TOK_UGE;
//        }
//        vswap();
//        type1.t = t;
//        type1.ref = NULL;
//        gen_cast(&type1);
//        vswap();
//        /* special case for shifts and long long: we keep the shift as
//        an integer */
//        if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL)
//            type1.t = VT_INT;
//        gen_cast(&type1);
//        if (is_float(t))
//            gen_opif(op);
//        else
//            gen_opic(op);
//        if (op >= TOK_ULT && op <= TOK_GT) {
//            /* relational op: the result is an int */
//            vtop->type.t = VT_INT;
//        } else {
//            vtop->type.t = t;
//        }
//    }

    // Make sure that we have converted to an rvalue:
//    if (vtop->r & VT_LVAL)
//        gv(is_float(vtop->type.t & VT_BTYPE) ? RC_FLOAT : RC_INT);
        }

        public void gen_cvt_itof1() { }
        public void gen_cvt_ftoi1() { }
        public void force_charshort_cast() { }
        public void gen_cast_s() { }
        public void gen_cast() { }
        public void type_size() { }
        public void vla_runtime_type_size() { }
        public void vla_sp_restore() { }
        public void vla_sp_restore_root() { }
        public void pointed_type() { }
        public void mk_pointer() { }
        public void is_compatible_func() { }

        //- type declaration --------------------------------------------------

        public bool compare_types(CType type1, CType type2, int unqualified)
        {
            return true;        //dummy val
        }

        public bool is_compatible_types(CType type1, CType type2)
        {
            return compare_types(type1, type2, 0);
        }

        public void is_compatible_unqualified_types() { }
        public void type_to_str() { }

        public void gen_assign_cast(CType dt)
        {
        }

        public void vstore() { }
        public void inc() { }
        public void parse_mult_str() { }
        public void exact_log2p1() { }

        public void parse_attribute(AttributeDef ad)
        {
        }

        public void find_field() { }
        public void struct_add_offset() { }
        public void struct_layout() { }

        public void struct_decl(CType type, int u)
        {
        }

        public void sym_to_attr(AttributeDef ad, Sym s)
        {
        }

        public void parse_btype_qualify(CType type, int qualifiers)
        {
        }

        public bool parse_btype(CType type, AttributeDef ad)
        {
            int u = 0;
            int bt;
            int st;
            int g;
            Sym s;
            CType type1 = new CType();

            ad.reset();
            bool type_found = false;
            bool typespec_found = false;
            int t = VT_INT;
            bt = -1;
            st = -1;
            type.reff = null;

            while (true)
            {
                switch ((TPTOKEN)prep.tok)
                {
                    case TPTOKEN.TOK_EXTENSION:
                        /* currently, we really ignore extension */
                        prep.next();
                        continue;

                    //type-specifier

                    /* basic types */
                    case TPTOKEN.TOK_CHAR:
                        u = VT_BYTE;
                    basic_type:
                        prep.next();
                    basic_type1:
                        if (u == VT_SHORT || u == VT_LONG)
                        {
                            if (st != -1 || (bt != -1 && bt != VT_INT))
                                tp.tp_error("too many basic types");
                            st = u;
                        }
                        else
                        {
                            if (bt != -1 || (st != -1 && u != VT_INT))
                                tp.tp_error("too many basic types");
                            bt = u;
                        }
                        if (u != VT_INT)
                            t = (t & ~(VT_BTYPE | VT_LONG)) | u;
                        typespec_found = true;
                        break;

                    case TPTOKEN.TOK_VOID:
                        u = VT_VOID;
                        goto basic_type;

                    case TPTOKEN.TOK_SHORT:
                        u = VT_SHORT;
                        goto basic_type;

                    case TPTOKEN.TOK_INT:
                        u = VT_INT;
                        goto basic_type;

                    case TPTOKEN.TOK_LONG:
                        if ((t & VT_BTYPE) == VT_DOUBLE)
                        {
                            t = (t & ~(VT_BTYPE | VT_LONG)) | VT_LDOUBLE;
                        }
                        else if ((t & (VT_BTYPE | VT_LONG)) == VT_LONG)
                        {
                            t = (t & ~(VT_BTYPE | VT_LONG)) | VT_LLONG;
                        }
                        else
                        {
                            u = VT_LONG;
                            goto basic_type;
                        }
                        prep.next();
                        break;

                    case TPTOKEN.TOK_BOOL:
                        u = VT_BOOL;
                        goto basic_type;

                    case TPTOKEN.TOK_FLOAT:
                        u = VT_FLOAT;
                        goto basic_type;

                    case TPTOKEN.TOK_DOUBLE:
                        if ((t & (VT_BTYPE | VT_LONG)) == VT_LONG)
                        {
                            t = (t & ~(VT_BTYPE | VT_LONG)) | VT_LDOUBLE;
                        }
                        else
                        {
                            u = VT_DOUBLE;
                            goto basic_type;
                        }
                        prep.next();
                        break;

                    //structured typed
                    case TPTOKEN.TOK_ENUM:
                        struct_decl(type1, VT_ENUM);
                    basic_type2:
                        u = type1.t;
                        type.reff = type1.reff;
                        goto basic_type1;

                    case TPTOKEN.TOK_STRUCT:
                        struct_decl(type1, VT_STRUCT);
                        goto basic_type2;

                    case TPTOKEN.TOK_UNION:
                        struct_decl(type1, VT_UNION);
                        goto basic_type2;

                    /* type modifiers */
                    case TPTOKEN.TOK_CONST1:
                    case TPTOKEN.TOK_CONST2:
                    case TPTOKEN.TOK_CONST3:
                        type.t = t;
                        parse_btype_qualify(type, VT_CONSTANT);
                        t = type.t;
                        prep.next();
                        break;

                    case TPTOKEN.TOK_VOLATILE1:
                    case TPTOKEN.TOK_VOLATILE2:
                    case TPTOKEN.TOK_VOLATILE3:
                        type.t = t;
                        parse_btype_qualify(type, VT_VOLATILE);
                        t = type.t;
                        prep.next();
                        break;

                    case TPTOKEN.TOK_SIGNED1:
                    case TPTOKEN.TOK_SIGNED2:
                    case TPTOKEN.TOK_SIGNED3:
                        if ((t & (VT_DEFSIGN | VT_UNSIGNED)) == (VT_DEFSIGN | VT_UNSIGNED))
                            tp.tp_error("signed and unsigned modifier");
                        t |= VT_DEFSIGN;
                        prep.next();
                        typespec_found = true;
                        break;

                    case TPTOKEN.TOK_UNSIGNED:
                        if ((t & (VT_DEFSIGN | VT_UNSIGNED)) == VT_DEFSIGN)
                            tp.tp_error("signed and unsigned modifier");
                        t |= VT_DEFSIGN | VT_UNSIGNED;
                        prep.next();
                        typespec_found = true;
                        break;

                    //we skip these
                    case TPTOKEN.TOK_REGISTER:
                    case TPTOKEN.TOK_AUTO:
                    case TPTOKEN.TOK_RESTRICT1:
                    case TPTOKEN.TOK_RESTRICT2:
                    case TPTOKEN.TOK_RESTRICT3:
                        prep.next();
                        break;

                    /* storage */
                    case TPTOKEN.TOK_EXTERN:
                        g = VT_EXTERN;
                        goto storage;
                    case TPTOKEN.TOK_STATIC:
                        g = VT_STATIC;
                        goto storage;
                    case TPTOKEN.TOK_TYPEDEF:
                        g = VT_TYPEDEF;
                        goto storage;
                    storage:
                        if ((t & (VT_EXTERN | VT_STATIC | VT_TYPEDEF) & ~g) != 0)
                            tp.tp_error("multiple storage classes");
                        t |= g;
                        prep.next();
                        break;

                    case TPTOKEN.TOK_INLINE1:
                    case TPTOKEN.TOK_INLINE2:
                    case TPTOKEN.TOK_INLINE3:
                        t |= VT_INLINE;
                        prep.next();
                        break;

                    /* GNUC attribute */
                    case TPTOKEN.TOK_ATTRIBUTE1:
                    case TPTOKEN.TOK_ATTRIBUTE2:
                        parse_attribute(ad);
                        if (ad.attr_mode != 0)
                        {
                            u = ad.attr_mode - 1;
                            t = (t & ~(VT_BTYPE | VT_LONG)) | u;
                        }
                        break;

                    /* GNUC typeof */
                    case TPTOKEN.TOK_TYPEOF1:
                    case TPTOKEN.TOK_TYPEOF2:
                    case TPTOKEN.TOK_TYPEOF3:
                        prep.next();
                        parse_expr_type(type1);
                        /* remove all storage modifiers except typedef */
                        type1.t &= ~(VT_STORAGE & ~VT_TYPEDEF);
                        if (type1.reff != null)
                            sym_to_attr(ad, type1.reff);
                        goto basic_type2;

                    default:
                        if (typespec_found)
                            goto the_end;

                        s = sym_find(prep.tok);
                        if (s == null || !((s.type.t & VT_TYPEDEF) != 0))
                            goto the_end;

                        t &= ~(VT_BTYPE | VT_LONG);
                        u = t & ~(VT_CONSTANT | VT_VOLATILE);
                        t ^= u;
                        type.t = (s.type.t & ~VT_TYPEDEF) | u;
                        type.reff = s.type.reff;

                        if (t > 0)
                            parse_btype_qualify(type, t);

                        t = type.t;
                        /* get attributes from typedef */
                        sym_to_attr(ad, s);
                        prep.next();
                        typespec_found = true;
                        st = bt = -2;
                        break;
                }
                type_found = true;
            }

        the_end:
            if (tp.char_is_unsigned)
            {
                if ((t & (VT_DEFSIGN | VT_BTYPE)) == VT_BYTE)
                    t |= VT_UNSIGNED;
            }

            /* VT_LONG is used just as a modifier for VT_INT / VT_LLONG */
            bt = t & (VT_BTYPE | VT_LONG);
            if (bt == VT_LONG)
                t |= LONG_SIZE == 8 ? VT_LLONG : VT_INT;

            if (bt == VT_LDOUBLE)
                t = (t & ~(VT_BTYPE | VT_LONG)) | VT_DOUBLE;

            type.t = t;
            return type_found;
        }

        public void convert_parameter_type() { }
        public void parse_asm_str() { }
        public void asm_label_instr() { }

        public int post_type(CType type, AttributeDef ad, int storage, int td)
        {
            int n;
            FuncAttr.FUNCTYPE l = FuncAttr.FUNCTYPE.FUNC_NONE;
            int t1;
            int arg_size;
            int align;
            //Sym **plast,  
            Sym first;
            Sym s;
            AttributeDef ad1;
            CType pt;

            if (prep.tok == '(')
            {
                /* function type, or recursive declarator (return if so) */
                prep.next();
                if ((td != 0) && !((td & TYPE_ABSTRACT) != 0))
                    return 0;
                if (prep.tok == ')')
                    l = FuncAttr.FUNCTYPE.FUNC_NONE;								//empty param list
                //else if (parse_btype(&pt, &ad1))
                //  l = FUNC_NEW;
                //else if (td)
                //  return 0;
                //else
                //  l = FUNC_OLD;

                first = null;
                //plast = &first;
                arg_size = 0;
                if (l > FuncAttr.FUNCTYPE.FUNC_NONE)
                {
                    for (; ; )
                    {
                        //                /* read param name and compute offset */
                        //                if (l != FUNC_OLD) {
                        //                    if ((pt.t & VT_BTYPE) == VT_VOID && tok == ')')
                        //                        break;
                        //                    type_decl(&pt, &ad1, &n, TYPE_DIRECT | TYPE_ABSTRACT);
                        //                    if ((pt.t & VT_BTYPE) == VT_VOID)
                        //                        tcc_error("parameter declared as void");
                        //                    arg_size += (type_size(&pt, &align) + PTR_SIZE - 1) / PTR_SIZE;
                        //                } else {
                        //                    n = tok;
                        //                    if (n < TOK_UIDENT)
                        //                        expect("identifier");
                        //                    pt.t = VT_VOID; /* invalid type */
                        //                    next();
                        //                }
                        //                convert_parameter_type(&pt);
                        //                s = sym_push(n | SYM_FIELD, &pt, 0, 0);
                        //                *plast = s;
                        //                plast = &s->next;
                        //                if (tok == ')')
                        //                    break;
                        //                skip(',');
                        //                if (l == FUNC_NEW && tok == TOK_DOTS) {
                        //                    l = FUNC_ELLIPSIS;
                        //                    next();
                        //                    break;
                        //                }
                        //		if (l == FUNC_NEW && !parse_btype(&pt, &ad1))
                        //		    tcc_error("invalid type");
                    }
                }
                else
                    /* if no parameters, then old type prototype */
                    l = FuncAttr.FUNCTYPE.FUNC_OLD;

                prep.skip(')');		//closing paren

                /* NOTE: const is ignored in returned type as it has a special meaning in gcc / C++ */
                //        type->t &= ~VT_CONSTANT; 
                //        /* some ancient pre-K&R C allows a function to return an array
                //           and the array brackets to be put after the arguments, such 
                //           that "int c()[]" means something like "int[] c()" */
                //        if (tok == '[') {
                //            next();
                //            skip(']'); /* only handle simple "[]" */
                //            mk_pointer(type);
                //        }

                /* we push a anonymous symbol which will contain the function prototype */
                ad.f.func_args = arg_size;
                ad.f.func_type = l;
                s = sym_push(SYM_FIELD, type, 0, 0);
                s.a = ad.a;
                s.f = ad.f;
                s.next = first;
                type.t = VT_FUNC;
                type.reff = s;
            }

            else if (prep.tok == '[')
            {
                //	int saved_nocode_wanted = nocode_wanted;
                //        /* array definition */
                //        next();
                //        if (tok == TOK_RESTRICT1)
                //            next();
                //        n = -1;
                //        t1 = 0;
                //        if (tok != ']') {
                //            if (!local_stack || (storage & VT_STATIC))
                //                vpushi(expr_const());
                //            else {
                //		/* VLAs (which can only happen with local_stack && !VT_STATIC)
                //		   length must always be evaluated, even under nocode_wanted,
                //		   so that its size slot is initialized (e.g. under sizeof
                //		   or typeof).  */
                //		nocode_wanted = 0;
                //		gexpr();
                //	    }
                //            if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                //                n = vtop->c.i;
                //                if (n < 0)
                //                    tcc_error("invalid array size");
                //            } else {
                //                if (!is_integer_btype(vtop->type.t & VT_BTYPE))
                //                    tcc_error("size of variable length array should be an integer");
                //                t1 = VT_VLA;
                //            }
                //        }
                //        skip(']');
                //        /* parse next post type */
                //        post_type(type, ad, storage, 0);
                //        if (type->t == VT_FUNC)
                //            tcc_error("declaration of an array of functions");
                //        t1 |= type->t & VT_VLA;
                //        
                //        if (t1 & VT_VLA) {
                //            loc -= type_size(&int_type, &align);
                //            loc &= -align;
                //            n = loc;
                //
                //            vla_runtime_type_size(type, &align);
                //            gen_op('*');
                //            vset(&int_type, VT_LOCAL|VT_LVAL, n);
                //            vswap();
                //            vstore();
                //        }
                //        if (n != -1)
                //            vpop();
                //	nocode_wanted = saved_nocode_wanted;
                //                
                //        /* we push an anonymous symbol which will contain the array
                //           element type */
                //        s = sym_push(SYM_FIELD, type, 0, n);
                //        type->t = (t1 ? VT_VLA : VT_ARRAY) | VT_PTR;
                //        type->ref = s;
            }

            return 1;
        }

        public CType type_decl(CType type, AttributeDef ad, ref int v, int td)
        {
            CType post;
            CType ret;
            int qualifiers;
            int storage;

            /* recursive type, remove storage bits first, apply them later again */
            storage = type.t & VT_STORAGE;
            type.t &= ~VT_STORAGE;
            post = ret = type;

            while (prep.tok == '*')
            {
                //        qualifiers = 0;
                //    redo:
                //        next();
                //        switch(tok) {
                //        case TOK_CONST1:
                //        case TOK_CONST2:
                //        case TOK_CONST3:
                //            qualifiers |= VT_CONSTANT;
                //            goto redo;
                //        case TOK_VOLATILE1:
                //        case TOK_VOLATILE2:
                //        case TOK_VOLATILE3:
                //            qualifiers |= VT_VOLATILE;
                //            goto redo;
                //        case TOK_RESTRICT1:
                //        case TOK_RESTRICT2:
                //        case TOK_RESTRICT3:
                //            goto redo;
                //	/* XXX: clarify attribute handling */
                //	case TOK_ATTRIBUTE1:
                //	case TOK_ATTRIBUTE2:
                //	    parse_attribute(ad);
                //	    break;
                //        }
                //        mk_pointer(type);
                //        type->t |= qualifiers;
                //	if (ret == type)
                //	    /* innermost pointed to type is the one for the first derivation */
                //	    ret = pointed_type(type);
            }

            if (prep.tok == '(')
            {
                /* This is possibly a parameter type list for abstract declarators ('int ()'), use post_type for testing this.  */
                //	if (!post_type(type, ad, 0, td)) {
                //	    /* It's not, so it's a nested declarator, and the post operations
                //	       apply to the innermost pointed to type (if any).  */
                //	    /* XXX: this is not correct to modify 'ad' at this point, but
                //	       the syntax is not clear */
                //	    parse_attribute(ad);
                //	    post = type_decl(type, ad, v, td);
                //	    skip(')');
                //	}
            }
            else if ((prep.tok >= (int)TPTOKEN.TOK_IDENT) && ((td & TYPE_DIRECT) != 0))
            {
                /* type identifier */
                v = prep.tok;				//save function or var ident
                prep.next();
            }
            else
            {
                //	if (!(td & TYPE_ABSTRACT))
                //	  expect("identifier");
                //	*v = 0;
            }
            post_type(post, ad, storage, 0);
            parse_attribute(ad);
            type.t |= storage;
            return ret;
        }

        public void lvalue_type() { }
        public void indir() { }
        public void gfunc_param_typed() { }
        public void expr_type() { }

        public void parse_expr_type(CType type)
        {
        }

        public void parse_type() { }
        public void parse_builtin_params() { }

        //- expressions -------------------------------------------------------

        public void unary()
        {
            int n;
            int t;
            int align;
            int size;
            int r;
            int sizeof_caller;
            CType type = new CType();
            Sym s;
            AttributeDef ad = new AttributeDef();

            sizeof_caller = in_sizeof;
            in_sizeof = 0;
            type.reff = null;

            /* XXX: GCC 2.95.3 does not generate a table although it should be better here */
        tok_next:
            switch (prep.tok)
            {
                case (int)TPTOKEN.TOK_EXTENSION:
                    prep.next();
                    goto tok_next;

                //    case TOK_LCHAR:
                //#ifdef TCC_TARGET_PE
                //        t = VT_SHORT|VT_UNSIGNED;
                //        goto push_tokc;
                //#endif

                case (int)TPTOKEN.TOK_CINT:
                case (int)TPTOKEN.TOK_CCHAR:
                    t = VT_INT;
                push_tokc:
                    type.t = t;
                    vsetc(type, VT_CONST, prep.tokc);
                    prep.next();
                    break;

                    //    case TOK_CUINT:
                    //        t = VT_INT | VT_UNSIGNED;
                    //        goto push_tokc;
                    //    case TOK_CLLONG:
                    //        t = VT_LLONG;
                    //        goto push_tokc;
                    //    case TOK_CULLONG:
                    //        t = VT_LLONG | VT_UNSIGNED;
                    //        goto push_tokc;
                    //    case TOK_CFLOAT:
                    //        t = VT_FLOAT;
                    //        goto push_tokc;
                    //    case TOK_CDOUBLE:
                    //        t = VT_DOUBLE;
                    //        goto push_tokc;
                    //    case TOK_CLDOUBLE:
                    //        t = VT_LDOUBLE;
                    //        goto push_tokc;
                    //    case TOK_CLONG:
                    //        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG;
                    //        goto push_tokc;
                    //    case TOK_CULONG:
                    //        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG | VT_UNSIGNED;
                    //        goto push_tokc;
                    //    case TOK___FUNCTION__:
                    //        if (!gnu_ext)
                    //            goto tok_identifier;
                    //        /* fall thru */
                    //    case TOK___FUNC__:
                    //        {
                    //            void *ptr;
                    //            int len;
                    //            /* special function name identifier */
                    //            len = strlen(funcname) + 1;
                    //            /* generate char[len] type */
                    //            type.t = VT_BYTE;
                    //            mk_pointer(&type);
                    //            type.t |= VT_ARRAY;
                    //            type.ref->c = len;
                    //            vpush_ref(&type, data_section, data_section->data_offset, len);
                    //            if (!NODATA_WANTED) {
                    //                ptr = section_ptr_add(data_section, len);
                    //                memcpy(ptr, funcname, len);
                    //            }
                    //            next();
                    //        }
                    //        break;
                    //    case TOK_LSTR:
                    //#ifdef TCC_TARGET_PE
                    //        t = VT_SHORT | VT_UNSIGNED;
                    //#else
                    //        t = VT_INT;
                    //#endif
                    //        goto str_init;
                    //    case TOK_STR:
                    //        /* string parsing */
                    //        t = VT_BYTE;
                    //        if (tcc_state->char_is_unsigned)
                    //            t = VT_BYTE | VT_UNSIGNED;
                    //str_init:
                    //        if (tcc_state->warn_write_strings)
                    //            t |= VT_CONSTANT;
                    //        type.t = t;
                    //        mk_pointer(&type);
                    //        type.t |= VT_ARRAY;
                    //        memset(&ad, 0, sizeof(AttributeDef));
                    //        decl_initializer_alloc(&type, &ad, VT_CONST, 2, 0, 0);
                    //        break;
                    //    case '(':
                    //        next();
                    //        /* cast ? */
                    //        if (parse_btype(&type, &ad)) {
                    //            type_decl(&type, &ad, &n, TYPE_ABSTRACT);
                    //            skip(')');
                    //            /* check ISOC99 compound literal */
                    //            if (tok == '{') {
                    //                /* data is allocated locally by default */
                    //                if (global_expr)
                    //                    r = VT_CONST;
                    //                else
                    //                    r = VT_LOCAL;
                    //                /* all except arrays are lvalues */
                    //                if (!(type.t & VT_ARRAY))
                    //                    r |= lvalue_type(type.t);
                    //                memset(&ad, 0, sizeof(AttributeDef));
                    //                decl_initializer_alloc(&type, &ad, r, 1, 0, 0);
                    //            } else {
                    //                if (sizeof_caller) {
                    //                    vpush(&type);
                    //                    return;
                    //                }
                    //                unary();
                    //                gen_cast(&type);
                    //            }
                    //        } else if (tok == '{') {
                    //            int saved_nocode_wanted = nocode_wanted;
                    //            if (const_wanted)
                    //                tcc_error("expected constant");
                    //            /* save all registers */
                    //            save_regs(0);
                    //            /* statement expression : we do not accept break/continue
                    //            inside as GCC does.  We do retain the nocode_wanted state,
                    //            as statement expressions can't ever be entered from the
                    //            outside, so any reactivation of code emission (from labels
                    //            or loop heads) can be disabled again after the end of it. */
                    //            block(NULL, NULL, 1);
                    //            nocode_wanted = saved_nocode_wanted;
                    //            skip(')');
                    //        } else {
                    //            gexpr();
                    //            skip(')');
                    //        }
                    //        break;
                    //    case '*':
                    //        next();
                    //        unary();
                    //        indir();
                    //        break;
                    //    case '&':
                    //        next();
                    //        unary();
                    //        /* functions names must be treated as function pointers,
                    //        except for unary '&' and sizeof. Since we consider that
                    //        functions are not lvalues, we only have to handle it
                    //        there and in function calls. */
                    //        /* arrays can also be used although they are not lvalues */
                    //        if ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
                    //            !(vtop->type.t & VT_ARRAY))
                    //            test_lvalue();
                    //        mk_pointer(&vtop->type);
                    //        gaddrof();
                    //        break;
                    //    case '!':
                    //        next();
                    //        unary();
                    //        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                    //            gen_cast_s(VT_BOOL);
                    //            vtop->c.i = !vtop->c.i;
                    //        } else if ((vtop->r & VT_VALMASK) == VT_CMP)
                    //            vtop->c.i ^= 1;
                    //        else {
                    //            save_regs(1);
                    //            vseti(VT_JMP, gvtst(1, 0));
                    //        }
                    //        break;
                    //    case '~':
                    //        next();
                    //        unary();
                    //        vpushi(-1);
                    //        gen_op('^');
                    //        break;
                    //    case '+':
                    //        next();
                    //        unary();
                    //        if ((vtop->type.t & VT_BTYPE) == VT_PTR)
                    //            tcc_error("pointer not accepted for unary plus");
                    //        /* In order to force cast, we add zero, except for floating point
                    //        where we really need an noop (otherwise -0.0 will be transformed
                    //        into +0.0).  */
                    //        if (!is_float(vtop->type.t)) {
                    //            vpushi(0);
                    //            gen_op('+');
                    //        }
                    //        break;
                    //    case TOK_SIZEOF:
                    //    case TOK_ALIGNOF1:
                    //    case TOK_ALIGNOF2:
                    //        t = tok;
                    //        next();
                    //        in_sizeof++;
                    //        expr_type(&type, unary); /* Perform a in_sizeof = 0; */
                    //        s = vtop[1].sym; /* hack: accessing previous vtop */
                    //        size = type_size(&type, &align);
                    //        if (s && s->a.aligned)
                    //            align = 1 << (s->a.aligned - 1);
                    //        if (t == TOK_SIZEOF) {
                    //            if (!(type.t & VT_VLA)) {
                    //                if (size < 0)
                    //                    tcc_error("sizeof applied to an incomplete type");
                    //                vpushs(size);
                    //            } else {
                    //                vla_runtime_type_size(&type, &align);
                    //            }
                    //        } else {
                    //            vpushs(align);
                    //        }
                    //        vtop->type.t |= VT_UNSIGNED;
                    //        break;

                    //    case TOK_builtin_expect:
                    //        /* __builtin_expect is a no-op for now */
                    //        parse_builtin_params(0, "ee");
                    //        vpop();
                    //        break;
                    //    case TOK_builtin_types_compatible_p:
                    //        parse_builtin_params(0, "tt");
                    //        vtop[-1].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
                    //        vtop[0].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
                    //        n = is_compatible_types(&vtop[-1].type, &vtop[0].type);
                    //        vtop -= 2;
                    //        vpushi(n);
                    //        break;
                    //    case TOK_builtin_choose_expr:
                    //        {
                    //            int64_t c;
                    //            next();
                    //            skip('(');
                    //            c = expr_const64();
                    //            skip(',');
                    //            if (!c) {
                    //                nocode_wanted++;
                    //            }
                    //            expr_eq();
                    //            if (!c) {
                    //                vpop();
                    //                nocode_wanted--;
                    //            }
                    //            skip(',');
                    //            if (c) {
                    //                nocode_wanted++;
                    //            }
                    //            expr_eq();
                    //            if (c) {
                    //                vpop();
                    //                nocode_wanted--;
                    //            }
                    //            skip(')');
                    //        }
                    //        break;
                    //    case TOK_builtin_constant_p:
                    //        parse_builtin_params(1, "e");
                    //        n = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
                    //        vtop--;
                    //        vpushi(n);
                    //        break;
                    //    case TOK_builtin_frame_address:
                    //    case TOK_builtin_return_address:
                    //        {
                    //            int tok1 = tok;
                    //            int level;
                    //            next();
                    //            skip('(');
                    //            if (tok != TOK_CINT) {
                    //                tcc_error("%s only takes positive integers",
                    //                    tok1 == TOK_builtin_return_address ?
                    //                    "__builtin_return_address" :
                    //                "__builtin_frame_address");
                    //            }
                    //            level = (uint32_t)tokc.i;
                    //            next();
                    //            skip(')');
                    //            type.t = VT_VOID;
                    //            mk_pointer(&type);
                    //            vset(&type, VT_LOCAL, 0);       /* local frame */
                    //            while (level--) {
                    //                mk_pointer(&vtop->type);
                    //                indir();                    /* -> parent frame */
                    //            }
                    //            if (tok1 == TOK_builtin_return_address) {
                    //                // assume return address is just above frame pointer on stack
                    //                vpushi(PTR_SIZE);
                    //                gen_op('+');
                    //                mk_pointer(&vtop->type);
                    //                indir();
                    //            }
                    //        }
                    //        break;
                    //#ifdef TCC_TARGET_X86_64
                    //#ifdef TCC_TARGET_PE
                    //    case TOK_builtin_va_start:
                    //        parse_builtin_params(0, "ee");
                    //        r = vtop->r & VT_VALMASK;
                    //        if (r == VT_LLOCAL)
                    //            r = VT_LOCAL;
                    //        if (r != VT_LOCAL)
                    //            tcc_error("__builtin_va_start expects a local variable");
                    //        vtop->r = r;
                    //        vtop->type = char_pointer_type;
                    //        vtop->c.i += 8;
                    //        vstore();
                    //        break;
                    //#else
                    //    case TOK_builtin_va_arg_types:
                    //        parse_builtin_params(0, "t");
                    //        vpushi(classify_x86_64_va_arg(&vtop->type));
                    //        vswap();
                    //        vpop();
                    //        break;
                    //#endif
                    //#endif

                    //#ifdef TCC_TARGET_ARM64
                    //    case TOK___va_start: {
                    //        parse_builtin_params(0, "ee");
                    //        //xx check types
                    //        gen_va_start();
                    //        vpushi(0);
                    //        vtop->type.t = VT_VOID;
                    //        break;
                    //                         }
                    //    case TOK___va_arg: {
                    //        parse_builtin_params(0, "et");
                    //        type = vtop->type;
                    //        vpop();
                    //        //xx check types
                    //        gen_va_arg(&type);
                    //        vtop->type = type;
                    //        break;
                    //                       }
                    //    case TOK___arm64_clear_cache: {
                    //        parse_builtin_params(0, "ee");
                    //        gen_clear_cache();
                    //        vpushi(0);
                    //        vtop->type.t = VT_VOID;
                    //        break;
                    //                                  }
                    //#endif
                    //                                  /* pre operations */
                    //    case TOK_INC:
                    //    case TOK_DEC:
                    //        t = tok;
                    //        next();
                    //        unary();
                    //        inc(0, t);
                    //        break;
                    //    case '-':
                    //        next();
                    //        unary();
                    //        t = vtop->type.t & VT_BTYPE;
                    //        if (is_float(t)) {
                    //            /* In IEEE negate(x) isn't subtract(0,x), but rather
                    //            subtract(-0, x).  */
                    //            vpush(&vtop->type);
                    //            if (t == VT_FLOAT)
                    //                vtop->c.f = -1.0 * 0.0;
                    //            else if (t == VT_DOUBLE)
                    //                vtop->c.d = -1.0 * 0.0;
                    //            else
                    //                vtop->c.ld = -1.0 * 0.0;
                    //        } else
                    //            vpushi(0);
                    //        vswap();
                    //        gen_op('-');
                    //        break;
                    //    case TOK_LAND:
                    //        if (!gnu_ext)
                    //            goto tok_identifier;
                    //        next();
                    //        /* allow to take the address of a label */
                    //        if (tok < TOK_UIDENT)
                    //            expect("label identifier");
                    //        s = label_find(tok);
                    //        if (!s) {
                    //            s = label_push(&global_label_stack, tok, LABEL_FORWARD);
                    //        } else {
                    //            if (s->r == LABEL_DECLARED)
                    //                s->r = LABEL_FORWARD;
                    //        }
                    //        if (!s->type.t) {
                    //            s->type.t = VT_VOID;
                    //            mk_pointer(&s->type);
                    //            s->type.t |= VT_STATIC;
                    //        }
                    //        vpushsym(&s->type, s);
                    //        next();
                    //        break;

                    //    case TOK_GENERIC:
                    //        {
                    //            CType controlling_type;
                    //            int has_default = 0;
                    //            int has_match = 0;
                    //            int learn = 0;
                    //            TokenString *str = NULL;

                    //            next();
                    //            skip('(');
                    //            expr_type(&controlling_type, expr_eq);
                    //            controlling_type.t &= ~(VT_CONSTANT | VT_VOLATILE | VT_ARRAY);
                    //            for (;;) {
                    //                learn = 0;
                    //                skip(',');
                    //                if (tok == TOK_DEFAULT) {
                    //                    if (has_default)
                    //                        tcc_error("too many 'default'");
                    //                    has_default = 1;
                    //                    if (!has_match)
                    //                        learn = 1;
                    //                    next();
                    //                } else {
                    //                    AttributeDef ad_tmp;
                    //                    int itmp;
                    //                    CType cur_type;
                    //                    parse_btype(&cur_type, &ad_tmp);
                    //                    type_decl(&cur_type, &ad_tmp, &itmp, TYPE_ABSTRACT);
                    //                    if (compare_types(&controlling_type, &cur_type, 0)) {
                    //                        if (has_match) {
                    //                            tcc_error("type match twice");
                    //                        }
                    //                        has_match = 1;
                    //                        learn = 1;
                    //                    }
                    //                }
                    //                skip(':');
                    //                if (learn) {
                    //                    if (str)
                    //                        tok_str_free(str);
                    //                    skip_or_save_block(&str);
                    //                } else {
                    //                    skip_or_save_block(NULL);
                    //                }
                    //                if (tok == ')')
                    //                    break;
                    //            }
                    //            if (!str) {
                    //                char buf[60];
                    //                type_to_str(buf, sizeof buf, &controlling_type, NULL);
                    //                tcc_error("type '%s' does not match any association", buf);
                    //            }
                    //            begin_macro(str, 1);
                    //            next();
                    //            expr_eq();
                    //            if (tok != TOK_EOF)
                    //                expect(",");
                    //            end_macro();
                    //            next();
                    //            break;
                    //        }
                    //        // special qnan , snan and infinity values
                    //    case TOK___NAN__:
                    //        vpush64(VT_DOUBLE, 0x7ff8000000000000ULL);
                    //        next();
                    //        break;
                    //    case TOK___SNAN__:
                    //        vpush64(VT_DOUBLE, 0x7ff0000000000001ULL);
                    //        next();
                    //        break;
                    //    case TOK___INF__:
                    //        vpush64(VT_DOUBLE, 0x7ff0000000000000ULL);
                    //        next();
                    //        break;

                    //    default:
                    //tok_identifier:
                    //        t = tok;
                    //        next();
                    //        if (t < TOK_UIDENT)
                    //            expect("identifier");
                    //        s = sym_find(t);
                    //        if (!s || IS_ASM_SYM(s)) {
                    //            const char *name = get_tok_str(t, NULL);
                    //            if (tok != '(')
                    //                tcc_error("'%s' undeclared", name);
                    //            /* for simple function calls, we tolerate undeclared
                    //            external reference to int() function */
                    //            if (tcc_state->warn_implicit_function_declaration
                    //#ifdef TCC_TARGET_PE
                    //                /* people must be warned about using undeclared WINAPI functions
                    //                (which usually start with uppercase letter) */
                    //                || (name[0] >= 'A' && name[0] <= 'Z')
                    //#endif
                    //                )
                    //                tcc_warning("implicit declaration of function '%s'", name);
                    //            s = external_global_sym(t, &func_old_type, 0); 
                    //        }

                    //        r = s->r;
                    //        /* A symbol that has a register is a local register variable,
                    //        which starts out as VT_LOCAL value.  */
                    //        if ((r & VT_VALMASK) < VT_CONST)
                    //            r = (r & ~VT_VALMASK) | VT_LOCAL;

                    //        vset(&s->type, r, s->c);
                    //        /* Point to s as backpointer (even without r&VT_SYM).
                    //        Will be used by at least the x86 inline asm parser for
                    //        regvars.  */
                    //        vtop->sym = s;

                    //        if (r & VT_SYM) {
                    //            vtop->c.i = 0;
                    //        } else if (r == VT_CONST && IS_ENUM_VAL(s->type.t)) {
                    //            vtop->c.i = s->enum_val;
                    //        }
                    break;
            }

            /* post operations */
            //    while (1) {
            //        if (tok == TOK_INC || tok == TOK_DEC) {
            //            inc(1, tok);
            //            next();
            //        } else if (tok == '.' || tok == TOK_ARROW || tok == TOK_CDOUBLE) {
            //            int qualifiers;
            //            /* field */ 
            //            if (tok == TOK_ARROW) 
            //                indir();
            //            qualifiers = vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
            //            test_lvalue();
            //            gaddrof();
            //            /* expect pointer on structure */
            //            if ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
            //                expect("struct or union");
            //            if (tok == TOK_CDOUBLE)
            //                expect("field name");
            //            next();
            //            if (tok == TOK_CINT || tok == TOK_CUINT)
            //                expect("field name");
            //            s = find_field(&vtop->type, tok);
            //            if (!s)
            //                tcc_error("field not found: %s",  get_tok_str(tok & ~SYM_FIELD, &tokc));
            //            /* add field offset to pointer */
            //            vtop->type = char_pointer_type; /* change type to 'char *' */
            //            vpushi(s->c);
            //            gen_op('+');
            //            /* change type to field type, and set to lvalue */
            //            vtop->type = s->type;
            //            vtop->type.t |= qualifiers;
            //            /* an array is never an lvalue */
            //            if (!(vtop->type.t & VT_ARRAY)) {
            //                vtop->r |= lvalue_type(vtop->type.t);
            //#ifdef CONFIG_TCC_BCHECK
            //                /* if bound checking, the referenced pointer must be checked */
            //                if (tcc_state->do_bounds_check && (vtop->r & VT_VALMASK) != VT_LOCAL)
            //                    vtop->r |= VT_MUSTBOUND;
            //#endif
            //            }
            //            next();
            //        } else if (tok == '[') {
            //            next();
            //            gexpr();
            //            gen_op('+');
            //            indir();
            //            skip(']');
            //        } else if (tok == '(') {
            //            SValue ret;
            //            Sym *sa;
            //            int nb_args, ret_nregs, ret_align, regsize, variadic;

            //            /* function call  */
            //            if ((vtop->type.t & VT_BTYPE) != VT_FUNC) {
            //                /* pointer test (no array accepted) */
            //                if ((vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
            //                    vtop->type = *pointed_type(&vtop->type);
            //                    if ((vtop->type.t & VT_BTYPE) != VT_FUNC)
            //                        goto error_func;
            //                } else {
            //error_func:
            //                    expect("function pointer");
            //                }
            //            } else {
            //                vtop->r &= ~VT_LVAL; /* no lvalue */
            //            }
            //            /* get return type */
            //            s = vtop->type.ref;
            //            next();
            //            sa = s->next; /* first parameter */
            //            nb_args = regsize = 0;
            //            ret.r2 = VT_CONST;
            //            /* compute first implicit argument if a structure is returned */
            //            if ((s->type.t & VT_BTYPE) == VT_STRUCT) {
            //                variadic = (s->f.func_type == FUNC_ELLIPSIS);
            //                ret_nregs = gfunc_sret(&s->type, variadic, &ret.type,
            //                    &ret_align, &regsize);
            //                if (!ret_nregs) {
            //                    /* get some space for the returned structure */
            //                    size = type_size(&s->type, &align);
            //#ifdef TCC_TARGET_ARM64
            //                    /* On arm64, a small struct is return in registers.
            //                    It is much easier to write it to memory if we know
            //                    that we are allowed to write some extra bytes, so
            //                    round the allocated space up to a power of 2: */
            //                    if (size < 16)
            //                        while (size & (size - 1))
            //                            size = (size | (size - 1)) + 1;
            //#endif
            //                    loc = (loc - size) & -align;
            //                    ret.type = s->type;
            //                    ret.r = VT_LOCAL | VT_LVAL;
            //                    /* pass it as 'int' to avoid structure arg passing
            //                    problems */
            //                    vseti(VT_LOCAL, loc);
            //                    ret.c = vtop->c;
            //                    nb_args++;
            //                }
            //            } else {
            //                ret_nregs = 1;
            //                ret.type = s->type;
            //            }

            //            if (ret_nregs) {
            //                /* return in register */
            //                if (is_float(ret.type.t)) {
            //                    ret.r = reg_fret(ret.type.t);
            //#ifdef TCC_TARGET_X86_64
            //                    if ((ret.type.t & VT_BTYPE) == VT_QFLOAT)
            //                        ret.r2 = REG_QRET;
            //#endif
            //                } else {
            //#ifndef TCC_TARGET_ARM64
            //#ifdef TCC_TARGET_X86_64
            //                    if ((ret.type.t & VT_BTYPE) == VT_QLONG)
            //#else
            //                    if ((ret.type.t & VT_BTYPE) == VT_LLONG)
            //#endif
            //                        ret.r2 = REG_LRET;
            //#endif
            //                    ret.r = REG_IRET;
            //                }
            //                ret.c.i = 0;
            //            }
            //            if (tok != ')') {
            //                for(;;) {
            //                    expr_eq();
            //                    gfunc_param_typed(s, sa);
            //                    nb_args++;
            //                    if (sa)
            //                        sa = sa->next;
            //                    if (tok == ')')
            //                        break;
            //                    skip(',');
            //                }
            //            }
            //            if (sa)
            //                tcc_error("too few arguments to function");
            //            skip(')');
            //            gfunc_call(nb_args);

            //            /* return value */
            //            for (r = ret.r + ret_nregs + !ret_nregs; r-- > ret.r;) {
            //                vsetc(&ret.type, r, &ret.c);
            //                vtop->r2 = ret.r2; /* Loop only happens when r2 is VT_CONST */
            //            }

            //            /* handle packed struct return */
            //            if (((s->type.t & VT_BTYPE) == VT_STRUCT) && ret_nregs) {
            //                int addr, offset;

            //                size = type_size(&s->type, &align);
            //                /* We're writing whole regs often, make sure there's enough
            //                space.  Assume register size is power of 2.  */
            //                if (regsize > align)
            //                    align = regsize;
            //                loc = (loc - size) & -align;
            //                addr = loc;
            //                offset = 0;
            //                for (;;) {
            //                    vset(&ret.type, VT_LOCAL | VT_LVAL, addr + offset);
            //                    vswap();
            //                    vstore();
            //                    vtop--;
            //                    if (--ret_nregs == 0)
            //                        break;
            //                    offset += regsize;
            //                }
            //                vset(&s->type, VT_LOCAL | VT_LVAL, addr);
            //            }
            //        } else {
            //            break;
            //        }
            //    }
        }

        public void expr_prod()
        {
            int t;

            unary();
            //while (tok == '*' || tok == '/' || tok == '%')
            //{
            //    t = tok;
            //    next();
            //    unary();
            //    gen_op(t);
            //}
        }

        public void expr_sum()
        {
            int t;

            expr_prod();
            while (prep.tok == '+' || prep.tok == '-')
            {
                t = prep.tok;
                prep.next();
                expr_prod();
                gen_op(t);
            }
        }

        public void expr_shift()
        {
            int t;

            expr_sum();
            //while (tok == TOK_SHL || tok == TOK_SAR)
            //{
            //    t = tok;
            //    next();
            //    expr_sum();
            //    gen_op(t);
            //}
        }

        public void expr_cmp()
        {
            int t;

            expr_shift();
            //while ((tok >= TOK_ULE && tok <= TOK_GT) ||
            //    tok == TOK_ULT || tok == TOK_UGE)
            //{
            //    t = tok;
            //    next();
            //    expr_shift();
            //    gen_op(t);
            //}
        }

        public void expr_cmpeq()
        {
            int t;

            expr_cmp();
            //while (tok == TOK_EQ || tok == TOK_NE)
            //{
            //    t = tok;
            //    next();
            //    expr_cmp();
            //    gen_op(t);
            //}
        }

        public void expr_and()
        {
            expr_cmpeq();
            //while (tok == '&')
            //{
            //    next();
            //    expr_cmpeq();
            //    gen_op('&');
            //}
        }

        public void expr_xor()
        {
            expr_and();
            //while (tok == '^')
            //{
            //    next();
            //    expr_and();
            //    gen_op('^');
            //}
        }

        public void expr_or()
        {
            expr_xor();
            //while (tok == '|')
            //{
            //    next();
            //    expr_xor();
            //    gen_op('|');
            //}
        }

        public void expr_land()
        {
            expr_or();
            if (prep.tok == (int)TPTOKEN.TOK_LAND)
            {
                //    int t = 0;
                //    for (; ; )
                //    {
                //        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST)
                //        {
                //            gen_cast_s(VT_BOOL);
                //            if (vtop->c.i)
                //            {
                //                vpop();
                //            }
                //            else
                //            {
                //                nocode_wanted++;
                //                while (tok == TOK_LAND)
                //                {
                //                    next();
                //                    expr_or();
                //                    vpop();
                //                }
                //                nocode_wanted--;
                //                if (t)
                //                    gsym(t);
                //                gen_cast_s(VT_INT);
                //                break;
                //            }
                //        }
                //        else
                //        {
                //            if (!t)
                //                save_regs(1);
                //            t = gvtst(1, t);
                //        }
                //        if (tok != TOK_LAND)
                //        {
                //            if (t)
                //                vseti(VT_JMPI, t);
                //            else
                //                vpushi(1);
                //            break;
                //        }
                //        next();
                //        expr_or();
                //    }
            }
        }

        public void expr_lor()
        {
            expr_land();
            if (prep.tok == (int)TPTOKEN.TOK_LOR)
            {
                //    int t = 0;
                //    for (; ; )
                //    {
                //        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST)
                //        {
                //            gen_cast_s(VT_BOOL);
                //            if (!vtop->c.i)
                //            {
                //                vpop();
                //            }
                //            else
                //            {
                //                nocode_wanted++;
                //                while (tok == TOK_LOR)
                //                {
                //                    next();
                //                    expr_land();
                //                    vpop();
                //                }
                //                nocode_wanted--;
                //                if (t)
                //                    gsym(t);
                //                gen_cast_s(VT_INT);
                //                break;
                //            }
                //        }
                //        else
                //        {
                //            if (!t)
                //                save_regs(1);
                //            t = gvtst(0, t);
                //        }
                //        if (tok != TOK_LOR)
                //        {
                //            if (t)
                //                vseti(VT_JMP, t);
                //            else
                //                vpushi(0);
                //            break;
                //        }
                //        next();
                //        expr_land();
                //    }
            }
        }

        public void condition_3way() { }

        public void expr_cond()
        {
            int tt;
            int u;
            int r1;
            int r2;
            int rc;
            int t1;
            int t2;
            int bt1;
            int bt2;
            int islv;
            int c;
            int g;
            SValue sv;
            CType type;
            CType type1;
            CType type2;

            expr_lor();
            if (prep.tok == '?')
            {
                //        next();
                //        c = condition_3way();
                //        g = (tok == ':' && gnu_ext);
                //        if (c < 0) {
                //            /* needed to avoid having different registers saved in
                //            each branch */
                //            if (is_float(vtop->type.t)) {
                //                rc = RC_FLOAT;
                //#ifdef TCC_TARGET_X86_64
                //                if ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
                //                    rc = RC_ST0;
                //                }
                //#endif
                //            } else
                //                rc = RC_INT;
                //            gv(rc);
                //            save_regs(1);
                //            if (g)
                //                gv_dup();
                //            tt = gvtst(1, 0);

                //        } else {
                //            if (!g)
                //                vpop();
                //            tt = 0;
                //        }

                //        if (1) {
                //            if (c == 0)
                //                nocode_wanted++;
                //            if (!g)
                //                gexpr();

                //            type1 = vtop->type;
                //            sv = *vtop; /* save value to handle it later */
                //            vtop--; /* no vpop so that FP stack is not flushed */
                //            skip(':');

                //            u = 0;
                //            if (c < 0)
                //                u = gjmp(0);
                //            gsym(tt);

                //            if (c == 0)
                //                nocode_wanted--;
                //            if (c == 1)
                //                nocode_wanted++;
                //            expr_cond();
                //            if (c == 1)
                //                nocode_wanted--;

                //            type2 = vtop->type;
                //            t1 = type1.t;
                //            bt1 = t1 & VT_BTYPE;
                //            t2 = type2.t;
                //            bt2 = t2 & VT_BTYPE;
                //            type.ref = NULL;

                //            /* cast operands to correct type according to ISOC rules */
                //            if (is_float(bt1) || is_float(bt2)) {
                //                if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                //                    type.t = VT_LDOUBLE;

                //                } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                //                    type.t = VT_DOUBLE;
                //                } else {
                //                    type.t = VT_FLOAT;
                //                }
                //            } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                //                /* cast to biggest op */
                //                type.t = VT_LLONG | VT_LONG;
                //                if (bt1 == VT_LLONG)
                //                    type.t &= t1;
                //                if (bt2 == VT_LLONG)
                //                    type.t &= t2;
                //                /* convert to unsigned if it does not fit in a long long */
                //                if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
                //                    (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
                //                    type.t |= VT_UNSIGNED;
                //            } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
                //                /* If one is a null ptr constant the result type
                //                is the other.  */
                //                if (is_null_pointer (vtop))
                //                    type = type1;
                //                else if (is_null_pointer (&sv))
                //                    type = type2;
                //                /* XXX: test pointer compatibility, C99 has more elaborate
                //                rules here.  */
                //                else
                //                    type = type1;
                //            } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                //                /* XXX: test function pointer compatibility */
                //                type = bt1 == VT_FUNC ? type1 : type2;
                //            } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                //                /* XXX: test structure compatibility */
                //                type = bt1 == VT_STRUCT ? type1 : type2;
                //            } else if (bt1 == VT_VOID || bt2 == VT_VOID) {
                //                /* NOTE: as an extension, we accept void on only one side */
                //                type.t = VT_VOID;
                //            } else {
                //                /* integer operations */
                //                type.t = VT_INT | (VT_LONG & (t1 | t2));
                //                /* convert to unsigned if it does not fit in an integer */
                //                if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
                //                    (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
                //                    type.t |= VT_UNSIGNED;
                //            }
                //            /* keep structs lvalue by transforming `(expr ? a : b)` to `*(expr ? &a : &b)` so
                //            that `(expr ? a : b).mem` does not error  with "lvalue expected" */
                //            islv = (vtop->r & VT_LVAL) && (sv.r & VT_LVAL) && VT_STRUCT == (type.t & VT_BTYPE);
                //            islv &= c < 0;

                //            /* now we convert second operand */
                //            if (c != 1) {
                //                gen_cast(&type);
                //                if (islv) {
                //                    mk_pointer(&vtop->type);
                //                    gaddrof();
                //                } else if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                //                    gaddrof();
                //            }

                //            rc = RC_INT;
                //            if (is_float(type.t)) {
                //                rc = RC_FLOAT;
                //#ifdef TCC_TARGET_X86_64
                //                if ((type.t & VT_BTYPE) == VT_LDOUBLE) {
                //                    rc = RC_ST0;
                //                }
                //#endif
                //            } else if ((type.t & VT_BTYPE) == VT_LLONG) {
                //                /* for long longs, we use fixed registers to avoid having
                //                to handle a complicated move */
                //                rc = RC_IRET;
                //            }

                //            tt = r2 = 0;
                //            if (c < 0) {
                //                r2 = gv(rc);
                //                tt = gjmp(0);
                //            }
                //            gsym(u);

                //            /* this is horrible, but we must also convert first
                //            operand */
                //            if (c != 0) {
                //                *vtop = sv;
                //                gen_cast(&type);
                //                if (islv) {
                //                    mk_pointer(&vtop->type);
                //                    gaddrof();
                //                } else if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                //                    gaddrof();
                //            }

                //            if (c < 0) {
                //                r1 = gv(rc);
                //                move_reg(r2, r1, type.t);
                //                vtop->r = r2;
                //                gsym(tt);
                //                if (islv)
                //                    indir();
                //            }
                //        }
            }
        }

        public void expr_eq()
        {
            int t;

            expr_cond();
            if (prep.tok == '=' ||
                (prep.tok >= (int)TPTOKEN.TOK_A_MOD && prep.tok <= (int)TPTOKEN.TOK_A_DIV) ||
                prep.tok == (int)TPTOKEN.TOK_A_XOR || prep.tok == (int)TPTOKEN.TOK_A_OR ||
                prep.tok == (int)TPTOKEN.TOK_A_SHL || prep.tok == (int)TPTOKEN.TOK_A_SAR)
            {
                //test_lvalue();
                //t = prep.tok;
                //prep.next();
                //if (t == '=')
                //{
                //    expr_eq();
                //}
                //else
                //{
                //    vdup();
                //    expr_eq();
                //    gen_op(t & 0x7f);
                //}
                //vstore();
            }
        }

        public void gexpr()
        {
            while (true)
            {
                expr_eq();
                if (prep.tok != ',')
                    break;
                vpop();
                prep.next();
            }
        }

        public void expr_const1() { }
        public void expr_const64() { }
        public void expr_const() { }

        //- statements ----------------------------------------------------------------

        public void is_label() { }

        public void gfunc_return(CType func_type)
        {
            if ((func_type.t & VT_BTYPE) == VT_STRUCT)
            {
                //    CType type, ret_type;
                //    int ret_align, ret_nregs, regsize;
                //    ret_nregs = gfunc_sret(func_type, func_var, &ret_type,
                //        &ret_align, &regsize);
                //    if (0 == ret_nregs)
                //    {
                //        /* if returning structure, must copy it to implicit
                //        first pointer arg location */
                //        type = *func_type;
                //        mk_pointer(&type);
                //        vset(&type, VT_LOCAL | VT_LVAL, func_vc);
                //        indir();
                //        vswap();
                //        /* copy structure value to pointer */
                //        vstore();
                //    }
                //    else
                //    {
                //        /* returning structure packed into registers */
                //        int r, size, addr, align;
                //        size = type_size(func_type, &align);
                //        if ((vtop->r != (VT_LOCAL | VT_LVAL) ||
                //            (vtop->c.i & (ret_align - 1)))
                //            && (align & (ret_align - 1)))
                //        {
                //            loc = (loc - size) & -ret_align;
                //            addr = loc;
                //            type = *func_type;
                //            vset(&type, VT_LOCAL | VT_LVAL, addr);
                //            vswap();
                //            vstore();
                //            vpop();
                //            vset(&ret_type, VT_LOCAL | VT_LVAL, addr);
                //        }
                //        vtop->type = ret_type;
                //        if (is_float(ret_type.t))
                //            r = rc_fret(ret_type.t);
                //        else
                //            r = RC_IRET;

                //        if (ret_nregs == 1)
                //            gv(r);
                //        else
                //        {
                //            for (; ; )
                //            {
                //                vdup();
                //                gv(r);
                //                vpop();
                //                if (--ret_nregs == 0)
                //                    break;
                //                /* We assume that when a structure is returned in multiple
                //                registers, their classes are consecutive values of the
                //                suite s(n) = 2^n */
                //                r <<= 1;
                //                vtop->c.i += regsize;
                //            }
                //        }
                //    }
            }
            else if (is_float(func_type.t))
            {
                //    gv(rc_fret(func_type->t));
            }
            else
            {
                gv(Generator.RC_IRET);
            }
            vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        }

        public void case_cmp() { }
        public void gcase() { }

        public void block(ref int bsym, ref int csym, bool is_expr)
        {
            int a;
            int b;
            int c;
            int d;
            int cond;
            Sym s;

            /* generate line number info */
            //    if (tcc_state->do_debug)
            //        tcc_debug_line(tcc_state);

            if (is_expr)
            {
                //        /* default return value is (void) */
                //        vpushi(0);
                //        vtop->type.t = VT_VOID;
            }

            if (prep.tok == (int)TPTOKEN.TOK_IF)
            {
                //        /* if test */
                //        int saved_nocode_wanted = nocode_wanted;
                //        next();
                //        skip('(');
                //        gexpr();
                //        skip(')');
                //        cond = condition_3way();
                //        if (cond == 1)
                //            a = 0, vpop();
                //        else
                //            a = gvtst(1, 0);
                //        if (cond == 0)
                //            nocode_wanted |= 0x20000000;
                //        block(bsym, csym, 0);
                //        if (cond != 1)
                //            nocode_wanted = saved_nocode_wanted;
                //        c = tok;
                //        if (c == TOK_ELSE) {
                //            next();
                //            d = gjmp(0);
                //            gsym(a);
                //            if (cond == 1)
                //                nocode_wanted |= 0x20000000;
                //            block(bsym, csym, 0);
                //            gsym(d); /* patch else jmp */
                //            if (cond != 0)
                //                nocode_wanted = saved_nocode_wanted;
                //        } else
                //            gsym(a);
            }
            else if (prep.tok == (int)TPTOKEN.TOK_WHILE)
            {
                //        int saved_nocode_wanted;
                //        nocode_wanted &= ~0x20000000;
                //        next();
                //        d = ind;
                //        vla_sp_restore();
                //        skip('(');
                //        gexpr();
                //        skip(')');
                //        a = gvtst(1, 0);
                //        b = 0;
                //        ++local_scope;
                //        saved_nocode_wanted = nocode_wanted;
                //        block(&a, &b, 0);
                //        nocode_wanted = saved_nocode_wanted;
                //        --local_scope;
                //        gjmp_addr(d);
                //        gsym(a);
                //        gsym_addr(b, d);
            }
            else if (prep.tok == '{')
            {
                Sym llabel;
                //        int block_vla_sp_loc = vla_sp_loc, saved_vlas_in_scope = vlas_in_scope;

                prep.next();
                /* record local declaration stack position */
                //        s = local_stack;
                //        llabel = local_label_stack;
                //        ++local_scope;

                /* handle local labels declarations */
                //        if (tok == TOK_LABEL) {
                //            next();
                //            for(;;) {
                //                if (tok < TOK_UIDENT)
                //                    expect("label identifier");
                //                label_push(&local_label_stack, tok, LABEL_DECLARED);
                //                next();
                //                if (tok == ',') {
                //                    next();
                //                } else {
                //                    skip(';');
                //                    break;
                //                }
                //            }
                //        }
                while (prep.tok != '}')
                {
                    //            if ((a = is_label()))
                    //                unget_tok(a);
                    //            else
                    //                decl(VT_LOCAL);
                    if (prep.tok != '}')
                    {
                        //                if (is_expr)
                        //                    vpop();
                        block(ref bsym, ref csym, is_expr);
                    }
                }
                /* pop locally defined labels */
                //        label_pop(&local_label_stack, llabel, is_expr);
                /* pop locally defined symbols */
                //        --local_scope;
                /* In the is_expr case (a statement expression is finished here),
                vtop might refer to symbols on the local_stack.  Either via the
                type or via vtop->sym.  We can't pop those nor any that in turn
                might be referred to.  To make it easier we don't roll back
                any symbols in that case; some upper level call to block() will
                do that.  We do have to remove such symbols from the lookup
                tables, though.  sym_pop will do that.  */
                //        sym_pop(&local_stack, s, is_expr);

                //        /* Pop VLA frames and restore stack pointer if required */
                //        if (vlas_in_scope > saved_vlas_in_scope) {
                //            vla_sp_loc = saved_vlas_in_scope ? block_vla_sp_loc : vla_sp_root_loc;
                //            vla_sp_restore();
                //        }
                //        vlas_in_scope = saved_vlas_in_scope;

                prep.next();
            }
            else if (prep.tok == (int)TPTOKEN.TOK_RETURN)
            {
                prep.next();
                if (prep.tok != ';')
                {
                    gexpr();
                    gen_assign_cast(func_vt);
                    if ((func_vt.t & VT_BTYPE) == VT_VOID)
                        vtop--;
                    else
                        gfunc_return(func_vt);
                }
                prep.skip(';');
                /* jump unless last stmt in top-level block */
                //        if (tok != '}' || local_scope != 1)
                //            rsym = gjmp(rsym);
                //                    nocode_wanted |= 0x20000000;
            }
            else if (prep.tok == (int)TPTOKEN.TOK_BREAK)
            {
                //        /* compute jump */
                //        if (!bsym)
                //            tcc_error("cannot break");
                //        *bsym = gjmp(*bsym);
                //        next();
                //        skip(';');
                //        nocode_wanted |= 0x20000000;
            }
            else if (prep.tok == (int)TPTOKEN.TOK_CONTINUE)
            {
                //        /* compute jump */
                //        if (!csym)
                //            tcc_error("cannot continue");
                //        vla_sp_restore_root();
                //        *csym = gjmp(*csym);
                //        next();
                //        skip(';');
            }
            else if (prep.tok == (int)TPTOKEN.TOK_FOR)
            {
                //        int e;
                //        int saved_nocode_wanted;
                //        nocode_wanted &= ~0x20000000;
                //        next();
                //        skip('(');
                //        s = local_stack;
                //        ++local_scope;
                //        if (tok != ';') {
                //            /* c99 for-loop init decl? */
                //            if (!decl0(VT_LOCAL, 1, NULL)) {
                //                /* no, regular for-loop init expr */
                //                gexpr();
                //                vpop();
                //            }
                //        }
                //        skip(';');
                //        d = ind;
                //        c = ind;
                //        vla_sp_restore();
                //        a = 0;
                //        b = 0;
                //        if (tok != ';') {
                //            gexpr();
                //            a = gvtst(1, 0);
                //        }
                //        skip(';');
                //        if (tok != ')') {
                //            e = gjmp(0);
                //            c = ind;
                //            vla_sp_restore();
                //            gexpr();
                //            vpop();
                //            gjmp_addr(d);
                //            gsym(e);
                //        }
                //        skip(')');
                //        saved_nocode_wanted = nocode_wanted;
                //        block(&a, &b, 0);
                //        nocode_wanted = saved_nocode_wanted;
                //        gjmp_addr(c);
                //        gsym(a);
                //        gsym_addr(b, c);
                //        --local_scope;
                //        sym_pop(&local_stack, s, 0);

            }
            else
                if (prep.tok == (int)TPTOKEN.TOK_DO)
                {
                    //            int saved_nocode_wanted;
                    //            nocode_wanted &= ~0x20000000;
                    //            next();
                    //            a = 0;
                    //            b = 0;
                    //            d = ind;
                    //            vla_sp_restore();
                    //            saved_nocode_wanted = nocode_wanted;
                    //            block(&a, &b, 0);
                    //            skip(TOK_WHILE);
                    //            skip('(');
                    //            gsym(b);
                    //            gexpr();
                    //            c = gvtst(0, 0);
                    //            gsym_addr(c, d);
                    //            nocode_wanted = saved_nocode_wanted;
                    //            skip(')');
                    //            gsym(a);
                    //            skip(';');
                }
                else
                    if (prep.tok == (int)TPTOKEN.TOK_SWITCH)
                    {
                        //                struct switch_t *saved, sw;
                        //                int saved_nocode_wanted = nocode_wanted;
                        //                SValue switchval;
                        //                next();
                        //                skip('(');
                        //                gexpr();
                        //                skip(')');
                        //                switchval = *vtop--;
                        //                a = 0;
                        //                b = gjmp(0); /* jump to first case */
                        //                sw.p = NULL; sw.n = 0; sw.def_sym = 0;
                        //                saved = cur_switch;
                        //                cur_switch = &sw;
                        //                block(&a, csym, 0);
                        //                nocode_wanted = saved_nocode_wanted;
                        //                a = gjmp(a); /* add implicit break */
                        //                /* case lookup */
                        //                gsym(b);
                        //                qsort(sw.p, sw.n, sizeof(void*), case_cmp);
                        //                for (b = 1; b < sw.n; b++)
                        //                    if (sw.p[b - 1]->v2 >= sw.p[b]->v1)
                        //                        tcc_error("duplicate case value");
                        //                /* Our switch table sorting is signed, so the compared
                        //                value needs to be as well when it's 64bit.  */
                        //                if ((switchval.type.t & VT_BTYPE) == VT_LLONG)
                        //                    switchval.type.t &= ~VT_UNSIGNED;
                        //                vpushv(&switchval);
                        //                gcase(sw.p, sw.n, &a);
                        //                vpop();
                        //                if (sw.def_sym)
                        //                    gjmp_addr(sw.def_sym);
                        //                dynarray_reset(&sw.p, &sw.n);
                        //                cur_switch = saved;
                        //                /* break label */
                        //                gsym(a);
                    }
                    else
                        if (prep.tok == (int)TPTOKEN.TOK_CASE)
                        {
                            //                    struct case_t *cr = tcc_malloc(sizeof(struct case_t));
                            //                    if (!cur_switch)
                            //                        expect("switch");
                            //                    nocode_wanted &= ~0x20000000;
                            //                    next();
                            //                    cr->v1 = cr->v2 = expr_const64();
                            //                    if (gnu_ext && tok == TOK_DOTS) {
                            //                        next();
                            //                        cr->v2 = expr_const64();
                            //                        if (cr->v2 < cr->v1)
                            //                            tcc_warning("empty case range");
                            //                    }
                            //                    cr->sym = ind;
                            //                    dynarray_add(&cur_switch->p, &cur_switch->n, cr);
                            //                    skip(':');
                            //                    is_expr = 0;
                            //                    goto block_after_label;
                        }
                        else
                            if (prep.tok == (int)TPTOKEN.TOK_DEFAULT)
                            {
                                //                        next();
                                //                        skip(':');
                                //                        if (!cur_switch)
                                //                            expect("switch");
                                //                        if (cur_switch->def_sym)
                                //                            tcc_error("too many 'default'");
                                //                        cur_switch->def_sym = ind;
                                //                        is_expr = 0;
                                //                        goto block_after_label;
                            }
                            else
                                if (prep.tok == (int)TPTOKEN.TOK_GOTO)
                                {
                                    //                            next();
                                    //                            if (tok == '*' && gnu_ext) {
                                    //                                /* computed goto */
                                    //                                next();
                                    //                                gexpr();
                                    //                                if ((vtop->type.t & VT_BTYPE) != VT_PTR)
                                    //                                    expect("pointer");
                                    //                                ggoto();
                                    //                            } else if (tok >= TOK_UIDENT) {
                                    //                                s = label_find(tok);
                                    //                                /* put forward definition if needed */
                                    //                                if (!s) {
                                    //                                    s = label_push(&global_label_stack, tok, LABEL_FORWARD);
                                    //                                } else {
                                    //                                    if (s->r == LABEL_DECLARED)
                                    //                                        s->r = LABEL_FORWARD;
                                    //                                }
                                    //                                vla_sp_restore_root();
                                    //                                if (s->r & LABEL_FORWARD)
                                    //                                    s->jnext = gjmp(s->jnext);
                                    //                                else
                                    //                                    gjmp_addr(s->jnext);
                                    //                                next();
                                    //                            } else {
                                    //                                expect("label identifier");
                                    //                            }
                                    //                            skip(';');
                                }
                                else if (prep.tok == (int)TPTOKEN.TOK_ASM1 || prep.tok == (int)TPTOKEN.TOK_ASM2
                                    || prep.tok == (int)TPTOKEN.TOK_ASM3)
                                {
                                    //                            asm_instr();
                                }
                                else
                                {
                                    //                            b = is_label();
                                    //                            if (b) {
                                    //                                /* label case */
                                    //                                next();
                                    //                                s = label_find(b);
                                    //                                if (s) {
                                    //                                    if (s->r == LABEL_DEFINED)
                                    //                                        tcc_error("duplicate label '%s'", get_tok_str(s->v, NULL));
                                    //                                    gsym(s->jnext);
                                    //                                    s->r = LABEL_DEFINED;
                                    //                                } else {
                                    //                                    s = label_push(&global_label_stack, b, LABEL_DEFINED);
                                    //                                }
                                    //                                s->jnext = ind;
                                    //                                vla_sp_restore();
                                    /* we accept this, but it is a mistake */
                                    //block_after_label:
                                    //                                nocode_wanted &= ~0x20000000;
                                    //                                if (tok == '}') {
                                    //                                    tcc_warning("deprecated use of label at end of compound statement");
                                    //                                } else {
                                    //                                    if (is_expr)
                                    //                                        vpop();
                                    //                                    block(bsym, csym, is_expr);
                                    //                                }
                                    //                            } else {
                                    //                                /* expression case */
                                    //                                if (tok != ';') {
                                    //                                    if (is_expr) {
                                    //                                        vpop();
                                    //                                        gexpr();
                                    //                                    } else {
                                    //                                        gexpr();
                                    //                                        vpop();
                                    //                                    }
                                    //                                }
                                    //                                skip(';');
                                    //                            }
                                }
        }

        public void skip_or_save_block() { }
        public void parse_init_elem() { }
        public void init_putz() { }
        public void decl_designator() { }
        public void init_putv() { }
        public void decl_initializer() { }
        public void decl_initializer_alloc() { }

        //- functions ---------------------------------------------------------

        public void gen_function(Sym sym)
        {
            nocode_wanted = 0;
            ind = Section.curTextSection.data_offset;

            /* NOTE: we patch the symbol size later */
            put_extern_sym(sym, Section.curTextSection, ind, 0);
            funcname = prep.get_tok_str(sym.v, null);
            func_ind = ind;

            /* Initialize VLA state */
            //    vla_sp_loc = -1;
            //    vla_sp_root_loc = -1;

            /* put debug symbol */
            //    tcc_debug_funcstart(tcc_state, sym);

            /* push a dummy symbol to enable local sym storage */
            //    sym_push2(&local_stack, SYM_FIELD, 0, 0);
            local_scope = 1;                    /* for function parameters */
            gen.gfunc_prolog(sym.type);
            local_scope = 0;
            rsym = 0;
            int bsym = 0;       //dummy init vals
            int csym = 0;
            block(ref bsym, ref csym, false);
            nocode_wanted = 0;
            gen.gsym(rsym);
            gen.gfunc_epilog();
            Section.curTextSection.data_offset = ind;
            //    label_pop(&global_label_stack, NULL, 0);

            /* reset local stack */
            local_scope = 0;
            //    sym_pop(&local_stack, NULL, 0);

            /* end of function */
            //    tcc_debug_funcend(tcc_state, ind - func_ind);

            /* patch symbol size */
            elfsym(sym).setSize(ind - func_ind);

            /* It's better to crash than to generate wrong code */
            Section.curTextSection = null;
            funcname = "";                          /* for safety */
            func_vt.t = VT_VOID;                    /* for safety */
            func_var = false;                       /* for safety */
            ind = 0;                                /* for safety */
            nocode_wanted = 0x80000000;
            //    check_vstack();
        }

        public void gen_inline_functions() { }
        public void free_inline_functions() { }

        //- external declarations ---------------------------------------------

        public int decl0(int l, bool is_for_loop_init, Sym func_sym)
        {
            int v = 0;
            int has_init;
            int r;

            CType type = new CType();
            CType btype = new CType();
            Sym sym = null;
            AttributeDef ad = new AttributeDef();

            /*external-declaration:
                function-definition
                declaration
             */
            while (true)
            {
                if (!parse_btype(btype, ad))        //parse declaration-specifiers ("base type")
                {
                    if (is_for_loop_init)
                        return 0;

                    /* skip redundant ';' if not in old parameter decl scope */
                    if (prep.tok == ';' && l != VT_CMP)
                    {
                        prep.next();
                        continue;
                    }
                    if (l != VT_CONST)
                        break;

                    if (prep.tok == (int)TPTOKEN.TOK_ASM1 || prep.tok == (int)TPTOKEN.TOK_ASM2 || prep.tok == (int)TPTOKEN.TOK_ASM3)
                    {
                        /* global asm block */
                        //asm_global_instr();
                        continue;
                    }

                    if (prep.tok >= (int)TPTOKEN.TOK_UIDENT)
                    {
                        /* special test for old K&R protos without explicit int
                           type. Only accepted when defining global data */
                        btype.t = VT_INT;
                    }
                    else
                    {
                        if (prep.tok != (int)TPTOKEN.TOK_EOF)
                            prep.expect("declaration");
                        break;
                    }
                }

                //if we have a anon type 
                if (prep.tok == ';')
                {
                    //	    if ((btype.t & VT_BTYPE) == VT_STRUCT) {
                    //		int v = btype.ref->v;
                    //		if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) >= SYM_FIRST_ANOM)
                    //        	    tcc_warning("unnamed struct/union that defines no instances");
                    //                next();
                    //                continue;
                    //	    }
                    //            if (IS_ENUM(btype.t)) {
                    //                next();
                    //                continue;
                    //            }
                }

                //base type is followed by either a function def or a list of declarators
                while (true)
                { /* iterate thru each declaration */
                    type = btype;
                    /* If the base type itself was an array type of unspecified
                       size (like in 'typedef int arr[]; arr x = {1};') then
                       we will overwrite the unknown size by the real one for
                       this decl.  We need to unshare the ref symbol holding that size.  */
                    //	    if ((type.t & VT_ARRAY) && type.ref->c < 0) {
                    //		type.ref = sym_push(SYM_FIELD, &type.ref->type, 0, type.ref->c);
                    //	    }

                    type_decl(type, ad, ref v, TYPE_DIRECT);

                    if ((type.t & VT_BTYPE) == VT_FUNC)
                    {
                        //                if ((type.t & VT_STATIC) && (l == VT_LOCAL)) {
                        //                    tcc_error("function without file scope cannot be static");
                        //                }
                        /* if old style function prototype, we accept a declaration list */
                        sym = type.reff;
                        if (sym.f.func_type == FuncAttr.FUNCTYPE.FUNC_OLD && l == VT_CONST)
                            decl0(VT_CMP, false, sym);
                    }

                    //            if (gnu_ext && (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3)) {
                    //                ad.asm_label = asm_label_instr();
                    //                /* parse one last attribute list, after asm label */
                    //                parse_attribute(&ad);
                    //                if (tok == '{')
                    //                    expect(";");
                    //            }

                    //#ifdef TCC_TARGET_PE
                    //            if (ad.a.dllimport || ad.a.dllexport) {
                    //                if (type.t & (VT_STATIC|VT_TYPEDEF))
                    //                    tcc_error("cannot have dll linkage with static or typedef");
                    //                if (ad.a.dllimport) {
                    //                    if ((type.t & VT_BTYPE) == VT_FUNC)
                    //                        ad.a.dllimport = 0;
                    //                    else
                    //                        type.t |= VT_EXTERN;
                    //                }
                    //            }
                    //#endif

                    //parse func defintion
                    if (prep.tok == '{')
                    {
                        //                if (l != VT_CONST)
                        //                    tcc_error("cannot use local functions");
                        //                if ((type.t & VT_BTYPE) != VT_FUNC)
                        //                    expect("function definition");

                        /* reject abstract declarators in function definition make old style params without decl have int type */
                        //                sym = type.ref;
                        //                while ((sym = sym->next) != NULL) {
                        //                    if (!(sym->v & ~SYM_FIELD))
                        //                        expect("identifier");
                        //		    if (sym->type.t == VT_VOID)
                        //		        sym->type = int_type;
                        //		}

                        /* XXX: cannot do better now: convert extern line to static inline */
                        //                if ((type.t & (VT_EXTERN | VT_INLINE)) == (VT_EXTERN | VT_INLINE))
                        //                    type.t = (type.t & ~VT_EXTERN) | VT_STATIC;

                        /* put function symbol */
                        sym = external_global_sym(v, type, 0);
                        type.t &= ~VT_EXTERN;
                        patch_storage(sym, ad, type);

                        /* static inline functions are just recorded as a kind
                           of macro. Their code will be emitted at the end of
                           the compilation unit only if they are used */
                        if ((type.t & (VT_INLINE | VT_STATIC)) == (VT_INLINE | VT_STATIC))
                        {
                            //                    struct InlineFunc *fn;
                            //                    const char *filename;
                            //                           
                            //                    filename = file ? file->filename : "";
                            //                    fn = tcc_malloc(sizeof *fn + strlen(filename));
                            //                    strcpy(fn->filename, filename);
                            //                    fn->sym = sym;
                            //		    skip_or_save_block(&fn->func_str);
                            //                    dynarray_add(&tcc_state->inline_fns,
                            //				 &tcc_state->nb_inline_fns, fn);
                        }
                        else
                        {
                            /* compute text section */
                            Section.curTextSection = ad.section;
                            if (Section.curTextSection == null)
                                Section.curTextSection = Section.textSection;
                            gen_function(sym);
                        }
                        break;
                    }
                    else
                    {
                        //		if (l == VT_CMP) {
                        //		    /* find parameter in function parameter list */
                        //		    for (sym = func_sym->next; sym; sym = sym->next)
                        //			if ((sym->v & ~SYM_FIELD) == v)
                        //			    goto found;
                        //		    tcc_error("declaration for parameter '%s' but no such parameter",
                        //			      get_tok_str(v, NULL));
                        //found:
                        //		    if (type.t & VT_STORAGE) /* 'register' is okay */
                        //		        tcc_error("storage class specified for '%s'",
                        //				  get_tok_str(v, NULL));
                        //		    if (sym->type.t != VT_VOID)
                        //		        tcc_error("redefinition of parameter '%s'",
                        //				  get_tok_str(v, NULL));
                        //		    convert_parameter_type(&type);
                        //		    sym->type = type;
                        //		} else if (type.t & VT_TYPEDEF) {
                        //                    /* save typedefed type  */
                        //                    /* XXX: test storage specifiers ? */
                        //                    sym = sym_find(v);
                        //                    if (sym && sym->sym_scope == local_scope) {
                        //                        if (!is_compatible_types(&sym->type, &type)
                        //                            || !(sym->type.t & VT_TYPEDEF))
                        //                            tcc_error("incompatible redefinition of '%s'",
                        //                                get_tok_str(v, NULL));
                        //                        sym->type = type;
                        //                    } else {
                        //                        sym = sym_push(v, &type, 0, 0);
                        //                    }
                        //                    sym->a = ad.a;
                        //                    sym->f = ad.f;
                        //                } else {
                        //                    r = 0;
                        //                    if ((type.t & VT_BTYPE) == VT_FUNC) {
                        //                        /* external function definition */
                        //                        /* specific case for func_call attribute */
                        //                        type.ref->f = ad.f;
                        //                    } else if (!(type.t & VT_ARRAY)) {
                        //                        /* not lvalue if array */
                        //                        r |= lvalue_type(type.t);
                        //                    }
                        //                    has_init = (tok == '=');
                        //                    if (has_init && (type.t & VT_VLA))
                        //                        tcc_error("variable length array cannot be initialized");
                        //                    if (((type.t & VT_EXTERN) && (!has_init || l != VT_CONST)) ||
                        //			((type.t & VT_BTYPE) == VT_FUNC) ||
                        //                        ((type.t & VT_ARRAY) && (type.t & VT_STATIC) &&
                        //                         !has_init && l == VT_CONST && type.ref->c < 0)) {
                        //                        /* external variable or function */
                        //                        /* NOTE: as GCC, uninitialized global static
                        //                           arrays of null size are considered as
                        //                           extern */
                        //                        type.t |= VT_EXTERN;
                        //                        sym = external_sym(v, &type, r, &ad);
                        //                        if (ad.alias_target) {
                        //                            ElfSym *esym;
                        //                            Sym *alias_target;
                        //                            alias_target = sym_find(ad.alias_target);
                        //                            esym = elfsym(alias_target);
                        //                            if (!esym)
                        //                                tcc_error("unsupported forward __alias__ attribute");
                        //                            /* Local statics have a scope until now (for
                        //                               warnings), remove it here.  */
                        //                            sym->sym_scope = 0;
                        //                            put_extern_sym2(sym, esym->st_shndx, esym->st_value, esym->st_size, 0);
                        //                        }
                        //                    } else {
                        //                        if (type.t & VT_STATIC)
                        //                            r |= VT_CONST;
                        //                        else
                        //                            r |= l;
                        //                        if (has_init)
                        //                            next();
                        //                        else if (l == VT_CONST)
                        //                            /* uninitialized global variables may be overridden */
                        //                            type.t |= VT_EXTERN;
                        //                        decl_initializer_alloc(&type, &ad, r, has_init, v, l);
                        //                    }
                        //                }
                        if (prep.tok != ',')
                        {
                            if (is_for_loop_init)
                                return 1;
                            prep.skip(';');
                            break;
                        }
                        prep.next();
                    }
                    ad.a.aligned = 0;
                }
            }

            return 0;
        }

        public void decl(int l)
        {
            decl0(l, false, null);
        }
    }

    //-------------------------------------------------------------------------

    public class CType
    {
        public int t;
        public Sym reff;
    }

    //-------------------------------------------------------------------------

    public class SValue 	/* value on stack */
    {
        public CType type;    /* type */
        public int r;         /* register + flags */
        public int r2;        /* second register, used for 'long long' type. If not used, set to VT_CONST */
        public CValue c;      /* constant, if VT_CONST */
        public Sym sym;       /* symbol, if (VT_SYM | VT_CONST), or if result of unary() for an identifier. */
    }

    //-------------------------------------------------------------------------

    public class SymAttr
    {
        public short aligned; /* alignment as log2+1 (0 == unspecified) */
        public short packed;
        public short weak;
        public short visibility;
        public short dllexport;
        public short dllimport;
        public short unused;
    }

    //-----------------------------------------------------------------------------

    public class FuncAttr
    {
        /* stored in 'Sym->f.func_type' field */
        public enum FUNCTYPE
        {
            FUNC_NONE = 0,
            FUNC_NEW = 1, /* ansi function prototype */
            FUNC_OLD = 2, /* old function prototype */
            FUNC_ELLIPSIS = 3 /* ansi function prototype with ... */
        }

        /* stored in 'Sym->f.func_call' field */
        public enum FUNCCALL
        {
            FUNC_CDECL = 0, /* standard c call */
            FUNC_STDCALL = 1, /* pascal c call */
            FUNC_FASTCALL1 = 2, /* first param in %eax */
            FUNC_FASTCALL2 = 3, /* first parameters in %eax, %edx */
            FUNC_FASTCALL3 = 4, /* first parameter in %eax, %edx, %ecx */
            FUNC_FASTCALLW = 5 /* first parameter in %ecx, %edx */
        }

        public FUNCCALL func_call; /* calling convention (0..5), see below */
        public FUNCTYPE func_type; /* FUNC_OLD/NEW/ELLIPSIS */
        public int func_args; /* PE __stdcall args */
    }

    //-------------------------------------------------------------------------

    /* GNUC attribute definition */
    public class AttributeDef
    {
        public SymAttr a;
        public FuncAttr f;
        public Section section;
        public int alias_target; /* token */
        public int asm_label; /* associated asm label */
        public char attr_mode; /* __attribute__((__mode__(...))) */

        public AttributeDef()
        {
            reset();
        }

        public void reset()
        {
            a = new SymAttr();
            f = new FuncAttr();
            section = null;
            alias_target = 0;
            asm_label = 0;
            attr_mode = '\0';
        }
    }

    //-------------------------------------------------------------------------

    public class Sym
    {
        public int v;                  /* symbol token */
        public short r;                /* associated register or VT_CONST/VT_LOCAL and LVAL type */
        public SymAttr a;              /* symbol attributes */
        public int c;                  /* associated number or Elf symbol index */
        public int sym_scope;          /* scope level for locals */
        public int jnext;              /* next jump label */
        public FuncAttr f;             /* function attributes */
        public int auxtype;            /* bitfield access type */
        public long enum_val;          /* enum constant if IS_ENUM_VAL */
        public int d;                  /* define token stream */
        public CType type;             /* associated type */
        public Sym next;               /* next related symbol (for fields and anoms) */
        public int asm_label;          /* associated asm label */
        public Sym prev;               /* prev symbol in stack */
        public Sym prev_tok;           /* previous symbol for this token */

        public Sym()
        {
            v = 0;
            r = 0;
            a = new SymAttr();
            c = 0;
            sym_scope = 0;
            jnext = 0;
            f = new FuncAttr();
            auxtype = 0;
            enum_val = 0;
            d = 0;
            type = new CType();
            next = null;
            asm_label = 0;
            prev = null;
            prev_tok = null;
        }
    }
}
