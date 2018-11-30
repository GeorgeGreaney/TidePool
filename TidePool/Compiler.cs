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

        public const int LONG_SIZE = 4;

        public const int SYM_STRUCT = 0x40000000;         /* struct/union/enum symbol space */
        public const int SYM_FIELD = 0x20000000;          /* struct/union field symbol space */
        public const int SYM_FIRST_ANOM = 0x10000000;     /* first anonymous sym */

        /* stored in 'Sym->f.func_type' field */
        public const int FUNC_NEW = 1;                    /* ansi function prototype */
        public const int FUNC_OLD = 2;                    /* old function prototype */
        public const int FUNC_ELLIPSIS = 3;               /* ansi function prototype with ... */

        /* type_decl() types */
        public const int TYPE_ABSTRACT = 1; /* type without variable */
        public const int TYPE_DIRECT = 2; /* type with variable */

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

        public List<Sym> global_stack;
        public List<Sym> local_stack;
        public List<Sym> define_stack;
        public List<Sym> global_label_stack;
        public List<Sym> local_label_stack;
        public int local_scope;
        public int in_sizeof;
        public int section_sym;

        public Compiler(TidePool _tp)
        {
            tp = _tp;

            global_stack = new List<Sym>();
            local_stack = null;
        }

        public void is_float() { }
        public void ieee_finite() { }
        public void test_lvalue() { }
        public void check_vstack() { }
        public void tp_debug_start() { }
        public void tp_debug_end() { }
        public void tp_debug_line() { }
        public void tp_debug_funcstart() { }
        public void tp_debug_funcend() { }

        public void tp_compile()
        {
            prep = tp.prep;

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

            //    tcc_debug_start(s1);

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
            //    return 0;

        }

        public void elfsym() { }

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
        public void put_extern_sym2() { }
        public void put_extern_sym() { }
        public void greloca() { }
        public void greloc() { }

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
        public void vsetc() { }
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
        public void get_reg() { }
        public void move_reg() { }
        public void gaddrof() { }
        public void gbound() { }
        public void incr_bf_adr() { }
        public void load_packed_bf() { }
        public void store_packed_bf() { }
        public void adjust_bf() { }
        public void gv() { }
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
        public void gen_op() { }
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

        public bool compare_types(CType type1, CType type2, int unqualified)
        {
            return true;
        }

        public bool is_compatible_types(CType type1, CType type2)
        {
            return compare_types(type1, type2, 0);
        }

        public void is_compatible_unqualified_types() { }
        public void type_to_str() { }
        public void gen_assign_cast() { }
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
            int l = 0;
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
                    l = 0;								//empty param list
                //else if (parse_btype(&pt, &ad1))
                //  l = FUNC_NEW;
                //else if (td)
                //  return 0;
                //else
                //  l = FUNC_OLD;

                first = null;
                //plast = &first;
                arg_size = 0;
                if (l > 0)
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
                    l = FUNC_OLD;

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
        public void unary() { }
        public void expr_prod() { }
        public void expr_sum() { }
        public void expr_shift() { }
        public void expr_cmp() { }
        public void expr_cmpeq() { }
        public void expr_and() { }
        public void expr_xor() { }
        public void expr_or() { }
        public void expr_land() { }
        public void expr_lor() { }
        public void condition_3way() { }
        public void expr_cond() { }
        public void expr_eq() { }
        public void gexpr() { }
        public void expr_const1() { }
        public void expr_const64() { }
        public void expr_const() { }
        public void is_label() { }
        public void gfunc_return() { }
        public void case_cmp() { }
        public void gcase() { }
        public void block() { }
        public void skip_or_save_block() { }
        public void parse_init_elem() { }
        public void init_putz() { }
        public void decl_designator() { }
        public void init_putv() { }
        public void decl_initializer() { }
        public void decl_initializer_alloc() { }

        public void gen_function(Sym sym)
        {
            //    nocode_wanted = 0;
            //    ind = curTextSection->data_offset;
            
            /* NOTE: we patch the symbol size later */
            //    put_extern_sym(sym, curTextSection, ind, 0);
            //    funcname = get_tok_str(sym->v, NULL);
            //    func_ind = ind;
            
            /* Initialize VLA state */
            //    vla_sp_loc = -1;
            //    vla_sp_root_loc = -1;
            
            /* put debug symbol */
            //    tcc_debug_funcstart(tcc_state, sym);
            
            /* push a dummy symbol to enable local sym storage */
            //    sym_push2(&local_stack, SYM_FIELD, 0, 0);
            //    local_scope = 1; /* for function parameters */
            //    gfunc_prolog(&sym->type);
            //    local_scope = 0;
            //    rsym = 0;
            //    block(NULL, NULL, 0);
            //    nocode_wanted = 0;
            //    gsym(rsym);
            //    gfunc_epilog();
            //    curTextSection->data_offset = ind;
            //    label_pop(&global_label_stack, NULL, 0);
            
            /* reset local stack */
            //    local_scope = 0;
            //    sym_pop(&local_stack, NULL, 0);
            
            /* end of function */            
            /* patch symbol size */
            //    elfsym(sym)->st_size = ind - func_ind;
            //    tcc_debug_funcend(tcc_state, ind - func_ind);

            /* It's better to crash than to generate wrong code */
            Section.curTextSection = null;
            //    funcname = ""; /* for safety */
            //    func_vt.t = VT_VOID; /* for safety */
            //    func_var = 0; /* for safety */
            //    ind = 0; /* for safety */
            //    nocode_wanted = 0x80000000;
            //    check_vstack();
        }

        public void gen_inline_functions() { }
        public void free_inline_functions() { }

        public int decl0(int l, bool is_for_loop_init, Sym func_sym)
        {
            int v = 0;
            int has_init;
            int r;

            CType type = new CType();
            CType btype = new CType();
            Sym sym = null;
            AttributeDef ad = new AttributeDef();

            while (true)
            {
                if (!parse_btype(btype, ad))
                {
                    if (is_for_loop_init)
                        return 0;

                                /* skip redundant ';' if not in old parameter decl scope */
                                if (prep.tok == ';' && l != VT_CMP) {
                                    prep.next();
                                    continue;
                                }
                                if (l != VT_CONST)
                                    break;

                    //            if (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3) {
                    //                /* global asm block */
                    //                asm_global_instr();
                    //                continue;
                    //            }
                    //            if (tok >= TOK_UIDENT) {
                    //               /* special test for old K&R protos without explicit int
                    //                  type. Only accepted when defining global data */
                    //                btype.t = VT_INT;
                    //            } else {
                    //                if (tok != TOK_EOF)
                    //                    expect("declaration");
                    //                break;
                    //            }
                }

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

                while (true)
                { /* iterate thru each declaration */
                    type = btype;
                    /* If the base type itself was an array type of unspecified
                       size (like in 'typedef int arr[]; arr x = {1};') then
                       we will overwrite the unknown size by the real one for
                       this decl.  We need to unshare the ref symbol holding
                       that size.  */
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
                        if (sym.f.func_type == FUNC_OLD && l == VT_CONST)
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
                        //                if (tok != ',') {
                        //                    if (is_for_loop_init)
                        //                        return 1;
                        //                    skip(';');
                        //                    break;
                        //                }
                        //                next();
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
        public int func_call; /* calling convention (0..5), see below */
        public int func_type; /* FUNC_OLD/NEW/ELLIPSIS */
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
