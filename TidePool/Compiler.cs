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

        public const int VT_VALMASK = 0x003f;  /* mask for value location, register or: */
        public const int VT_CONST = 0x0030;  /* constant in vc (must be first non register value) */
        public const int VT_LLOCAL = 0x0031;  /* lvalue, offset on stack */
        public const int VT_LOCAL = 0x0032;  /* offset on stack */
        public const int VT_CMP = 0x0033;  /* the value is stored in processor flags (in vc) */
        public const int VT_JMP = 0x0034;  /* value is the consequence of jmp true (even) */
        public const int VT_JMPI = 0x0035;  /* value is the consequence of jmp false (odd) */
        public const int VT_LVAL = 0x0100;  /* var is an lvalue */
        public const int VT_SYM = 0x0200;  /* a symbol value is added */
        public const int VT_MUSTCAST = 0x0400;  /* value must be casted to be correct (used for char/short stored in integer registers) */
        public const int VT_MUSTBOUND = 0x0800;  /* bound checking must be done before dereferencing value */
        public const int VT_BOUNDED = 0x8000; /* value is bounded. The address of the bounding function call point is in vc */
        public const int VT_LVAL_BYTE = 0x1000;  /* lvalue is a byte */
        public const int VT_LVAL_SHORT = 0x2000;  /* lvalue is a short */
        public const int VT_LVAL_UNSIGNED = 0x4000;  /* lvalue is unsigned */
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

        public Compiler(TidePool _tp)
        {
            tp = _tp;
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
            //            cur_text_section = NULL;
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
            //    /* end of translation unit info */
            //    tcc_debug_end(s1);
            //    return 0;

        }

        public void elfsym() { }
        public void update_storage() { }
        public void put_extern_sym2() { }
        public void put_extern_sym() { }
        public void greloca() { }
        public void greloc() { }
        public void __sym_malloc() { }
        public void sym_malloc() { }
        public void sym_free() { }
        public void sym_push2() { }
        public void sym_find2() { }
        public void struct_find() { }

        public Sym sym_find(int v)
        {
            return null;
        }

        public void sym_push() { }
        public void global_identifier_push() { }
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
        public void external_global_sym() { }
        public void patch_type() { }
        public void patch_storage() { }
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
        public void compare_types() { }
        public void is_compatible_types() { }
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

        public void struct_decl(ref CType type, int u)
        {
        }

        public void sym_to_attr(AttributeDef ad, Sym s)
        {
        }

        public void parse_btype_qualify(CType type, int qualifiers)
        {
        }

        public bool parse_btype(ref CType type, ref AttributeDef ad)
        {
            int u = 0;
            int bt;
            int st;
            int g;
            Sym s;
            CType type1 = null;

            ad = new AttributeDef();
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

                    case TPTOKEN.TOK_ENUM:
                        struct_decl(ref type1, VT_ENUM);
                    basic_type2:
                        u = type1.t;
                        type.reff = type1.reff;
                        goto basic_type1;

                    case TPTOKEN.TOK_STRUCT:
                        struct_decl(ref type1, VT_STRUCT);
                        goto basic_type2;

                    case TPTOKEN.TOK_UNION:
                        struct_decl(ref type1, VT_UNION);
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
                        parse_expr_type(ref type1);
                        /* remove all storage modifiers except typedef */
                        type1.t &= ~(VT_STORAGE & ~VT_TYPEDEF);
                        if (type1.reff != null)
                            sym_to_attr(ad, type1.reff);
                        goto basic_type2;

                    default:
                        if (typespec_found)
                            goto the_end;

                        s = sym_find(prep.tok);
                        if (s != null || !((s.type.t & VT_TYPEDEF) != 0))
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
                if (tp.char_is_unsigned) {
                    if ((t & (VT_DEFSIGN|VT_BTYPE)) == VT_BYTE)
                        t |= VT_UNSIGNED;
                }

            /* VT_LONG is used just as a modifier for VT_INT / VT_LLONG */
                bt = t & (VT_BTYPE|VT_LONG);
                if (bt == VT_LONG)
                    t |= LONG_SIZE == 8 ? VT_LLONG : VT_INT;

                if (bt == VT_LDOUBLE)
                    t = (t & ~(VT_BTYPE|VT_LONG)) | VT_DOUBLE;

                type.t = t;
            return type_found;
        }

        public void convert_parameter_type() { }
        public void parse_asm_str() { }
        public void asm_label_instr() { }
        public void post_type() { }
        public void type_decl() { }
        public void lvalue_type() { }
        public void indir() { }
        public void gfunc_param_typed() { }
        public void expr_type() { }

        public void parse_expr_type(ref CType type)
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
        public void gen_function() { }
        public void gen_inline_functions() { }
        public void free_inline_functions() { }

        public int decl0(int l, bool is_for_loop_init, Sym func_sym)
        {
            int v;
            int has_init;
            int r;
            CType type;
            CType btype = null;
            Sym sym;
            AttributeDef ad = null;

            while (true)
            {
                if (!parse_btype(ref btype, ref ad))
                {
                    if (is_for_loop_init)
                        return 0;

                    //            /* skip redundant ';' if not in old parameter decl scope */
                    //            if (tok == ';' && l != VT_CMP) {
                    //                next();
                    //                continue;
                    //            }
                    //            if (l != VT_CONST)
                    //                break;
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
                    //        }
                    //        if (tok == ';') {
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

                    //type_decl(&type, &ad, &v, TYPE_DIRECT);

                    //            if ((type.t & VT_BTYPE) == VT_FUNC) {
                    //                if ((type.t & VT_STATIC) && (l == VT_LOCAL)) {
                    //                    tcc_error("function without file scope cannot be static");
                    //                }
                    //                /* if old style function prototype, we accept a
                    //                   declaration list */
                    //                sym = type.ref;
                    //                if (sym->f.func_type == FUNC_OLD && l == VT_CONST)
                    //                    decl0(VT_CMP, 0, sym);
                    //            }

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

                    //            if (tok == '{') {
                    //                if (l != VT_CONST)
                    //                    tcc_error("cannot use local functions");
                    //                if ((type.t & VT_BTYPE) != VT_FUNC)
                    //                    expect("function definition");
                    //
                    //                /* reject abstract declarators in function definition
                    //		   make old style params without decl have int type */
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

                    //                /* put function symbol */
                    //                sym = external_global_sym(v, &type, 0);
                    //                type.t &= ~VT_EXTERN;
                    //                patch_storage(sym, &ad, &type);
                    //
                    //                /* static inline functions are just recorded as a kind
                    //                   of macro. Their code will be emitted at the end of
                    //                   the compilation unit only if they are used */
                    //                if ((type.t & (VT_INLINE | VT_STATIC)) == 
                    //                    (VT_INLINE | VT_STATIC)) {
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
                    //                } else {
                    //                    /* compute text section */
                    //                    cur_text_section = ad.section;
                    //                    if (!cur_text_section)
                    //                        cur_text_section = text_section;
                    //                    gen_function(sym);
                    //                }
                    //                break;
                    //            } else {
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
                    //            }
                    //            ad.a.aligned = 0;
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
    }

    //---------------------------------------------------------------------

    public class Section
    {
        public ulong data_offset;          /* current data offset */
        public byte[] data;                /* section data */
        public ulong data_allocated;       /* used for realloc() handling */
        public int sh_name;                /* elf section name (only used during output) */
        public int sh_num;                 /* elf section number */
        public int sh_type;                /* elf section type */
        public int sh_flags;               /* elf section flags */
        public int sh_info;                /* elf section info */
        public int sh_addralign;           /* elf section alignment */
        public int sh_entsize;             /* elf entry size */
        public ulong sh_size;              /* section size (only used during output) */
        public ulong sh_addr;              /* address at which the section is relocated */
        public ulong sh_offset;            /* file offset */
        public int nb_hashed_syms;         /* used to resize the hash table */
        public Section link;               /* link to another section */
        public Section reloc;              /* corresponding section for relocation, if any */
        public Section hash;               /* hash table for symbols */
        public Section prev;               /* previous section on section stack */
        public string name;                /* section name */
    }
}
