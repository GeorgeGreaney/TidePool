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
    public class Section
    {
        public const int SHDRSIZE = 40;
        public static readonly byte[] EMPTYSECHDR = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

        /* from tcc.h - section definition */
        public int data_offset;             /* current data offset */
        public byte[] data;                 /* section data */
        public int data_allocated;          /* used for realloc() handling */

        public int sh_name;                 /* elf section name (only used during output) */
        public int sh_num;                  /* elf section number */
        public SECTIONTYPE sh_type;         /* elf section type */
        public SECTIONFLAGS sh_flags;       /* elf section flags */
        public int sh_info;                 /* elf section info */
        public int sh_addralign;            /* elf section alignment */
        public int sh_entsize;              /* elf entry size */
        public int sh_size;                 /* section size (only used during output) */
        public uint sh_addr;                /* address at which the section is relocated */
        public int sh_offset;               /* file offset */

        public int nb_hashed_syms;          /* used to resize the hash table */

        public Section link;                /* link to another section */
        public Section reloc;               /* corresponding section for relocation, if any */
        public Section hash;                /* hash table for symbols */
        public Section prev;                /* previous section on section stack */

        public string name;                 /* section name */

        //from tcc.h

        //#define ARMAG  "!<arch>\012"    /* For COFF and a.out archives */

        //typedef struct {
        //    unsigned int n_strx;         /* index into string table of name */
        //    unsigned char n_type;         /* type of symbol */
        //    unsigned char n_other;        /* misc info (usually empty) */
        //    unsigned short n_desc;        /* description field */
        //    unsigned int n_value;        /* value of symbol */
        //} Stab_Sym;

        public static Section textSection;
        public static Section dataSection;
        public static Section bssSection;             /* predefined sections */
        public static Section commonSection;
        public static Section curTextSection;        /* current section where function code is generated */

        //#ifdef CONFIG_TCC_ASM
        //ST_DATA Section *last_text_section; /* to handle .previous asm directive */
        //#endif

        //#ifdef CONFIG_TCC_BCHECK
        ///* bound check related sections */
        //ST_DATA Section *bounds_section; /* contains global data bound description */
        //ST_DATA Section *lbounds_section; /* contains local data bound description */
        //ST_FUNC void tccelf_bounds_new(TCCState *s);
        //#endif

        /* symbol sections */
        public static Section symtab_section;

        /* debug sections */
        public static Section stab_section;
        public static Section stabstr_section;

        //kludge
        public Dictionary<int, Elf32_Sym> SymbolDict;

        //new_section
        public Section(TidePool tp, string _name, SECTIONTYPE _sh_type, SECTIONFLAGS _sh_flags)
        {
            data = null;
            data_allocated = 0;

            name = _name;
            sh_type = _sh_type;
            sh_flags = _sh_flags;

            switch (sh_type)
            {
                case SECTIONTYPE.SHT_HASH:
                case SECTIONTYPE.SHT_REL:
                case SECTIONTYPE.SHT_RELA:
                case SECTIONTYPE.SHT_DYNSYM:
                case SECTIONTYPE.SHT_SYMTAB:
                case SECTIONTYPE.SHT_DYNAMIC:
                    sh_addralign = 4;
                    break;
                case SECTIONTYPE.SHT_STRTAB:
                    sh_addralign = 1;
                    break;
                default:
                    sh_addralign = Generator.PTR_SIZE; /* gcc/pcc default alignment */
                    break;
            }

            if ((sh_flags & SECTIONFLAGS.SHF_PRIVATE) != 0)
            {
                tp.priv_sections.Add(this);
                tp.nb_priv_sections++;
            }
            else
            {
                sh_num = tp.nb_sections;
                tp.sections.Add(this);
                tp.nb_sections++;
            }

            SymbolDict = new Dictionary<int, Elf32_Sym>();
        }

        //tccelf_new
        public static void initSection(TidePool tp)
        {
            /* no section zero */
            tp.sections.Add(null);
            tp.nb_sections++;

            /* create standard sections */
            textSection = new Section(tp, ".text", SECTIONTYPE.SHT_PROGBITS, SECTIONFLAGS.SHF_ALLOC | SECTIONFLAGS.SHF_EXECINSTR);
            dataSection = new Section(tp, ".data", SECTIONTYPE.SHT_PROGBITS, SECTIONFLAGS.SHF_ALLOC | SECTIONFLAGS.SHF_WRITE);
            bssSection = new Section(tp, ".bss", SECTIONTYPE.SHT_NOBITS, SECTIONFLAGS.SHF_ALLOC | SECTIONFLAGS.SHF_WRITE);
            commonSection = new Section(tp, ".common", SECTIONTYPE.SHT_NOBITS, SECTIONFLAGS.SHF_PRIVATE);
            commonSection.sh_num = (int)SECTIONIDX.SHN_COMMON;

            /* symbols are always generated for linking stage */
            symtab_section = new_symtab(tp, ".symtab", SECTIONTYPE.SHT_SYMTAB, 0, ".strtab", ".hashtab", SECTIONFLAGS.SHF_PRIVATE);
            tp.symtab = symtab_section;

            /* private symbol table for dynamic symbols */
            //    s->dynsymtab_section = new_symtab(s, ".dynsymtab", SHT_SYMTAB, SHF_PRIVATE|SHF_DYNSYM,
            //                                      ".dynstrtab",
            //                                      ".dynhashtab", SHF_PRIVATE);
            //    get_sym_attr(s, 0, 1);
        }

        public void tccelf_bounds_new() { }
        public void tccelf_stab_new() { }

        public void tccelf_delete(TidePool tp)
        {
            int i;

            /* free all sections */
            //    for(i = 1; i < s1->nb_sections; i++)
            //        free_section(s1->sections[i]);
            //    dynarray_reset(&s1->sections, &s1->nb_sections);

            //    for(i = 0; i < s1->nb_priv_sections; i++)
            //        free_section(s1->priv_sections[i]);
            //    dynarray_reset(&s1->priv_sections, &s1->nb_priv_sections);

            /* free any loaded DLLs */
            //    for ( i = 0; i < s1->nb_loaded_dlls; i++) {
            //        DLLReference *ref = s1->loaded_dlls[i];
            //        if ( ref->handle )
            //            FreeLibrary((HMODULE)ref->handle);
            //    }

            /* free loaded dlls array */
            //    dynarray_reset(&s1->loaded_dlls, &s1->nb_loaded_dlls);
            //    tcc_free(s1->sym_attrs);

            //    symtab_section = NULL; /* for tccrun.c:rt_printline() */

        }

        /* save section data state */
        public void tccelf_begin_file(TidePool tp)
        {
            //                Section *s; int i;
            //    printf("[tccelf.c] tccelf_begin_file \n");
            //    for (i = 1; i < s1->nb_sections; i++) {
            //        s = s1->sections[i];
            //        s->sh_offset = s->data_offset;
            //    }
            //    /* disable symbol hashing during compilation */
            //    s = s1->symtab, s->reloc = s->hash, s->hash = NULL;
            //#if defined TCC_TARGET_X86_64 && defined TCC_TARGET_PE
            //    s1->uw_sym = 0;
            //#endif
        }

        /* At the end of compilation, convert any UNDEF syms to global, and merge with previously existing symbols */
        public void tccelf_end_file(TidePool tp)
        {
            //            Section *s = s1->symtab;
            //int first_sym, nb_syms, *tr, i;

            //printf("[tccelf.c] tccelf_end_file \n");
            //first_sym = s->sh_offset / sizeof (ElfSym);
            //nb_syms = s->data_offset / sizeof (ElfSym) - first_sym;
            //s->data_offset = s->sh_offset;
            //s->link->data_offset = s->link->sh_offset;
            //s->hash = s->reloc, s->reloc = NULL;
            //tr = tcc_mallocz(nb_syms * sizeof *tr);

            //for (i = 0; i < nb_syms; ++i) {
            //    ElfSym *sym = (ElfSym*)s->data + first_sym + i;
            //    if (sym->st_shndx == SHN_UNDEF
            //        && ELFW(ST_BIND)(sym->st_info) == STB_LOCAL)
            //        sym->st_info = ELFW(ST_INFO)(STB_GLOBAL, ELFW(ST_TYPE)(sym->st_info));
            //    tr[i] = set_elf_sym(s, sym->st_value, sym->st_size, sym->st_info,
            //        sym->st_other, sym->st_shndx, s->link->data + sym->st_name);
            //}
            ///* now update relocations */
            //for (i = 1; i < s1->nb_sections; i++) {
            //    Section *sr = s1->sections[i];
            //    if (sr->sh_type == SHT_RELX && sr->link == s) {
            //        ElfW_Rel *rel = (ElfW_Rel*)(sr->data + sr->sh_offset);
            //        ElfW_Rel *rel_end = (ElfW_Rel*)(sr->data + sr->data_offset);
            //        for (; rel < rel_end; ++rel) {
            //            int n = ELFW(R_SYM)(rel->r_info) - first_sym;
            //            //if (n < 0) tcc_error("internal: invalid symbol index in relocation");
            //            rel->r_info = ELFW(R_INFO)(tr[n], ELFW(R_TYPE)(rel->r_info));
            //        }
            //    }
            //}
            //tcc_free(tr);
        }

        public static Section new_symtab(TidePool tp, string symtab_name, SECTIONTYPE sh_type, SECTIONFLAGS sh_flags,
                                        string strtab_name, string hash_name, SECTIONFLAGS hash_sh_flags)
        {
            Section symtab;
            Section strtab;
            Section hash;
            int ptr;
            int nb_buckets;

            symtab = new Section(tp, symtab_name, sh_type, sh_flags);
            symtab.sh_entsize = Elf32_Sym.SYMENTSIZE;
            symtab.put_elf_sym(0, 0, 0, 0, 0, null);

            strtab = new Section(tp, strtab_name, SECTIONTYPE.SHT_STRTAB, sh_flags);
            strtab.put_elf_str("");
            symtab.link = strtab;

            nb_buckets = 1;

            hash = new Section(tp, hash_name, SECTIONTYPE.SHT_HASH, hash_sh_flags);
            hash.sh_entsize = sizeof(int);
            symtab.hash = hash;
            hash.link = symtab;

            //ptr = section_ptr_add(hash, (2 + nb_buckets + 1) * sizeof(int));
            //ptr[0] = nb_buckets;
            //ptr[1] = 1;
            //memset(ptr + 2, 0, (nb_buckets + 1) * sizeof(int));
            return symtab;
        }

        public void section_realloc(int new_size)
        {
            int size = data_allocated;
            if (size == 0)
                size = 1;
            while (size < new_size)
                size = size * 2;
            Array.Resize(ref data, size);
            data_allocated = size;
        }

        /* reserve at least 'size' bytes aligned per 'align' in section
            'sec' from current offset, and return the aligned offset */
        public int section_add(int size, int align)
        {
            int offset;
            int offset1;

            offset = (data_offset + align - 1) & -align;
            offset1 = offset + size;
            if ((sh_type != SECTIONTYPE.SHT_NOBITS) && (offset1 > data_allocated))
                section_realloc(offset1);
            data_offset = offset1;
            if (align > sh_addralign)
                sh_addralign = align;
            return offset;
        }

        /* reserve at least 'size' bytes in section 'sec' from sec->data_offset. */
        public int section_ptr_add(int size)
        {
            int offset = section_add(size, 1);
            return offset;
        }

        public void section_reserve() { }
        public void find_section() { }

        //---------------------------------------------------------------------

        public int put_elf_str(string sym)
        {
            int len = sym.Length;
            int offset = data_offset;
            int ptr = section_ptr_add(len + 1);     //include space for the ending 0
            for (int i = 0; i < len; i++)
            {
                data[ptr + i] = (byte)sym[i];
            }
            data[ptr + len] = 0;
            return offset;
        }

        /* elf symbol hashing function */
        public int elf_hash(string name)
        {
            //            unsigned long h = 0, g;

            //printf("[tccelf.c] elf_hash \n");
            //while (*name) {
            //    h = (h << 4) + *name++;
            //    g = h & 0xf0000000;
            //    if (g)
            //        h ^= g >> 24;
            //    h &= ~g;
            //}
            //return h;
            return 0;
        }

        public void rebuild_hash() { }

        /* return the symbol number */
        public int put_elf_sym(int value, int size, int info, int other, int shndx, string name)
        {
            int name_offset;
            int sym_index = 0;
            int nbuckets;
            int h;
            Elf32_Sym sym;
            int symidx;
            Section hs;

            //tinyc makes space in the data buf for a new Elf32_Sym rec & returns the rec ofs
            //we can't do that here, so use a List<> instead - this may cause further porting contorsions
            symidx = section_ptr_add(Elf32_Sym.SYMENTSIZE);
            if ((name != null) && (name[0] != 0))
            {
                name_offset = link.put_elf_str(name);
            }
            else
            {
                name_offset = 0;
            }

            /* XXX: endianness */
            sym = new Elf32_Sym();
            sym.st_name = name_offset;
            sym.st_value = value;
            sym.st_size = size;
            sym.st_info = info;
            sym.st_other = other;
            sym.st_shndx = shndx;
            sym_index = symidx; 
            sym.storeData(data, symidx);

            hs = hash;
            if (hs != null)
            {
                //    int *ptr, *base;
                //    ptr = section_ptr_add(hs, sizeof(int));
                //    base = (int *)hs->data;

                /* only add global or weak symbols. */
                if ((info >> 4) != (int)STBIND.STB_LOCAL)
                {
                    //        /* add another hashing entry */
                    //        nbuckets = base[0];
                    //        h = elf_hash((unsigned char *)s->link->data + name_offset) % nbuckets;
                    //        *ptr = base[2 + h];
                    //        base[2 + h] = sym_index;
                    //        base[1]++;
                    //        /* we resize the hash table */
                    //        hs->nb_hashed_syms++;
                    //        if (hs->nb_hashed_syms > 2 * nbuckets) {
                    //            rebuild_hash(s, 2 * nbuckets);
                    //        }
                }
                else
                {
                    //        *ptr = 0;
                    //        base[1]++;
                }
            }

            SymbolDict.Add(sym_index, sym);
            return sym_index;
        }

        public int find_elf_sym(string name)
        {
            //            ElfW(Sym) *sym;
            //Section *hs;
            //int nbuckets, sym_index, h;
            //const char *name1;

            //printf("[tccelf.c] find_elf_sym \n");
            //hs = s->hash;
            //if (!hs)
            //    return 0;
            //nbuckets = ((int *)hs->data)[0];
            //h = elf_hash((unsigned char *) name) % nbuckets;
            //sym_index = ((int *)hs->data)[2 + h];
            //while (sym_index != 0) {
            //    sym = &((ElfW(Sym) *)s->data)[sym_index];
            //    name1 = (char *) s->link->data + sym->st_name;
            //    if (!strcmp(name, name1))
            //        return sym_index;
            //    sym_index = ((int *)hs->data)[2 + nbuckets + sym_index];
            //}
            return 0;
        }

        /* return elf symbol value, signal error if 'err' is nonzero */
        public static uint get_elf_sym_addr(TidePool tp, string name, int err)
        {
            int sym_index;
            //ElfW(Sym) *sym;

            //sym_index = find_elf_sym(s->symtab, name);
            //sym = &((ElfW(Sym) *)s->symtab->data)[sym_index];
            //if (!sym_index || sym->st_shndx == SHN_UNDEF) {
            //    if (err)
            //        tcc_error("%s not defined", name);
            return 0;
            //}
            //return sym->st_value;
        }

        public void tcc_get_symbol() { }
        public void tcc_get_symbol_err() { }

        /* add an elf symbol : check if it is already defined and patch
           it. Return symbol index. NOTE that sh_num can be SHN_UNDEF. */
        public int set_elf_sym(int value, int size, int info, int other, int shndx, string name)
        {
            //                ElfW(Sym) *esym;
            int sym_bind;
            int sym_index = 0;
            int sym_type;
            int esym_bind;
            //    unsigned char sym_vis, esym_vis, new_vis;

            //    sym_bind = ELFW(ST_BIND)(info);
            //    sym_type = ELFW(ST_TYPE)(info);
            //    sym_vis = ELFW(ST_VISIBILITY)(other);

            //    if (sym_bind != STB_LOCAL) {
            //        /* we search global or weak symbols */
            //        sym_index = find_elf_sym(s, name);
            //        if (!sym_index)
            //            goto do_def;
            //        esym = &((ElfW(Sym) *)s->data)[sym_index];
            //        if (esym->st_value == value && esym->st_size == size && esym->st_info == info
            //            && esym->st_other == other && esym->st_shndx == shndx)
            //            return sym_index;
            //        if (esym->st_shndx != SHN_UNDEF) {
            //            esym_bind = ELFW(ST_BIND)(esym->st_info);
            //            /* propagate the most constraining visibility */
            //            /* STV_DEFAULT(0)<STV_PROTECTED(3)<STV_HIDDEN(2)<STV_INTERNAL(1) */
            //            esym_vis = ELFW(ST_VISIBILITY)(esym->st_other);
            //            if (esym_vis == STV_DEFAULT) {
            //                new_vis = sym_vis;
            //            } else if (sym_vis == STV_DEFAULT) {
            //                new_vis = esym_vis;
            //            } else {
            //                new_vis = (esym_vis < sym_vis) ? esym_vis : sym_vis;
            //            }
            //            esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
            //                | new_vis;
            //            other = esym->st_other; /* in case we have to patch esym */
            //            if (shndx == SHN_UNDEF) {
            //                /* ignore adding of undefined symbol if the
            //                corresponding symbol is already defined */
            //            } else if (sym_bind == STB_GLOBAL && esym_bind == STB_WEAK) {
            //                /* global overrides weak, so patch */
            //                goto do_patch;
            //            } else if (sym_bind == STB_WEAK && esym_bind == STB_GLOBAL) {
            //                /* weak is ignored if already global */
            //            } else if (sym_bind == STB_WEAK && esym_bind == STB_WEAK) {
            //                /* keep first-found weak definition, ignore subsequents */
            //            } else if (sym_vis == STV_HIDDEN || sym_vis == STV_INTERNAL) {
            //                /* ignore hidden symbols after */
            //            } else if ((esym->st_shndx == SHN_COMMON
            //                || esym->st_shndx == bss_section->sh_num)
            //                && (shndx < SHN_LORESERVE
            //                && shndx != bss_section->sh_num)) {
            //                    /* data symbol gets precedence over common/bss */
            //                    goto do_patch;
            //            } else if (shndx == SHN_COMMON || shndx == bss_section->sh_num) {
            //                /* data symbol keeps precedence over common/bss */
            //            } else if (s->sh_flags & SHF_DYNSYM) {
            //                /* we accept that two DLL define the same symbol */
            //            } else if (esym->st_other & ST_ASM_SET) {
            //                /* If the existing symbol came from an asm .set
            //                we can override.  */
            //                goto do_patch;
            //            } else {
            //#if 0
            //                printf("new_bind=%x new_shndx=%x new_vis=%x old_bind=%x old_shndx=%x old_vis=%x\n",
            //                    sym_bind, shndx, new_vis, esym_bind, esym->st_shndx, esym_vis);
            //#endif
            //                tcc_error_noabort("'%s' defined twice", name);
            //            }
            //        } else {
            //do_patch:
            //            esym->st_info = ELFW(ST_INFO)(sym_bind, sym_type);
            //            esym->st_shndx = shndx;
            //            new_undef_sym = 1;
            //            esym->st_value = value;
            //            esym->st_size = size;
            //            esym->st_other = other;
            //        }
            //    } else {
            //do_def:
            //        sym_index = put_elf_sym(s, value, size, ELFW(ST_INFO)(sym_bind, sym_type), other, shndx, name);
            //    }
            return sym_index;
        }

        public void put_elf_reloca() { }
        public void put_elf_reloc() { }
        public void squeeze_multi_relocs() { }
        public void put_stabs() { }
        public void put_stabs_r() { }
        public void put_stabn() { }
        public void put_stabd() { }

        public sym_attr get_sym_attr(TidePool tp, int index, int alloc)
        {
            //            int n;
            //struct sym_attr *tab;

            //printf("[tccelf.c] get_sym_attr \n");
            //if (index >= s1->nb_sym_attrs) {
            //    if (!alloc)
            //        return s1->sym_attrs;
            //    /* find immediately bigger power of 2 and reallocate array */
            //    n = 1;
            //    while (index >= n)
            //        n *= 2;
            //    tab = tcc_realloc(s1->sym_attrs, n * sizeof(*s1->sym_attrs));
            //    s1->sym_attrs = tab;
            //    memset(s1->sym_attrs + s1->nb_sym_attrs, 0,
            //        (n - s1->nb_sym_attrs) * sizeof(*s1->sym_attrs));
            //    s1->nb_sym_attrs = n;
            //}
            //return &s1->sym_attrs[index];
            return null;
        }

        /* In an ELF file symbol table, the local symbols must appear below
the global and weak ones. Since TCC cannot sort it while generating
the code, we must do it after. All the relocation tables are also
modified to take into account the symbol table sorting */
        public static void sort_syms(TidePool tp, Section s)
        {
            int[] old_to_new_syms;
            Elf32_Sym[] new_syms;
            int nb_syms;
            int i;
            //ElfW(Sym) *p, *q;
            int p;
            int q;
            //ElfW_Rel *rel;
            Section sr;
            int type;
            int sym_index;

            nb_syms = s.data_offset / Elf32_Sym.SYMENTSIZE;
            new_syms = new Elf32_Sym[nb_syms];
            old_to_new_syms = new int[nb_syms];                

            /* first pass for local symbols */
            //p = (ElfW(Sym) *)s->data;
            //q = new_syms;
            p = 0;
            q = 0;
            for(i = 0; i < nb_syms; i++) {
            //    if (ELFW(ST_BIND)(p->st_info) == STB_LOCAL) {
            //        old_to_new_syms[i] = q - new_syms;
            //        *q++ = *p;
            //    }
            //    p++;
            }

            /* save the number of local symbols in section header */
            if( s.sh_size > 0)             /* this 'if' makes IDA happy */
            //s.sh_info = q;
                s.sh_info = nb_syms - 1;        //late night kludge

            /* then second pass for non local symbols */
            //p = (ElfW(Sym) *)s->data;
            //for(i = 0; i < nb_syms; i++) {
            //    if (ELFW(ST_BIND)(p->st_info) != STB_LOCAL) {
            //        old_to_new_syms[i] = q - new_syms;
            //        *q++ = *p;
            //    }
            //    p++;
            //}

            /* we copy the new symbols to the old */
            //memcpy(s->data, new_syms, nb_syms * sizeof(ElfW(Sym)));
            //tcc_free(new_syms);

            /* now we modify all the relocations */
            //for(i = 1; i < s1->nb_sections; i++) {
            //    sr = s1->sections[i];
            //    if (sr->sh_type == SHT_RELX && sr->link == s) {
            //        for_each_elem(sr, 0, rel, ElfW_Rel) {
            //            sym_index = ELFW(R_SYM)(rel->r_info);
            //            type = ELFW(R_TYPE)(rel->r_info);
            //            sym_index = old_to_new_syms[sym_index];
            //            rel->r_info = ELFW(R_INFO)(sym_index, type);
            //        }
            //    }
            //}

            //tcc_free(old_to_new_syms);

        }

        public void relocate_syms() { }
        public void relocate_section() { }
        public void relocate_rel() { }

        /* count the number of dynamic relocations so that we can reserve their space */
        public static int prepare_dynamic_rel(TidePool tp, Section sr)
        {
            //Elf32_Rel rel;
            int sym_index;
            int type;
            int count;

            count = 0;
            //    for_each_elem(sr, 0, rel, ElfW_Rel) {
            //        sym_index = ELFW(R_SYM)(rel->r_info);
            //        type = ELFW(R_TYPE)(rel->r_info);
            //        switch(type) {
            //#if defined(TCC_TARGET_I386)
            //        case R_386_32:
            //            if (!get_sym_attr(s1, sym_index, 0)->dyn_index
            //                && ((ElfW(Sym)*)symtab_section->data + sym_index)->st_shndx == SHN_UNDEF) {
            //                    /* don't fixup unresolved (weak) symbols */
            //                    rel->r_info = ELFW(R_INFO)(sym_index, R_386_RELATIVE);
            //                    break;
            //            }
            //#elif defined(TCC_TARGET_X86_64)
            //        case R_X86_64_32:
            //        case R_X86_64_32S:
            //        case R_X86_64_64:
            //#endif
            //            count++;
            //            break;
            //#if defined(TCC_TARGET_I386)
            //        case R_386_PC32:
            //#elif defined(TCC_TARGET_X86_64)
            //        case R_X86_64_PC32:
            //#endif
            //            if (get_sym_attr(s1, sym_index, 0)->dyn_index)
            //                count++;
            //            break;
            //        default:
            //            break;
            //        }
            //    }
            //    if (count) {
            //        /* allocate the section */
            //        sr->sh_flags |= SHF_ALLOC;
            //        sr->sh_size = count * sizeof(ElfW_Rel);
            //    }
            return count;
        }

        public void build_got() { }
        public void put_got_entry() { }
        public void build_got_entries() { }
        public void put_dt() { }
        public void tcc_add_support() { }
        public void tcc_add_bcheck() { }
        public void tcc_add_runtime() { }
        public void tcc_add_linker_symbols() { }
        public void resolve_common_syms() { }

        public static void tcc_output_binary(TidePool tp, FileStream f, int[] sec_order)
        {
            Section s;
            int i;
            int offset;
            int size;

            offset = 0;
            for (i = 1; i < tp.nb_sections; i++)
            {
                //    s = s1->sections[sec_order[i]];
                //    if (s->sh_type != SHT_NOBITS &&
                //        (s->sh_flags & SHF_ALLOC))
                //    {
                //        while (offset < s->sh_offset)
                //        {
                //            fputc(0, f);
                //            offset++;
                //        }
                //        size = s->sh_size;
                //        fwrite(s->data, 1, size, f);
                //        offset += size;
                //    }
            }
        }

        public void fill_got_entry() { }
        public void fill_got() { }
        public void fill_local_got_entries() { }
        public void bind_exe_dynsyms() { }
        public void bind_libs_dynsyms() { }
        public void export_global_syms() { }

        public static int alloc_sec_names(TidePool tp, OUTPUTTYPE file_type, Section strsec)
        {
            int i;
            Section s;
            int textrel = 0;

            /* Allocate strings for section names */
            for (i = 1; i < tp.nb_sections; i++)
            {
                s = tp.sections[i];
                /* when generating a DLL, we include relocations but we may patch them */
                if ((file_type == OUTPUTTYPE.TP_OUTPUT_DLL) && (s.sh_type == SECTIONTYPE.SHT_REL) && ((s.sh_flags & SECTIONFLAGS.SHF_ALLOC) == 0)
                    && ((tp.sections[s.sh_info].sh_flags & SECTIONFLAGS.SHF_ALLOC) != 0) && (prepare_dynamic_rel(tp, s) > 0))
                {
                    //        if (s1->sections[s->sh_info]->sh_flags & SHF_EXECINSTR)
                    //            textrel = 1;
                }
                else if ((tp.do_debug != 0) || file_type == OUTPUTTYPE.TP_OUTPUT_OBJ ||
                    ((s.sh_flags & SECTIONFLAGS.SHF_ALLOC) != 0) || (i == (tp.nb_sections - 1)))
                {
                    /* we output all sections if debug or object file */
                    s.sh_size = s.data_offset;
                }

                if ((s.sh_size > 0) || ((s.sh_flags & SECTIONFLAGS.SHF_ALLOC) != 0))
                    s.sh_name = strsec.put_elf_str(s.name);
            }
            strsec.sh_size = strsec.data_offset;
            return textrel;
        }

        public static int layout_sections(TidePool tp, Elf32_Phdr[] phdr, int phnum, Section interp, Section strsec, dyn_inf dyninf, int[] sec_order)
        {
            int i;
            int j;
            int k;
            OUTPUTTYPE file_type;
            int sh_order_index;
            int file_offset;
            int s_align;
            int tmp;
            int addr;
            Elf32_Phdr ph;
            Section s;

            file_type = tp.output_type;
            sh_order_index = 1;
            file_offset = 0;

            if (tp.output_format == OUTPUTFORMAT.TP_OUTPUT_FORMAT_ELF)
            {
                file_offset = Elf32_Ehdr.EHDRSIZE + (phnum * Elf32_Phdr.PHDRSIZE);
            }
            s_align = Linker.ELF_PAGE_SIZE;
            if (tp.section_align > 0)
                s_align = tp.section_align;

            if (phnum > 0)
            {
                if (tp.has_text_addr != 0)
                {
                    //            int a_offset, p_offset;
                    //            addr = s1->text_addr;
                    //            /* we ensure that (addr % ELF_PAGE_SIZE) == file_offset %
                    //            ELF_PAGE_SIZE */
                    //            a_offset = (int) (addr & (s_align - 1));
                    //            p_offset = file_offset & (s_align - 1);
                    //            if (a_offset < p_offset)
                    //                a_offset += s_align;
                    //            file_offset += (a_offset - p_offset);
                }
                else
                {
                    //            if (file_type == TCC_OUTPUT_DLL)
                    //                addr = 0;
                    //            else
                    //                addr = ELF_START_ADDR;
                    /* compute address after headers */
                    //            addr += (file_offset & (s_align - 1));
                }

                //        ph = &phdr[0];
                /* Leave one program headers for the program interpreter and one for
                the program header table itself if needed. These are done later as
                they require section layout to be done first. */
                //        if (interp)
                //            ph += 2;

                /* dynamic relocation table information, for .dynamic section */
                //        dyninf->rel_addr = dyninf->rel_size = 0;
                //#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
                //        dyninf->bss_addr = dyninf->bss_size = 0;
                //#endif

                for (j = 0; j < 2; j++)
                {
                    //            ph->p_type = PT_LOAD;
                    //            if (j == 0)
                    //                ph->p_flags = PF_R | PF_X;
                    //            else
                    //                ph->p_flags = PF_R | PF_W;
                    //            ph->p_align = s_align;

                    /* Decide the layout of sections loaded in memory. This must
                    be done before program headers are filled since they contain
                    info about the layout. We do the following ordering: interp,
                    symbol tables, relocations, progbits, nobits */
                    /* XXX: do faster and simpler sorting */
                    //            for(k = 0; k < 5; k++) {
                    //                for(i = 1; i < s1->nb_sections; i++) {
                    //                    s = s1->sections[i];
                    //                    /* compute if section should be included */
                    //                    if (j == 0) {
                    //                        if ((s->sh_flags & (SHF_ALLOC | SHF_WRITE)) !=
                    //                            SHF_ALLOC)
                    //                            continue;
                    //                    } else {
                    //                        if ((s->sh_flags & (SHF_ALLOC | SHF_WRITE)) !=
                    //                            (SHF_ALLOC | SHF_WRITE))
                    //                            continue;
                    //                    }
                    //                    if (s == interp) {
                    //                        if (k != 0)
                    //                            continue;
                    //                    } else if (s->sh_type == SHT_DYNSYM ||
                    //                        s->sh_type == SHT_STRTAB ||
                    //                        s->sh_type == SHT_HASH) {
                    //                            if (k != 1)
                    //                                continue;
                    //                    } else if (s->sh_type == SHT_RELX) {
                    //                        if (k != 2)
                    //                            continue;
                    //                    } else if (s->sh_type == SHT_NOBITS) {
                    //                        if (k != 4)
                    //                            continue;
                    //                    } else {
                    //                        if (k != 3)
                    //                            continue;
                    //                    }
                    //                    sec_order[sh_order_index++] = i;

                    /* section matches: we align it and add its size */
                    //                    tmp = addr;
                    //                    addr = (addr + s->sh_addralign - 1) &
                    //                        ~(s->sh_addralign - 1);
                    //                    file_offset += (int) ( addr - tmp );
                    //                    s->sh_offset = file_offset;
                    //                    s->sh_addr = addr;

                    /* update program header infos */
                    //                    if (ph->p_offset == 0) {
                    //                        ph->p_offset = file_offset;
                    //                        ph->p_vaddr = addr;
                    //                        ph->p_paddr = ph->p_vaddr;
                    //                    }
                    //                    /* update dynamic relocation infos */
                    //                    if (s->sh_type == SHT_RELX) {
                    //#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
                    //                        if (!strcmp(strsec->data + s->sh_name, ".rel.got")) {
                    //                            dyninf->rel_addr = addr;
                    //                            dyninf->rel_size += s->sh_size; /* XXX only first rel. */
                    //                        }
                    //                        if (!strcmp(strsec->data + s->sh_name, ".rel.bss")) {
                    //                            dyninf->bss_addr = addr;
                    //                            dyninf->bss_size = s->sh_size; /* XXX only first rel. */
                    //                        }
                    //#else
                    //                        if (dyninf->rel_size == 0)
                    //                            dyninf->rel_addr = addr;
                    //                        dyninf->rel_size += s->sh_size;
                    //#endif
                    //                    }
                    //                    addr += s->sh_size;
                    //                    if (s->sh_type != SHT_NOBITS)
                    //                        file_offset += s->sh_size;
                    //                }
                    //            }

                    if (j == 0)
                    {
                        /* Make the first PT_LOAD segment include the program
                        headers itself (and the ELF header as well), it'll
                        come out with same memory use but will make various
                        tools like binutils strip work better.  */
                        //                ph->p_offset &= ~(ph->p_align - 1);
                        //                ph->p_vaddr &= ~(ph->p_align - 1);
                        //                ph->p_paddr &= ~(ph->p_align - 1);
                    }
                    //            ph->p_filesz = file_offset - ph->p_offset;
                    //            ph->p_memsz = addr - ph->p_vaddr;
                    //            ph++;
                    if (j == 0)
                    {
                        if (tp.output_format == OUTPUTFORMAT.TP_OUTPUT_FORMAT_ELF)
                        {
                            /* if in the middle of a page, we duplicate the page in
                            memory so that one copy is RX and the other is RW */
                            //                    if ((addr & (s_align - 1)) != 0)
                            //                        addr += s_align;
                        }
                        else
                        {
                            //                    addr = (addr + s_align - 1) & ~(s_align - 1);
                            //                    file_offset = (file_offset + s_align - 1) & ~(s_align - 1);
                        }
                    }
                }
            }

            /* all other sections come after */
            for (i = 1; i < tp.nb_sections; i++)
            {
                s = tp.sections[i];
                if (phnum > 0 && (s.sh_flags & SECTIONFLAGS.SHF_ALLOC) != 0)
                    continue;
                sec_order[sh_order_index++] = i;

                file_offset = (file_offset + s.sh_addralign - 1) & ~(s.sh_addralign - 1);
                s.sh_offset = file_offset;
                if (s.sh_type != SECTIONTYPE.SHT_NOBITS)
                    file_offset += s.sh_size;
            }

            return file_offset;
        }

        public void fill_unloadable_phdr() { }
        public void fill_dynamic() { }
        public void final_sections_reloc() { }

        /* Create an ELF file on disk. This function handles ELF specific layout requirements */
        public static void tcc_output_elf(TidePool tp, FileStream f, int phnum, Elf32_Phdr[] phdr, int file_offset, int[] sec_order)
        {
            int i;
            ushort shnum;
            int offset;
            int size;
            OUTPUTTYPE file_type;
            Section s;
            Elf32_Ehdr ehdr = new Elf32_Ehdr();

            file_type = tp.output_type;
            shnum = (ushort)tp.nb_sections;

            //build obj header
            if (phnum > 0)
            {
                ehdr.e_phentsize = Elf32_Phdr.PHDRSIZE;
                ehdr.e_phnum = (ushort)phnum;
                ehdr.e_phoff = Elf32_Ehdr.EHDRSIZE;
            }

            /* align to 4 */
            file_offset = (file_offset + 3) & -4;

            switch (file_type)
            {
                default:
                case OUTPUTTYPE.TP_OUTPUT_EXE:
                    ehdr.e_type = ETYPE.ET_EXEC;
                    ehdr.e_entry = get_elf_sym_addr(tp, "_start", 1);
                    break;
                case OUTPUTTYPE.TP_OUTPUT_DLL:
                    ehdr.e_type = ETYPE.ET_DYN;
                    ehdr.e_entry = textSection.sh_addr; /* XXX: is it correct ? */
                    break;
                case OUTPUTTYPE.TP_OUTPUT_OBJ:
                    ehdr.e_type = ETYPE.ET_REL;
                    ehdr.e_entry = 0;
                    break;
            }

            ehdr.e_machine = EMACHINE.EM_386;
            ehdr.e_version = EVERSION.EV_CURRENT;
            ehdr.e_shoff = (ushort)file_offset;
            ehdr.e_ehsize = Elf32_Ehdr.EHDRSIZE;
            ehdr.e_shentsize = SHDRSIZE;
            ehdr.e_shnum = shnum;
            ehdr.e_shstrndx = (ushort)(shnum - 1);

            //write out obj header
            ehdr.writeOut(f);

            //write out program hdr entries
            for (i = 0; i < phnum; i++)
            {
                phdr[i].writeOut(f);
            }

            offset = Elf32_Ehdr.EHDRSIZE + (phnum * Elf32_Phdr.PHDRSIZE);

            //write section data
            sort_syms(tp, symtab_section);
            for (i = 1; i < tp.nb_sections; i++)
            {
                s = tp.sections[sec_order[i]];
                if (s.sh_type != SECTIONTYPE.SHT_NOBITS)
                {
                    while (offset < s.sh_offset)
                    {
                        f.WriteByte(0);
                        offset++;
                    }
                    size = s.sh_size;
                    if (size != 0)
                        f.Write(s.data, 0, size);
                    offset += size;
                }
            }

            /* output section headers */
            while (offset < ehdr.e_shoff)
            {
                f.WriteByte(0);
                offset++;
            }

            for (i = 0; i < tp.nb_sections; i++)
            {
                s = tp.sections[i];
                if (s != null)
                {
                    s.writeOut(f);
                }
                else
                {
                    f.Write(EMPTYSECHDR, 0, EMPTYSECHDR.Length);
                }
            }
        }

        private void writeOut(FileStream f)
        {
            byte[] data = new byte[SHDRSIZE];
            for (int i = 0; i < SHDRSIZE; i++)
            {
                data[i] = 0;
            }
            BitConverter.GetBytes(sh_name).CopyTo(data, 0);
            BitConverter.GetBytes((int)sh_type).CopyTo(data, 4);
            BitConverter.GetBytes((int)sh_flags).CopyTo(data, 8);
            BitConverter.GetBytes(sh_addr).CopyTo(data, 12);
            BitConverter.GetBytes(sh_offset).CopyTo(data, 16);
            BitConverter.GetBytes(sh_size).CopyTo(data, 20);
            if (link != null)
            {
                BitConverter.GetBytes((int)link.sh_num).CopyTo(data, 24);
            }
            BitConverter.GetBytes(sh_info).CopyTo(data, 28);
            BitConverter.GetBytes(sh_addralign).CopyTo(data, 32);
            BitConverter.GetBytes(sh_entsize).CopyTo(data, 36);

            f.Write(data, 0, SHDRSIZE);
        }

        /* Write an elf, coff or "binary" file */
        public static int tcc_write_elf_file(TidePool tp, string filename, int phnum, Elf32_Phdr[] phdr, int file_offset, int[] sec_order)
        {
            OUTPUTTYPE file_type;
            FileStream f = null;

            file_type = tp.output_type;

            try
            {
                f = File.Open(filename, FileMode.Create, FileAccess.Write);
            }
            catch (Exception e)
            {

                tp.tp_error_noabort("could not write '{0}'", filename);
                return -1;
            }
            if (tp.verbose != 0)
                Console.Out.WriteLine("<- {0}\n", filename);

            //#ifdef TCC_TARGET_COFF
            //    if (s1->output_format == TCC_OUTPUT_FORMAT_COFF)
            //        tcc_output_coff(s1, f);
            //    else
            //#endif
            if (tp.output_format == OUTPUTFORMAT.TP_OUTPUT_FORMAT_ELF)
                tcc_output_elf(tp, f, phnum, phdr, file_offset, sec_order);
            else
                tcc_output_binary(tp, f, sec_order);
            f.Close();

            return 0;
        }

        public void tidy_section_headers()
        {
        }

        /* Output an elf, coff or binary file */
        /* XXX: suppress unneeded sections */
        public static int elf_output_file(TidePool tp, string filename)
        {
            int i;
            int ret;
            int phnum = 0;
            int shnum = 0;
            OUTPUTTYPE file_type;
            int file_offset = 0;
            int[] sec_order;

            dyn_inf dyninf = null;
            Elf32_Phdr[] phdr;
            Elf32_Sym sym = null;

            Section strsec;
            Section interp;
            Section dynamic;
            Section dynstr;

            int textrel;

            file_type = tp.output_type;
            tp.nb_errors = 0;
            ret = -1;
            phdr = null;
            sec_order = null;
            interp = dynamic = dynstr = null; /* avoid warning */
            textrel = 0;

            if (file_type != OUTPUTTYPE.TP_OUTPUT_OBJ)
            {
                /* if linking, also link in runtime libraries (libc, libgcc, etc.) */
                //        tcc_add_runtime(s1);
                //        resolve_common_syms(s1);

                //        if (!s1->static_link) {
                //            if (file_type == TCC_OUTPUT_EXE) {
                //                char *ptr;
                //                /* allow override the dynamic loader */
                //                const char *elfint = getenv("LD_SO");
                //                if (elfint == NULL)
                //                    elfint = DEFAULT_ELFINTERP(s1);
                //                /* add interpreter section only if executable */
                //                interp = new_section(s1, ".interp", SHT_PROGBITS, SHF_ALLOC);
                //                interp->sh_addralign = 1;
                //                ptr = section_ptr_add(interp, 1 + strlen(elfint));
                //                strcpy(ptr, elfint);
                //            }

                //            /* add dynamic symbol table */
                //            s1->dynsym = new_symtab(s1, ".dynsym", SHT_DYNSYM, SHF_ALLOC,
                //                ".dynstr",
                //                ".hash", SHF_ALLOC);
                //            dynstr = s1->dynsym->link;

                //            /* add dynamic section */
                //            dynamic = new_section(s1, ".dynamic", SHT_DYNAMIC,
                //                SHF_ALLOC | SHF_WRITE);
                //            dynamic->link = dynstr;
                //            dynamic->sh_entsize = sizeof(ElfW(Dyn));

                //            build_got(s1);

                //            if (file_type == TCC_OUTPUT_EXE) {
                //                bind_exe_dynsyms(s1);
                //                if (s1->nb_errors)
                //                    return ret;
                //                bind_libs_dynsyms(s1);
                //            } else {
                //                /* shared library case: simply export all global symbols */
                //                export_global_syms(s1);
                //            }
                //        }
                //        build_got_entries(s1);
            }

            /* we add a section for symbols */
            strsec = new Section(tp, ".shstrtab", SECTIONTYPE.SHT_STRTAB, SECTIONFLAGS.SHF_NONE);
            strsec.put_elf_str("");

            /* Allocate strings for section names */
            textrel = alloc_sec_names(tp, file_type, strsec);

            if (dynamic != null)
            {
                //        /* add a list of needed dlls */
                //        for(i = 0; i < s1->nb_loaded_dlls; i++) {
                //            DLLReference *dllref = s1->loaded_dlls[i];
                //            if (dllref->level == 0)
                //                put_dt(dynamic, DT_NEEDED, put_elf_str(dynstr, dllref->name));
                //        }

                //        if (s1->rpath)
                //            put_dt(dynamic, s1->enable_new_dtags ? DT_RUNPATH : DT_RPATH,
                //            put_elf_str(dynstr, s1->rpath));

                //        if (file_type == TCC_OUTPUT_DLL) {
                //            if (s1->soname)
                //                put_dt(dynamic, DT_SONAME, put_elf_str(dynstr, s1->soname));
                //            /* XXX: currently, since we do not handle PIC code, we
                //            must relocate the readonly segments */
                //            if (textrel)
                //                put_dt(dynamic, DT_TEXTREL, 0);
                //        }

                //        if (s1->symbolic)
                //            put_dt(dynamic, DT_SYMBOLIC, 0);

                //        dyninf.dynamic = dynamic;
                //        dyninf.dynstr = dynstr;
                //        /* remember offset and reserve space for 2nd call below */
                //        dyninf.data_offset = dynamic->data_offset;
                //        fill_dynamic(s1, &dyninf);
                //        dynamic->sh_size = dynamic->data_offset;
                //        dynstr->sh_size = dynstr->data_offset;
            }

            /* compute number of program headers */
            if (file_type == OUTPUTTYPE.TP_OUTPUT_OBJ)
                phnum = 0;
            else if (file_type == OUTPUTTYPE.TP_OUTPUT_DLL)
                phnum = 3;
            else if (tp.static_link != 0)
                phnum = 2;
            else
                phnum = 5;

            /* allocate program segment headers */
            phdr = new Elf32_Phdr[phnum];

            /* compute number of sections */
            shnum = tp.nb_sections;

            /* this array is used to reorder sections in the output file */
            sec_order = new int[shnum];
            sec_order[0] = 0;

            /* compute section to program header mapping */
            file_offset = layout_sections(tp, phdr, phnum, interp, strsec, dyninf, sec_order);

            /* Fill remaining program header and finalize relocation related to dynamic linking. */
            if (file_type != OUTPUTTYPE.TP_OUTPUT_OBJ)
            {
                //        fill_unloadable_phdr(phdr, phnum, interp, dynamic);
                //        if (dynamic) {
                //            dynamic->data_offset = dyninf.data_offset;
                //            fill_dynamic(s1, &dyninf);

                //            /* put in GOT the dynamic section address and relocate PLT */
                //            write32le(s1->got->data, dynamic->sh_addr);
                //            if (file_type == TCC_OUTPUT_EXE
                //                || (RELOCATE_DLLPLT && file_type == TCC_OUTPUT_DLL))
                //                relocate_plt(s1);

                //            /* relocate symbols in .dynsym now that final addresses are known */
                //            for_each_elem(s1->dynsym, 1, sym, ElfW(Sym)) {
                //                if (sym->st_shndx != SHN_UNDEF && sym->st_shndx < SHN_LORESERVE) {
                //                    /* do symbol relocation */
                //                    sym->st_value += s1->sections[sym->st_shndx]->sh_addr;
                //                }
                //            }
                //        }

                /* if building executable or DLL, then relocate each section except the GOT which is already relocated */
                //        ret = final_sections_reloc(s1);
                //        if (ret)
                //            return ret;
                //        tidy_section_headers(s1, sec_order);

                /* Perform relocation to GOT or PLT entries */
                //        if (file_type == TCC_OUTPUT_EXE && s1->static_link)
                //            fill_got(s1);
                //        else if (s1->got)
                //            fill_local_got_entries(s1);
            }

            /* Create the ELF file with name 'filename' */
            ret = tcc_write_elf_file(tp, filename, phnum, phdr, file_offset, sec_order);
            tp.nb_sections = shnum;

            return ret;
        }

        public static int tp_output_file(TidePool tp, string filename)
        {
            int ret = 0;

            if (tp.output_type != OUTPUTTYPE.TP_OUTPUT_OBJ)
            {
                ret = Win32PE.pe_output_file(tp, filename);
            }
            else
            {
                ret = elf_output_file(tp, filename);
            }

            return ret;
        }

        public void load_data() { }
        public void tcc_object_type() { }
        public void tcc_load_object_file() { }
        public void get_be32() { }
        public void get_be64() { }
        public void tcc_load_alacarte() { }
        public void tcc_load_archive() { }
    }

    //-------------------------------------------------------------------------

    //from tcc.h
    public enum OUTPUTFORMAT
    {
        TP_OUTPUT_FORMAT_ELF = 0,       /* default output format: ELF */
        TP_OUTPUT_FORMAT_BINARY = 1,    /* binary image output */
        TP_OUTPUT_FORMAT_COFF = 2       /* COFF */
    }

    /* extra symbol attributes (not in symbol table) */
    public class sym_attr
    {
        int got_offset;
        int plt_offset;
        int plt_sym;
        int dyn_index;
    }

    //-------------------------------------------------------------------------

    //from elf.h - structs used by ELF object files

    /* The ELF file header.  This appears at the start of every ELF file.  */
    public class Elf32_Ehdr
    {
        public const int EHDRSIZE = 52;

        public byte[] e_ident;	    /* Magic number and other info */
        public ETYPE e_type;		/* Object file type */
        public EMACHINE e_machine;	/* Architecture */
        public EVERSION e_version;	/* Object file version */
        public uint e_entry;		    /* Entry point virtual address */
        public uint e_phoff;		    /* Program header table file offset */
        public uint e_shoff;		    /* Section header table file offset */
        public uint e_flags;		    /* Processor-specific flags */
        public ushort e_ehsize;		    /* ELF header size in bytes */
        public ushort e_phentsize;	    /* Program header table entry size */
        public ushort e_phnum;		    /* Program header table entry count */
        public ushort e_shentsize;	    /* Section header table entry size */
        public ushort e_shnum;		    /* Section header table entry count */
        public ushort e_shstrndx;		/* Section header string table index */

        public Elf32_Ehdr()
        {
            e_ident = new byte[16];

            /* fill header */
            e_ident[0] = 0x7f;
            e_ident[1] = (byte)'E';
            e_ident[2] = (byte)'L';
            e_ident[3] = (byte)'F';
            e_ident[4] = 1;                 /* 32-bit objects */
            e_ident[5] = 1;                 /* 2's complement, little endian */
            e_ident[6] = 1;                 // original version of ELF. 
            for (int i = 7; i < 16; i++)
            {
                e_ident[i] = 0;
            }
            e_entry = 0;
            e_phoff = 0;
            e_shoff = 0;
            e_flags = 0;
            e_ehsize = EHDRSIZE;
            e_phentsize = 0;
            e_phnum = 0;
            e_shentsize = 0;
            e_shnum = 0;
            e_shstrndx = 0;
        }

        public void writeOut(FileStream f)
        {
            byte[] data = new byte[EHDRSIZE];
            e_ident.CopyTo(data, 0);
            BitConverter.GetBytes((ushort)e_type).CopyTo(data, 16);
            BitConverter.GetBytes((ushort)e_machine).CopyTo(data, 18);
            BitConverter.GetBytes((uint)e_version).CopyTo(data, 20);
            BitConverter.GetBytes(e_entry).CopyTo(data, 24);
            BitConverter.GetBytes(e_phoff).CopyTo(data, 28);
            BitConverter.GetBytes(e_shoff).CopyTo(data, 32);
            BitConverter.GetBytes(e_flags).CopyTo(data, 36);
            BitConverter.GetBytes(e_ehsize).CopyTo(data, 40);
            BitConverter.GetBytes(e_phentsize).CopyTo(data, 42);
            BitConverter.GetBytes(e_phnum).CopyTo(data, 44);
            BitConverter.GetBytes(e_shentsize).CopyTo(data, 46);
            BitConverter.GetBytes(e_shnum).CopyTo(data, 48);
            BitConverter.GetBytes(e_shstrndx).CopyTo(data, 50);
            f.Write(data, 0, EHDRSIZE);
        }
    }

    /* Legal values for e_type (object file type).  */
    public enum ETYPE : ushort
    {
        ET_NONE = 0,		    /* No file type */
        ET_REL = 1,		        /* Relocatable file */
        ET_EXEC = 2,		    /* Executable file */
        ET_DYN = 3,		        /* Shared object file */
        ET_CORE = 4,		    /* Core file */
        ET_NUM = 5,		        /* Number of defined types */
        ET_LOOS = 0xfe00,	    /* OS-specific range start */
        ET_HIOS = 0xfeff,	    /* OS-specific range end */
        ET_LOPROC = 0xff00,	    /* Processor-specific range start */
        ET_HIPROC = 0xffff		/* Processor-specific range end */
    }

    /* Legal values for e_machine (architecture).  */
    public enum EMACHINE : ushort
    {
        EM_NONE = 0,		    /* No machine */
        EM_M32 = 1,	            /* AT&T WE 32100 */
        EM_SPARC = 2,	        /* SUN SPARC */
        EM_386 = 3,	            /* Intel 80386 */
        EM_68K = 4,	            /* Motorola m68k family */
        EM_88K = 5,	            /* Motorola m88k family */
        EM_860 = 7,	            /* Intel 80860 */
        EM_MIPS = 8,	        /* MIPS R3000 big-endian */
        EM_S370 = 9,	        /* IBM System/370 */
        EM_MIPS_RS3_LE = 10,	/* MIPS R3000 little-endian */

        EM_PARISC = 15,	        /* HPPA */
        EM_VPP500 = 17,	        /* Fujitsu VPP500 */
        EM_SPARC32PLUS = 18,	/* Sun's "v8plus" */
        EM_960 = 19,	        /* Intel 80960 */
        EM_PPC = 20,	        /* PowerPC */
        EM_PPC64 = 21,	        /* PowerPC 64-bit */
        EM_S390 = 22,	        /* IBM S390 */

        EM_V800 = 36,	        /* NEC V800 series */
        EM_FR20 = 37,	        /* Fujitsu FR20 */
        EM_RH32 = 38,	        /* TRW RH-32 */
        EM_RCE = 39,	        /* Motorola RCE */
        EM_ARM = 40,	        /* ARM */
        EM_FAKE_ALPHA = 41,		/* Digital Alpha */
        EM_SH = 42,	            /* Hitachi SH */
        EM_SPARCV9 = 43,	    /* SPARC v9 64-bit */
        EM_TRICORE = 44,	    /* Siemens Tricore */
        EM_ARC = 45,	        /* Argonaut RISC Core */
        EM_H8_300 = 46,	        /* Hitachi H8/300 */
        EM_H8_300H = 47,	    /* Hitachi H8/300H */
        EM_H8S = 48,	        /* Hitachi H8S */
        EM_H8_500 = 49,	        /* Hitachi H8/500 */
        EM_IA_64 = 50,	        /* Intel Merced */
        EM_MIPS_X = 51,	        /* Stanford MIPS-X */
        EM_COLDFIRE = 52,		/* Motorola Coldfire */
        EM_68HC12 = 53,		    /* Motorola M68HC12 */
        EM_MMA = 54,	        /* Fujitsu MMA Multimedia Accelerator*/
        EM_PCP = 55,	        /* Siemens PCP */
        EM_NCPU = 56,	        /* Sony nCPU embedded RISC */
        EM_NDR1 = 57,           /* Denso NDR1 microprocessor */
        EM_STARCORE = 58,	    /* Motorola Start*Core processor */
        EM_ME16 = 59,	        /* Toyota ME16 processor */
        EM_ST100 = 60,	        /* STMicroelectronic ST100 processor */
        EM_TINYJ = 61,	        /* Advanced Logic Corp. Tinyj emb.fam*/
        EM_X86_64 = 62,	        /* AMD x86-64 architecture */
        EM_PDSP = 63,		    /* Sony DSP Processor */

        EM_FX66 = 66,	        /* Siemens FX66 microcontroller */
        EM_ST9PLUS = 67,	    /* STMicroelectronics ST9+ 8/16 mc */
        EM_ST7 = 68,	        /* STMicroelectronics ST7 8 bit mc */
        EM_68HC16 = 69,		    /* Motorola MC68HC16 microcontroller */
        EM_68HC11 = 70,		    /* Motorola MC68HC11 microcontroller */
        EM_68HC08 = 71,	        /* Motorola MC68HC08 microcontroller */
        EM_68HC05 = 72,	        /* Motorola MC68HC05 microcontroller */
        EM_SVX = 73,	        /* Silicon Graphics SVx */
        EM_ST19 = 74,	        /* STMicroelectronics ST19 8 bit mc */
        EM_VAX = 75,	        /* Digital VAX */
        EM_CRIS = 76,	        /* Axis Communications 32-bit embedded processor */
        EM_JAVELIN = 77,	    /* Infineon Technologies 32-bit embedded processor */
        EM_FIREPATH = 78,		/* Element 14 64-bit DSP Processor */
        EM_ZSP = 79,	        /* LSI Logic 16-bit DSP Processor */
        EM_MMIX = 80,	        /* Donald Knuth's educational 64-bit processor */
        EM_HUANY = 81,	        /* Harvard University machine-independent object files */
        EM_PRISM = 82,	        /* SiTera Prism */
        EM_AVR = 83,	        /* Atmel AVR 8-bit microcontroller */
        EM_FR30 = 84,	        /* Fujitsu FR30 */
        EM_D10V = 85,	        /* Mitsubishi D10V */
        EM_D30V = 86,		    /* Mitsubishi D30V */
        EM_V850 = 87,	        /* NEC v850 */
        EM_M32R = 88,	        /* Mitsubishi M32R */
        EM_MN10300 = 89,	    /* Matsushita MN10300 */
        EM_MN10200 = 90,	    /* Matsushita MN10200 */
        EM_PJ = 91,	            /* picoJava */
        EM_OPENRISC = 92,		/* OpenRISC 32-bit embedded processor */
        EM_ARC_A5 = 93,	        /* ARC Cores Tangent-A5 */
        EM_XTENSA = 94,	        /* Tensilica Xtensa Architecture */
        EM_AARCH64 = 183,	    /* ARM AARCH64 */
        EM_TILEPRO = 188,	    /* Tilera TILEPro */
        EM_TILEGX = 191,		/* Tilera TILE-Gx */
        EM_NUM = 192,

        /* If it is necessary to assign new unofficial EM_* values, please
           pick large random numbers (0x8523, 0xa7f2, etc.) to minimize the
           chances of collision with official or non-GNU unofficial values.  */
        EM_ALPHA = 0x9026,
        EM_C60 = 0x9c60
    }

    /* Legal values for e_version (version).  */
    public enum EVERSION : uint
    {
        EV_NONE = 0,		/* Invalid ELF version */
        EV_CURRENT = 1,		/* Current version */
        EV_NUM = 2
    }

    //-------------------------------------------------------------------------

    /* Section header.  */
    //public class Elf32_Shdr
    //{
    //    public const int SHDRSIZE = 40;

    //    public int sh_name;		        /* Section name (string tbl index) */
    //    public SECTIONTYPE sh_type;	    /* Section type */
    //    public SECTIONFLAGS sh_flags;	/* Section flags */
    //    public int sh_addr;		        /* Section virtual addr at execution */
    //    public int sh_offset;		    /* Section file offset */
    //    public int sh_size;		        /* Section size in bytes */
    //    public int sh_link;		        /* Link to another section */
    //    public int sh_info;		        /* Additional section information */
    //    public int sh_addralign;	    /* Section alignment */
    //    public int sh_entsize;		    /* Entry size if section holds table */
    //}

    /* Special section indices.  */
    public enum SECTIONIDX
    {
        SHN_UNDEF = 0,	                /* Undefined section */
        SHN_LORESERVE = 0xff00,	        /* Start of reserved indices */
        SHN_LOPROC = 0xff00,		    /* Start of processor-specific */
        SHN_BEFORE = 0xff00,	        /* Order section before all others (Solaris).  */
        SHN_AFTER = 0xff01,	            /* Order section after all others (Solaris).  */
        SHN_HIPROC = 0xff1f,		    /* End of processor-specific */
        SHN_LOOS = 0xff20,		        /* Start of OS-specific */
        SHN_HIOS = 0xff3f,		        /* End of OS-specific */
        SHN_ABS = 0xfff1,	            /* Associated symbol is absolute */
        SHN_COMMON = 0xfff2,	        /* Associated symbol is common */
        SHN_XINDEX = 0xffff,		    /* Index is in extra table.  */
        SHN_HIRESERVE = 0xffff		    /* End of reserved indices */
    }

    /* Legal values for sh_type (section type).  */
    public enum SECTIONTYPE : uint
    {
        SHT_NULL = 0,		        /* Section header table entry unused */
        SHT_PROGBITS = 1,		    /* Program data */
        SHT_SYMTAB = 2,		        /* Symbol table */
        SHT_STRTAB = 3,		        /* String table */
        SHT_RELA = 4,		        /* Relocation entries with addends */
        SHT_HASH = 5,		        /* Symbol hash table */
        SHT_DYNAMIC = 6,		    /* Dynamic linking information */
        SHT_NOTE = 7,		        /* Notes */
        SHT_NOBITS = 8,		        /* Program space with no data (bss) */
        SHT_REL = 9,		        /* Relocation entries, no addends */
        SHT_SHLIB = 10,		        /* Reserved */
        SHT_DYNSYM = 11,		    /* Dynamic linker symbol table */
        SHT_INIT_ARRAY = 14,		/* Array of constructors */
        SHT_FINI_ARRAY = 15,		/* Array of destructors */
        SHT_PREINIT_ARRAY = 16,		/* Array of pre-constructors */
        SHT_GROUP = 17,		        /* Section group */
        SHT_SYMTAB_SHNDX = 18,	    /* Extended section indices */
        SHT_NUM = 19,	            /* Number of defined types.  */
        SHT_LOOS = 0x60000000,	    /* Start OS-specific.  */
        SHT_GNU_ATTRIBUTES = 0x6ffffff5,	/* Object attributes.  */
        SHT_GNU_HASH = 0x6ffffff6,	/* GNU-style hash table.  */
        SHT_GNU_LIBLIST = 0x6ffffff7,	/* Prelink library list */
        SHT_CHECKSUM = 0x6ffffff8,	/* Checksum for DSO content.  */
        SHT_LOSUNW = 0x6ffffffa,	/* Sun-specific low bound.  */
        SHT_SUNW_move = 0x6ffffffa,
        SHT_SUNW_COMDAT = 0x6ffffffb,
        SHT_SUNW_syminfo = 0x6ffffffc,
        SHT_GNU_verdef = 0x6ffffffd,	/* Version definition section.  */
        SHT_GNU_verneed = 0x6ffffffe,	/* Version needs section.  */
        SHT_GNU_versym = 0x6fffffff,	/* Version symbol table.  */
        SHT_HISUNW = 0x6fffffff,	/* Sun-specific high bound.  */
        SHT_HIOS = 0x6fffffff,	    /* End OS-specific type */
        SHT_LOPROC = 0x70000000,	/* Start of processor-specific */
        SHT_HIPROC = 0x7fffffff,	/* End of processor-specific */
        SHT_LOUSER = 0x80000000,	/* Start of application-specific */
        SHT_HIUSER = 0x8fffffff,	/* End of application-specific */
    }

    /* Legal values for sh_flags (section flags).  */
    public enum SECTIONFLAGS : uint
    {
        SHF_NONE = 0,
        SHF_WRITE = (1 << 0),	/* Writable */
        SHF_ALLOC = (1 << 1),	/* Occupies memory during execution */
        SHF_EXECINSTR = (1 << 2),	/* Executable */
        SHF_MERGE = (1 << 4),	/* Might be merged */
        SHF_STRINGS = (1 << 5),	/* Contains nul-terminated strings */
        SHF_INFO_LINK = (1 << 6),	/* `sh_info' contains SHT index */
        SHF_LINK_ORDER = (1 << 7),	/* Preserve order after combining */
        SHF_OS_NONCONFORMING = (1 << 8),	/* Non-standard OS specific handling  required */
        SHF_GROUP = (1 << 9),	/* Section is member of a group.  */
        SHF_TLS = (1 << 10),	/* Section hold thread-local data.  */
        SHF_COMPRESSED = (1 << 11),	/* Section with compressed data. */
        SHF_MASKOS = 0x0ff00000,	/* OS-specific.  */
        SHF_MASKPROC = 0xf0000000,	/* Processor-specific */
        SHF_ORDERED = 0x40000000,	/* Special ordering requirement (Solaris).  */
        SHF_EXCLUDE = 0x80000000,	/* Section is excluded unless  referenced or allocated (Solaris).*/
        SHF_PRIVATE = 0x80000000,   /* special flag to indicate that the section should not be linked to the other ones */
        SHF_DYNSYM = 0x40000000     /* section is dynsymtab_section */
    }

    //-------------------------------------------------------------------------

    /* Symbol table entry.  */
    public class Elf32_Sym
    {
        public const int SYMENTSIZE = 16;

        public int st_name;		        /* Symbol name (string tbl index) */
        public int st_value;		    /* Symbol value */
        public int st_size;		        /* Symbol size */
        public int st_info;		        /* Symbol type and binding */
        public int st_other;		    /* Symbol visibility */
        public int st_shndx;		    /* Section index */

        public byte[] data;
        public int symidx;

        public void storeData(byte[] _data, int _symidx)
        {
            data = _data;
            symidx = _symidx;
            BitConverter.GetBytes(st_name).CopyTo(data, symidx);
            BitConverter.GetBytes(st_value).CopyTo(data, symidx + 4);
            BitConverter.GetBytes(st_size).CopyTo(data, symidx + 8);
            BitConverter.GetBytes((byte)st_info).CopyTo(data, symidx + 12);
            BitConverter.GetBytes((byte)st_other).CopyTo(data, symidx + 13);
            BitConverter.GetBytes((ushort)st_shndx).CopyTo(data, symidx + 14);
        }

        internal void setSize(int size)
        {
            st_size = size;
            BitConverter.GetBytes(st_size).CopyTo(data, symidx + 8);
        }
    }

    /* Legal values for ST_BIND subfield of st_info (symbol binding).  */
    public enum STBIND
    {
        STB_LOCAL = 0,		/* Local symbol */
        STB_GLOBAL = 1,	/* Global symbol */
        STB_WEAK = 2,	/* Weak symbol */
        STB_NUM = 3,	/* Number of defined types.  */
        STB_LOOS = 10,	/* Start of OS-specific */
        STB_GNU_UNIQUE = 10,		/* Unique symbol.  */
        STB_HIOS = 12,	/* End of OS-specific */
        STB_LOPROC = 13,	/* Start of processor-specific */
        STB_HIPROC = 15		/* End of processor-specific */
    }

    /* Legal values for ST_TYPE subfield of st_info (symbol type).  */
    public enum STTYPE
    {
        STT_NOTYPE = 0,		/* Symbol type is unspecified */
        STT_OBJECT = 1,	/* Symbol is a data object */
        STT_FUNC = 2,		/* Symbol is a code object */
        STT_SECTION = 3,		/* Symbol associated with a section */
        STT_FILE = 4,	/* Symbol's name is file name */
        STT_COMMON = 5,	/* Symbol is a common data object */
        STT_TLS = 6,		/* Symbol is thread-local data object*/
        STT_NUM = 7,		/* Number of defined types.  */
        STT_LOOS = 10,	/* Start of OS-specific */
        STT_GNU_IFUNC = 10,	/* Symbol is indirect code object */
        STT_HIOS = 12,	/* End of OS-specific */
        STT_LOPROC = 13,	/* Start of processor-specific */
        STT_HIPROC = 15,	/* End of processor-specific */
    }

    //-----------------------------------------------------------------------------

    /* Program segment header.  */
    public class Elf32_Phdr
    {
        public const int PHDRSIZE = 32;

        public int p_type;		    /* Segment type */
        public int p_offset;		/* Segment file offset */
        public int p_vaddr;		    /* Segment virtual address */
        public int p_paddr;		    /* Segment physical address */
        public int p_filesz;		/* Segment size in file */
        public int p_memsz;		    /* Segment size in memory */
        public int p_flags;		    /* Segment flags */
        public int p_align;		    /* Segment alignment */

        public void writeOut(FileStream f)
        {
            //throw new NotImplementedException();
        }
    }

    //-------------------------------------------------------------------------

    public enum I386RELOCS
    {
        R_386_NONE = 0,	/* No reloc */
        R_386_32 = 1,	/* Direct 32 bit  */
        R_386_PC32 = 2,	/* PC relative 32 bit */
        R_386_GOT32 = 3,	/* 32 bit GOT entry */
        R_386_PLT32 = 4,	/* 32 bit PLT address */
        R_386_COPY = 5,	/* Copy symbol at runtime */
        R_386_GLOB_DAT = 6,		/* Create GOT entry */
        R_386_JMP_SLOT = 7,	/* Create PLT entry */
        R_386_RELATIVE = 8,	/* Adjust by program base */
        R_386_GOTOFF = 9,	/* 32 bit offset to GOT */
        R_386_GOTPC = 10,	/* 32 bit PC relative offset to GOT */
        R_386_32PLT = 11,
        R_386_TLS_TPOFF = 14,		/* Offset in static TLS block */
        R_386_TLS_IE = 15,	/* Address of GOT entry for static TLS block offset */
        R_386_TLS_GOTIE = 16,	/* GOT entry for static TLS block offset */
        R_386_TLS_LE = 17,	/* Offset relative to static TLS block */
        R_386_TLS_GD = 18,	/* Direct 32 bit for GNU version of general dynamic thread local data */
        R_386_TLS_LDM = 19,	/* Direct 32 bit for GNU version of local dynamic thread local data in LE code */
        R_386_16 = 20,
        R_386_PC16 = 21,
        R_386_8 = 22,
        R_386_PC8 = 23,
        R_386_TLS_GD_32 = 24,		/* Direct 32 bit for general dynamic thread local data */
        R_386_TLS_GD_PUSH = 25,	/* Tag for pushl in GD TLS code */
        R_386_TLS_GD_CALL = 26,	/* Relocation for call to __tls_get_addr() */
        R_386_TLS_GD_POP = 27,	/* Tag for popl in GD TLS code */
        R_386_TLS_LDM_32 = 28,	/* Direct 32 bit for local dynamic thread local data in LE code */
        R_386_TLS_LDM_PUSH = 29,	/* Tag for pushl in LDM TLS code */
        R_386_TLS_LDM_CALL = 30,	/* Relocation for call to __tls_get_addr() in LDM code */
        R_386_TLS_LDM_POP = 31,	/* Tag for popl in LDM TLS code */
        R_386_TLS_LDO_32 = 32,	/* Offset relative to TLS block */
        R_386_TLS_IE_32 = 33,	/* GOT entry for negated static TLS block offset */
        R_386_TLS_LE_32 = 34,	/* Negated offset relative to static TLS block */
        R_386_TLS_DTPMOD32 = 35,	/* ID of module containing symbol */
        R_386_TLS_DTPOFF32 = 36,	/* Offset in TLS block */
        R_386_TLS_TPOFF32 = 37,	/* Negated offset in static TLS block */

        /* 38? */
        R_386_TLS_GOTDESC = 39,	/* GOT offset for TLS descriptor.  */
        R_386_TLS_DESC_CALL = 40,	/* Marker of call through TLS descriptor for relaxation.  */
        R_386_TLS_DESC = 41,	/* TLS descriptor containing pointer to code and to argument, returning the TLS offset for the symbol.  */
        R_386_IRELATIVE = 42,	/* Adjust indirectly by program base */
        R_386_GOT32X = 43,	/* 32 bit GOT entry, relaxable */

        /* Keep this the last entry.  */
        R_386_NUM = 44
    }

    //-------------------------------------------------------------------------

    //from tccelf.c

    /* Info to be copied in dynamic section */
    public class dyn_inf
    {
        Section dynamic;
        Section dynstr;
        int data_offset;
        int rel_addr;
        int rel_size;
    };

}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");