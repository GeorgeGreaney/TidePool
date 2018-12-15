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
        public uint sh_addr;                 /* address at which the section is relocated */
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
        }

        //tccelf_new
        public static void initSection(TidePool tp)
        {
            /* no section zero */
            //    dynarray_add(&s->sections, &s->nb_sections, NULL);

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
        public void free_section() { }
        public void tccelf_delete() { }
        public void tccelf_begin_file() { }
        public void tccelf_end_file() { }

        public static Section new_symtab(TidePool tp, string symtab_name, SECTIONTYPE sh_type, SECTIONFLAGS sh_flags,
                            string strtab_name, string hash_name, SECTIONFLAGS hash_sh_flags)
        {
            Section symtab;
            Section strtab;
            Section hash;
            int ptr;
            int nb_buckets;

            symtab = new Section(tp, symtab_name, sh_type, sh_flags);
            //symtab->sh_entsize = sizeof(ElfW(Sym));
            //strtab = new_section(s1, strtab_name, SHT_STRTAB, sh_flags);
            //put_elf_str(strtab, "");
            //symtab->link = strtab;
            //put_elf_sym(symtab, 0, 0, 0, 0, 0, NULL);

            //nb_buckets = 1;

            //hash = new_section(s1, hash_name, SHT_HASH, hash_sh_flags);
            //hash->sh_entsize = sizeof(int);
            //symtab->hash = hash;
            //hash->link = symtab;

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

        public void elf_hash() { }
        public void rebuild_hash() { }
        public void put_elf_sym() { }
        public void find_elf_sym() { }

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
        public void set_elf_sym() { }
        public void put_elf_reloca() { }
        public void put_elf_reloc() { }
        public void squeeze_multi_relocs() { }
        public void put_stabs() { }
        public void put_stabs_r() { }
        public void put_stabn() { }
        public void put_stabd() { }
        public void get_sym_attr() { }

        /* In an ELF file symbol table, the local symbols must appear below
the global and weak ones. Since TCC cannot sort it while generating
the code, we must do it after. All the relocation tables are also
modified to take into account the symbol table sorting */
        public static void sort_syms(TidePool tp, Section s)
        {
            int old_to_new_syms;
            //ElfW(Sym) *new_syms;
            int nb_syms;
                int i;
            //ElfW(Sym) *p, *q;
            //ElfW_Rel *rel;
            Section sr;
            int type; 
                int sym_index;

            //nb_syms = s->data_offset / sizeof(ElfW(Sym));
            //new_syms = tcc_malloc(nb_syms * sizeof(ElfW(Sym)));
            //old_to_new_syms = tcc_malloc(nb_syms * sizeof(int));

            /* first pass for local symbols */
            //p = (ElfW(Sym) *)s->data;
            //q = new_syms;
            //for(i = 0; i < nb_syms; i++) {
            //    if (ELFW(ST_BIND)(p->st_info) == STB_LOCAL) {
            //        old_to_new_syms[i] = q - new_syms;
            //        *q++ = *p;
            //    }
            //    p++;
            //}

            /* save the number of local symbols in section header */
            //if( s->sh_size )    /* this 'if' makes IDA happy */
            //    s->sh_info = q - new_syms;

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
        public void prepare_dynamic_rel() { }
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
        public void alloc_sec_names() { }
        public void layout_sections() { }
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
            Elf32_Shdr shdr;
            Elf32_Shdr sh;

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
            ehdr.e_shentsize = Elf32_Shdr.SHDRSIZE;
            ehdr.e_shnum = shnum;
            ehdr.e_shstrndx = (ushort)(shnum - 1);

            ehdr.writeOut(f);
            for (i = 0; i < phnum; i++)
            {
                phdr[i].writeOut(f);
            }
            offset = Elf32_Ehdr.EHDRSIZE + (phnum * Elf32_Phdr.PHDRSIZE);

            //write section data
            sort_syms(tp, symtab_section);
            for (i = 1; i < tp.nb_sections; i++)
            {
                //        s = s1->sections[sec_order[i]];
                //        if (s->sh_type != SHT_NOBITS) {
                //            while (offset < s->sh_offset) {
                //                fputc(0, f);
                //                offset++;
                //            }
                //            size = s->sh_size;
                //            if (size)
                //                fwrite(s->data, 1, size, f);
                //            offset += size;
                //        }
            }

            /* output section headers */
            //    while (offset < ehdr.e_shoff) {
            //        fputc(0, f);
            //        offset++;
            //    }

            //    for(i = 0; i < s1->nb_sections; i++) {
            //        sh = &shdr;
            //        memset(sh, 0, sizeof(ElfW(Shdr)));
            //        s = s1->sections[i];
            //        if (s) {
            //            sh->sh_name = s->sh_name;
            //            sh->sh_type = s->sh_type;
            //            sh->sh_flags = s->sh_flags;
            //            sh->sh_entsize = s->sh_entsize;
            //            sh->sh_info = s->sh_info;
            //            if (s->link)
            //                sh->sh_link = s->link->sh_num;
            //            sh->sh_addralign = s->sh_addralign;
            //            sh->sh_addr = s->sh_addr;
            //            sh->sh_offset = s->sh_offset;
            //            sh->sh_size = s->sh_size;
            //        }
            //        fwrite(sh, 1, sizeof(ElfW(Shdr)), f);
            //    }

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

            //    struct dyn_inf dyninf = {0};
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
                //        /* if linking, also link in runtime libraries (libc, libgcc, etc.) */
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
            //    textrel = alloc_sec_names(s1, file_type, strsec);

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
            //    file_offset = layout_sections(s1, phdr, phnum, interp, strsec, &dyninf, sec_order);

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
    public class Elf32_Shdr
    {
        public const int SHDRSIZE = 40;

        public int sh_name;		        /* Section name (string tbl index) */
        public SECTIONTYPE sh_type;	    /* Section type */
        public SECTIONFLAGS sh_flags;	/* Section flags */
        public int sh_addr;		        /* Section virtual addr at execution */
        public int sh_offset;		    /* Section file offset */
        public int sh_size;		        /* Section size in bytes */
        public int sh_link;		        /* Link to another section */
        public int sh_info;		        /* Additional section information */
        public int sh_addralign;	    /* Section alignment */
        public int sh_entsize;		    /* Entry size if section holds table */
    }

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
        public int st_name;		    /* Symbol name (string tbl index) */
        public int st_value;		/* Symbol value */
        public int st_size;		    /* Symbol size */
        public char st_info;		/* Symbol type and binding */
        public char st_other;		/* Symbol visibility */
        public int st_shndx;		/* Section index */
    }

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
}

//Console.Out.WriteLine("There's no sun in the shadow of the wizard");