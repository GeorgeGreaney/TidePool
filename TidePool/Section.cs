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
    public class Section
    {
        /* from tcc.h - section definition */
        public int data_offset;             /* current data offset */
        public byte[] data;                 /* section data */
        public int data_allocated;          /* used for realloc() handling */

        public int sh_name;                /* elf section name (only used during output) */
        public int sh_num;                 /* elf section number */
        public SECTIONTYPE sh_type;        /* elf section type */
        public SECTIONFLAGS sh_flags;      /* elf section flags */
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

        //from tcc.h
        public static Section textSection;
        public static Section dataSection;
        public static Section bssSection;             /* predefined sections */
        public static Section commonSection;
        public static Section curTextSection;        /* current section where function code is generated */

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

            //if (sh_flags & SHF_PRIVATE)
            //{
            //    dynarray_add(&s1->priv_sections, &s1->nb_priv_sections, sec);
            //}
            //else
            //{
            //    sec->sh_num = s1->nb_sections;
            //    dynarray_add(&s1->sections, &s1->nb_sections, sec);
            //}

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
            //    common_section->sh_num = SHN_COMMON;

            /* symbols are always generated for linking stage */
            //    symtab_section = new_symtab(s, ".symtab", SHT_SYMTAB, 0,
            //                                ".strtab",
            //                                ".hashtab", SHF_PRIVATE);
            //    s->symtab = symtab_section;

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
        public void new_symtab() { }

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

        public void section_add() { }
        public void section_ptr_add() { }
        public void section_reserve() { }
        public void find_section() { }
        public void put_elf_str() { }
        public void elf_hash() { }
        public void rebuild_hash() { }
        public void put_elf_sym() { }
        public void find_elf_sym() { }
        public void get_elf_sym_addr() { }
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
        public void sort_syms() { }
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
        public void tcc_output_binary() { }
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
        public void tcc_output_elf() { }

        /* Write an elf, coff or "binary" file */
        public int tcc_write_elf_file(TidePool tp, string filename, int phnum, Elf32_Phdr phdr, int file_offset, int sec_order)
        { 
//            int fd, mode, file_type;
//    FILE *f;

//    file_type = s1->output_type;
//    if (file_type == TCC_OUTPUT_OBJ)
//        mode = 0666;
//    else
//        mode = 0777;
//    unlink(filename);
//    fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, mode);
//    if (fd < 0) {
//        tcc_error_noabort("could not write '%s'", filename);
//        return -1;
//    }
//    f = fdopen(fd, "wb");
//    if (s1->verbose)
//        printf("<- %s\n", filename);

//#ifdef TCC_TARGET_COFF
//    if (s1->output_format == TCC_OUTPUT_FORMAT_COFF)
//        tcc_output_coff(s1, f);
//    else
//#endif
//        if (s1->output_format == TCC_OUTPUT_FORMAT_ELF)
//            tcc_output_elf(s1, f, phnum, phdr, file_offset, sec_order);
//        else
//            tcc_output_binary(s1, f, sec_order);
//    fclose(f);

	return 0;

        }

        public void tidy_section_headers() { }

        public static int elf_output_file(TidePool tp, string filename)
        {
            int i;
            int ret;
            int phnum;
            int shnum;
            OUTPUTTYPE file_type;
            int file_offset;
            int sec_order;

//    struct dyn_inf dyninf = {0};
//    ElfW(Phdr) *phdr;
//    ElfW(Sym) *sym;

            Section strsec = null;
            Section interp = null;
            Section dynamic = null;
            Section dynstr = null;

            int textrel;

    file_type = tp.output_type;
//    s1->nb_errors = 0;
//    ret = -1;
//    phdr = NULL;
//    sec_order = NULL;
//    interp = dynamic = dynstr = NULL; /* avoid warning */
//    textrel = 0;

    if (file_type != OUTPUTTYPE.TP_OUTPUT_OBJ) {
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
//                    goto the_end;
//                bind_libs_dynsyms(s1);
//            } else {
//                /* shared library case: simply export all global symbols */
//                export_global_syms(s1);
//            }
//        }
//        build_got_entries(s1);
    }

    /* we add a section for symbols */
//    strsec = new_section(s1, ".shstrtab", SHT_STRTAB, 0);
//    put_elf_str(strsec, "");

    /* Allocate strings for section names */
//    textrel = alloc_sec_names(s1, file_type, strsec);

    if (dynamic != null) {
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
//    if (file_type == TCC_OUTPUT_OBJ)
//        phnum = 0;
//    else if (file_type == TCC_OUTPUT_DLL)
//        phnum = 3;
//    else if (s1->static_link)
//        phnum = 2;
//    else
//        phnum = 5;

    /* allocate program segment headers */
//    phdr = tcc_mallocz(phnum * sizeof(ElfW(Phdr)));

    /* compute number of sections */
//    shnum = s1->nb_sections;

    /* this array is used to reorder sections in the output file */
//    sec_order = tcc_malloc(sizeof(int) * shnum);
//    sec_order[0] = 0;

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
//            goto the_end;
//        tidy_section_headers(s1, sec_order);

        /* Perform relocation to GOT or PLT entries */
//        if (file_type == TCC_OUTPUT_EXE && s1->static_link)
//            fill_got(s1);
//        else if (s1->got)
//            fill_local_got_entries(s1);
    }

    /* Create the ELF file with name 'filename' */
    ret = tcc_write_elf_file(tp, filename, phnum, phdr, file_offset, sec_order);
//    s1->nb_sections = shnum;
//the_end:
//    tcc_free(sec_order);
//    tcc_free(phdr);
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
