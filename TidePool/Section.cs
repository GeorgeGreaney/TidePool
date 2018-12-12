using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TidePool
{
    public class Section
    {
        public static Section textSection;
        public static Section dataSection;
        public static Section bssSection;             /* predefined sections */
        public static Section commonSection;
        public static Section curTextSection;        /* current section where function code is generated */

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
          public void tcc_write_elf_file() { }
          public void tidy_section_headers() { }
          public void elf_output_file() { }
          public void tcc_output_file() { }
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
R_386_NONE	=   0	,	/* No reloc */
R_386_32	=   1	,	/* Direct 32 bit  */
R_386_PC32	=   2	,	/* PC relative 32 bit */
R_386_GOT32	=   3	,	/* 32 bit GOT entry */
R_386_PLT32	=   4	,	/* 32 bit PLT address */
R_386_COPY	=   5	,	/* Copy symbol at runtime */
R_386_GLOB_DAT	 =  6,		/* Create GOT entry */
R_386_JMP_SLOT=	   7	,	/* Create PLT entry */
R_386_RELATIVE	=   8	,	/* Adjust by program base */
R_386_GOTOFF	=   9	,	/* 32 bit offset to GOT */
R_386_GOTPC	=   10	,	/* 32 bit PC relative offset to GOT */
R_386_32PLT=	   11,
R_386_TLS_TPOFF	=   14,		/* Offset in static TLS block */
R_386_TLS_IE	=   15	,	/* Address of GOT entry for static TLS block offset */
R_386_TLS_GOTIE	=   16	,	/* GOT entry for static TLS block offset */
R_386_TLS_LE	=   17	,	/* Offset relative to static TLS block */
R_386_TLS_GD=	   18	,	/* Direct 32 bit for GNU version of general dynamic thread local data */
R_386_TLS_LDM	=   19	,	/* Direct 32 bit for GNU version of local dynamic thread local data in LE code */
R_386_16	=   20,
R_386_PC16	=   21,
R_386_8		=   22,
R_386_PC8	=   23,
R_386_TLS_GD_32	 =  24,		/* Direct 32 bit for general dynamic thread local data */
R_386_TLS_GD_PUSH=  25	,	/* Tag for pushl in GD TLS code */
R_386_TLS_GD_CALL=  26	,	/* Relocation for call to __tls_get_addr() */
R_386_TLS_GD_POP =  27	,	/* Tag for popl in GD TLS code */
R_386_TLS_LDM_32 =  28	,	/* Direct 32 bit for local dynamic thread local data in LE code */
R_386_TLS_LDM_PUSH= 29	,	/* Tag for pushl in LDM TLS code */
R_386_TLS_LDM_CALL= 30	,	/* Relocation for call to __tls_get_addr() in LDM code */
R_386_TLS_LDM_POP=  31	,	/* Tag for popl in LDM TLS code */
R_386_TLS_LDO_32 =  32	,	/* Offset relative to TLS block */
R_386_TLS_IE_32	=   33	,	/* GOT entry for negated static TLS block offset */
R_386_TLS_LE_32	=   34	,	/* Negated offset relative to static TLS block */
R_386_TLS_DTPMOD32= 35	,	/* ID of module containing symbol */
R_386_TLS_DTPOFF32= 36	,	/* Offset in TLS block */
R_386_TLS_TPOFF32=  37	,	/* Negated offset in static TLS block */

    /* 38? */
R_386_TLS_GOTDESC = 39	,	/* GOT offset for TLS descriptor.  */
R_386_TLS_DESC_CALL= 40	,	/* Marker of call through TLS descriptor for relaxation.  */
R_386_TLS_DESC  =   41	,	/* TLS descriptor containing pointer to code and to argument, returning the TLS offset for the symbol.  */
R_386_IRELATIVE	=   42	,	/* Adjust indirectly by program base */
R_386_GOT32X    =   43	,	/* 32 bit GOT entry, relaxable */

    /* Keep this the last entry.  */
R_386_NUM	=   44

    }
}
