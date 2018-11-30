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


        public Section(TidePool tp, string name, SECTIONTYPE sh_type, SECTIONFLAGS sh_flags)
        {
        }

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
}
