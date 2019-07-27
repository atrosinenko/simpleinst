package freechips.rocketchip.instrumenter

object ElfConstants {
  object Elf64_Ehdr {
    object e_ident {
      val ElfMagic = 0x464c457f // [0x7F, 'E', 'L', 'F']

      val EI_CLASS = 4
      val EI_DATA = 5
      val EI_VERSION = 6

      val ELFCLASS32 = 1
      val ELFCLASS64 = 2

      val ELFDATA2LSB = 1
      val ELFDATA2MSB = 2

      val EV_CURRENT = 1
    }
    val TypeOffset = 16
    val MachineOffset = 18
    val ShOffsetOffset = 40
    val ShentSizeOffset = 58
    val ShNumOffset = 60
    val ShStrndxOffset = 62

    val ET_REL = 1
    val EM_BPF = 247
  }

  object Elf64_Shdr {
    val NameOffset = 0
    val TypeOffset = 4
    val OffsetOffset = 24
    val SizeOffset = 32
    val LinkOffset = 40
    val InfoOffset = 44

    val SHT_NULL = 0
    val SHT_NOBITS = 8
    val SHT_SYMTAB = 2
    val SHT_STRTAB = 3
    val SHT_REL = 9
    val SHT_RELA = 4

    val SHN_COMMON = 0xfff2.toShort
  }

  object Elf64_Sym {
    val NameOffset = 0
    val InfoOffset = 4
    val ShndxOffset = 6
    val ValueOffset = 8
    val SizeOffset = 16
    val TotalSize = 24

    val GlobalFunction = 0x12
  }

  object Elf64_Rel {
    val OffsetOffset = 0
    val InfoOffset = 8
    def r_sym(x: Long): Long = (x >> 32).toInt
    def r_type(x: Long): Int = x.toInt
    val TotalSize = 16

    val R_BPF_64_64 = 1
  }
}
