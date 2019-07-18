package freechips.rocketchip.instrumenter

import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}


import scala.collection.mutable.ArrayBuffer

object BpfLoader {
  import ElfConstants._

  sealed trait SectionKind
  case object RegularSection extends SectionKind
  case object SymtabSection extends SectionKind
  case object StrtabSection extends SectionKind
  case object RelSection extends SectionKind

  final case class Elf64Header(sectionHeaders: Seq[ByteBuffer], sectionStringTableIndex: Int)
  final case class Elf64Section(data: ByteBuffer, linkIndex: Int, infoIndex: Int, kind: SectionKind)
  final case class Symbol(name: String, value: Int, size: Int, shndx: Int, isInstrumenter: Boolean)
  final case class Relocation(relocatedSection: Int, offset: Int, symbol: Symbol)

  final case class BpfInsn(opcode: Int, dst: Int, src: Int, offset: Int, imm: Either[Long, Symbol])
  final case class BpfProg(name: String, insns: Seq[BpfInsn])

  def isDoubleWidthOpcode(opcode: Int): Boolean = opcode == 0x18

  def relocatedInsn(buffer: ByteBuffer, ind: Int, sym: Symbol): BpfInsn = notRelocatedInsn(buffer, ind).copy(imm = Right(sym))
  def notRelocatedInsn(buffer: ByteBuffer, ind: Int): BpfInsn = {
    val raw = buffer.getLong(ind)
    val opcode = (raw & 0xFF).toInt
    val hi_imm = if (isDoubleWidthOpcode(opcode)) buffer.getLong(ind + 8) & 0xFFFFFFFF00000000l else 0
    BpfInsn(
      opcode = opcode,
      dst = ((raw >> 8) & 0xF).toInt,
      src = ((raw >> 12) & 0xF).toInt,
      offset = ((raw >> 16) & 0xFFFF).toInt,
      imm = Left(((raw >> 32) & 0xFFFFFFFFl) | hi_imm)
    )
  }


  def getString(strtab: ByteBuffer, start: Int): String = {
    val array = strtab.array()
    val realStart = strtab.arrayOffset() + start
    var end = realStart
    while (array(end) != 0)
      end += 1
    new String(array, realStart, end - realStart)
  }

  def headerFrom(data: ByteBuffer): Elf64Header = {
    import Elf64_Ehdr._
    {
      import e_ident._
      assert(data.getInt(0) == ElfMagic)
      assert(data.get(EI_CLASS) == ELFCLASS64)
      assert(data.get(EI_DATA) == ELFDATA2LSB)
      assert(data.get(EI_VERSION) == EV_CURRENT)
    }
    assert(data.getShort(TypeOffset) == ET_REL)
    assert(data.getShort(MachineOffset) == EM_BPF)

    val sectionTableOffset = data.getLong(ShOffsetOffset).toInt
    assert(sectionTableOffset != 0)

    val sectionNumber = data.getShort(ShNumOffset)
    val sectionHeaderSize = data.getShort(ShentSizeOffset)
    val sectionStrtabInd = data.getShort(ShStrndxOffset)

    val sectionHeaders = (0 until sectionNumber).map { ind =>
      data.position(sectionTableOffset + ind * sectionHeaderSize)
      data.limit(sectionTableOffset + (ind + 1) * sectionHeaderSize)
      data.slice().order(ByteOrder.LITTLE_ENDIAN)
    }

    Elf64Header(sectionHeaders, sectionStrtabInd)
  }

  def createSection(data: ByteBuffer, shdrData: ByteBuffer, names: Option[ByteBuffer]): Elf64Section = {
    import Elf64_Shdr._

    val nameOffset = shdrData.getInt(NameOffset)
    val name = names.map(getString(_, nameOffset)).getOrElse("<none>")
    val tpe = shdrData.getInt(TypeOffset)
    val offset = shdrData.getLong(OffsetOffset).toInt
    val size = shdrData.getLong(SizeOffset).toInt
    val link = shdrData.getInt(LinkOffset)
    val info = shdrData.getInt(InfoOffset)

    val sectionData = tpe match {
      case SHT_NULL => null
      case SHT_NOBITS => ByteBuffer.allocate(size)
      case _ =>
        data.position(0)
        data.limit(offset + size)
        data.position(offset)
        data.slice().order(ByteOrder.LITTLE_ENDIAN)
    }
    val sectionKind = tpe match {
      case SHT_STRTAB => StrtabSection
      case SHT_SYMTAB => SymtabSection
      case SHT_REL => RelSection
      case SHT_RELA => ???
      case _ => RegularSection
    }
    Elf64Section(sectionData, link, info, sectionKind)
  }

  def parseSymbols(symtab: ByteBuffer, symstrtab: ByteBuffer): Seq[Symbol] = {
    import Elf64_Sym._
    (0 until (symtab.capacity() / TotalSize)).map { ind =>
      val nameOffset = symtab.getShort(ind * TotalSize + NameOffset)
      val tpe = symtab.get(ind * TotalSize + InfoOffset)
      val shndx = symtab.getShort(ind * TotalSize + ShndxOffset)
      val value = symtab.getLong(ind * TotalSize + ValueOffset).toInt
      val size = symtab.getLong(ind * TotalSize + SizeOffset).toInt

      val name = getString(symstrtab, nameOffset)
      Symbol(name, value, size, shndx, tpe == GlobalFunction)
    }
  }

  def collectRelocations(sections: Seq[Elf64Section], symstrtab: ByteBuffer, rel: Elf64Section): Seq[Relocation] = {
    import Elf64_Rel._
    val data = rel.data
    val syms = parseSymbols(sections(rel.linkIndex).data, symstrtab)
    (0 until (data.capacity() / TotalSize)).map { ind =>
      val offset = data.getLong(ind * TotalSize + OffsetOffset).toInt
      val info = data.getLong(ind * TotalSize + InfoOffset)
      val tpe = r_type(info)
      val sym = r_sym(info).toInt

      assert(tpe == R_BPF_64_64)

      Relocation(rel.infoIndex, offset, syms(sym))
    }
  }

  def parseInstrumenters(syms: Seq[Symbol], sectionData: ByteBuffer, relocs: Seq[Relocation]): Seq[BpfProg] = {
    // values are supposed to be of length 1
    val relocsByOffset = relocs.groupBy(_.offset)

    syms.filter(_.isInstrumenter).map { sym =>
      var cur = sym.value
      val end = sym.value + sym.size
      val insns = ArrayBuffer[BpfInsn]()
      while (cur < end) {
        val insn = relocsByOffset.get(cur) match {
          case None => notRelocatedInsn(sectionData, cur)
          case Some(Seq(rel)) => relocatedInsn(sectionData, cur, rel.symbol)
        }
        cur += (if (isDoubleWidthOpcode(insn.opcode)) 16 else 8)
        insns += insn
      }
      BpfProg(sym.name, insns)
    }
  }

  def fetchProgs(data: ByteBuffer): Seq[BpfProg] = {
    data.order(ByteOrder.LITTLE_ENDIAN)
    val header = headerFrom(data)
    val strtab = createSection(data, header.sectionHeaders(header.sectionStringTableIndex), None)
    val sections = header.sectionHeaders.map { sectionHeaderData =>
      createSection(data, sectionHeaderData, Some(strtab.data))
    }
    val symtab = sections.find(_.kind == SymtabSection).get
    val symstrtab = sections.find(_.kind == StrtabSection).get

    val relocs = sections
      .collect {
        case rel@Elf64Section(_, _, _, `RelSection`) =>
          collectRelocations(sections, symstrtab.data, rel)
      }
      .flatten
    val syms = parseSymbols(symtab.data, symstrtab.data)

    syms
      .groupBy(_.shndx)
      .flatMap {
        case (0xFFFFFFF1 | 0xFFFFFFF2, _) =>
          Seq()
        case (shndx, theseSymbols) =>
          parseInstrumenters(theseSymbols, sections(shndx).data, relocs.filter(_.relocatedSection == shndx))
      }
      .toSeq
  }
  def fetchProgs(file: String): Seq[BpfProg] = {
    val data = Files.readAllBytes(Paths.get(file))
    fetchProgs(ByteBuffer.wrap(data))
  }
}
