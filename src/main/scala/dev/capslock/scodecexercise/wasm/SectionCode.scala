package dev.capslock.scodecexercise.wasm

enum SectionCode:
  case TypeSection
  case ImportSection
  case FunctionSection
  case MemorySection
  case ExportSection
  case CodeSection
  case DataSection

object SectionCode:
  import scodec.{*, given}
  import scodec.codecs.*

  def fromId(id: Int): Option[SectionCode] = id match
    case 0   => None // Custom Section. TBD
    case 1   => Some(TypeSection)
    case 2   => Some(ImportSection)
    case 3   => Some(FunctionSection)
    case 5   => Some(MemorySection)
    case 7   => Some(ExportSection)
    case 0xa => Some(CodeSection)
    case 0xb => Some(DataSection)
    case _   => None

  val codec: Codec[SectionCode] = {
    val uint8ToSection: Int => Attempt[SectionCode] = id =>
      fromId(id).fold[Attempt[SectionCode]](
        Attempt.failure(Err(s"Unknown section code: $id")),
      )(Attempt.successful)

    val sectionToUint8: SectionCode => Attempt[Int] =
      case TypeSection     => Attempt.successful(1)
      case ImportSection   => Attempt.successful(2)
      case FunctionSection => Attempt.successful(3)
      case MemorySection   => Attempt.successful(5)
      case ExportSection   => Attempt.successful(7)
      case CodeSection     => Attempt.successful(0xa)
      case DataSection     => Attempt.successful(0xb)

    "sectionCode" | uint8L.exmap(uint8ToSection, sectionToUint8)
  }
