#include <cassert>
#include <span>

#pragma once

namespace endian {

enum class Endianness { little, big };

static const Endianness hostEndian = []() {
    uint32_t test = 1;
    return *reinterpret_cast<uint8_t *>(&test) == 1 ? Endianness::little : Endianness::big;
}();

template <typename T> T byteSwap(T value, Endianness targetEndian)
{
    static_assert(std::is_trivially_copyable_v<T>, "T must be trivially copyable");
    constexpr size_t N = sizeof(T);

    if (N <= 1 || targetEndian == hostEndian) {
        return value;
    }

    // swap bytes to target endian
    T result = value;
    uint8_t *bytes = reinterpret_cast<uint8_t *>(&result);
    std::reverse(bytes, bytes + N);
    return result;
}

template <typename T> void write(uint8_t *dst, T value, Endianness endian)
{
    T converted = byteSwap<T>(value, endian);
    std::memcpy(dst, &converted, sizeof(T));
}

} // namespace endian

namespace elf {

using Elf64_Addr = uint64_t;
using Elf64_Off = uint64_t;
using Elf64_Half = uint16_t;
using Elf64_Word = uint32_t;
using Elf64_Sword = int32_t;
using Elf64_Xword = uint64_t;
using Elf64_Sxword = int64_t;

inline constexpr int EI_NIDENT = 16;

// ELF Header
struct Elf64_Ehdr {
    unsigned char e_ident[EI_NIDENT]; // ELF Identification bytes
    Elf64_Half e_type;                // Type of file (see ET_* below)
    Elf64_Half e_machine;             // Required architecture for this file (see EM_*)
    Elf64_Word e_version;             // Must be equal to 1
    Elf64_Addr e_entry;               // Address to jump to in order to start program
    Elf64_Off e_phoff;                // Program header table's file offset, in bytes
    Elf64_Off e_shoff;                // Section header table's file offset, in bytes
    Elf64_Word e_flags;               // Processor-specific flags
    Elf64_Half e_ehsize;              // Size of ELF header, in bytes
    Elf64_Half e_phentsize;           // Size of an entry in the program header table
    Elf64_Half e_phnum;               // Number of entries in the program header table
    Elf64_Half e_shentsize;           // Size of an entry in the section header table
    Elf64_Half e_shnum;               // Number of entries in the section header table
    Elf64_Half e_shstrndx;            // Sect hdr table index of sect name string table
};

// Section Header
struct Elf64_Shdr {
    Elf64_Word sh_name;       // Section name (index into string table)
    Elf64_Word sh_type;       // Section type (SHT_*)
    Elf64_Xword sh_flags;     // Section flags (SHF_*)
    Elf64_Addr sh_addr;       // Address where section is to be loaded
    Elf64_Off sh_offset;      // File offset of section data, in bytes
    Elf64_Xword sh_size;      // Size of section, in bytes
    Elf64_Word sh_link;       // Section type-specific header table index link
    Elf64_Word sh_info;       // Section type-specific extra information
    Elf64_Xword sh_addralign; // Section address alignment
    Elf64_Xword sh_entsize;   // Size of records contained within the section
};

// Symbol Table Entry
struct Elf64_Sym {
    Elf64_Word st_name;     // Symbol name (index into string table)
    unsigned char st_info;  // Symbol's type and binding attributes
    unsigned char st_other; // Must be zero; reserved
    Elf64_Half st_shndx;    // Which section (header tbl index) it's defined in
    Elf64_Addr st_value;    // Value or address associated with the symbol
    Elf64_Xword st_size;    // Size of the symbol

    unsigned char getBinding() const { return st_info >> 4; }
    unsigned char getType() const { return st_info & 0x0f; }
    void setBinding(unsigned char b) { setBindingAndType(b, getType()); }
    void setType(unsigned char t) { setBindingAndType(getBinding(), t); }
    void setBindingAndType(unsigned char b, unsigned char t) { st_info = (b << 4) + (t & 0x0f); }
};

// Relocation entry with explicit addend
struct Elf64_Rela {
    Elf64_Addr r_offset;   // Location (file byte offset, or program virtual addr).
    Elf64_Xword r_info;    // Symbol table index and type of relocation.
    Elf64_Sxword r_addend; // Additional relocation data (e.g., offset).

    // These accessors and mutators correspond to the ELF64_R_SYM, ELF64_R_TYPE,
    // and ELF64_R_INFO macros defined in the ELF specification.
    Elf64_Word getSymbol() const { return (r_info >> 32); }
    Elf64_Word getType() const { return (Elf64_Word)(r_info & 0xFFFFFFFF); }
    void setSymbol(Elf64_Word s) { setSymbolAndType(s, getType()); }
    void setType(Elf64_Word t) { setSymbolAndType(getSymbol(), t); }
    void setSymbolAndType(Elf64_Word s, Elf64_Word t)
    {
        r_info = ((Elf64_Xword)s << 32) + (t & 0xFFFFFFFF);
    }
};

// ELF Type
enum ElfType : Elf64_Half {
    ET_REL = 1, // Relocatable file
};

// Section types
enum SectionType : Elf64_Word {
    SHT_NULL = 0,     // Section header table entry unused
    SHT_PROGBITS = 1, // Program data
    SHT_SYMTAB = 2,   // Symbol table
    SHT_STRTAB = 3,   // String table
    SHT_RELA = 4,     // Relocation entries with addends
    SHT_NOBITS = 8,   // Program space with no data (bss)
};

// Section flags
enum SectionFlags : Elf64_Xword {
    SHF_WRITE = 0x1,       // Writable
    SHF_ALLOC = 0x2,       // Occupies memory during execution
    SHF_EXECINSTR = 0x4,   // Executable
    SHF_MERGE = 0x10,      // Might be merged
    SHF_STRINGS = 0x20,    // Contains nul-terminated strings
    SHF_INFO_LINK = 0x40U, // `sh_info' contains SHT index
};

// Symbol Bindings
enum SymbolBinding : unsigned char {
    STB_LOCAL = 0, // Local symbol. These symbols are not visible outside the object file containing
                   // their definition
    STB_GLOBAL = 1, // Global symbols. These symbols are visible to all object files being combined.
    STB_WEAK = 2, // Weak symbols. These symbols resemble global symbols, but their definitions have
                  // lower precedence.
};

enum SymbolType : unsigned char {
    // Symbol Types
    STT_NOTYPE = 0,  // Symbol type is not specified
    STT_OBJECT = 1,  // Symbol is a data object (variable, array, etc.)
    STT_FUNC = 2,    // Symbol is a code object (function, etc.)
    STT_SECTION = 3, // Symbol associated with a section (for relocation)
};

// Special section indices
enum SectionIndex : Elf64_Half {
    SHN_UNDEF = 0, // Undefined, missing, irrelevant, or meaningless
};

// Relocation types
enum RelocationType : uint32_t {
    // Data access
    R_AARCH64_ABS64 = 257,            // Direct 64 bit.
    R_AARCH64_ADR_PREL_PG_HI21 = 275, // Page-rel. ADRP imm. from 32:12.
    R_AARCH64_ADD_ABS_LO12_NC = 277,  // Dir. ADD imm. from bits 11:0.

    // Control flow
    R_AARCH64_JUMP26 = 282,   // PC-rel. B imm. from bits 27:2 (unconditional jump).
    R_AARCH64_CALL26 = 283,   // PC-rel. BL imm. from bits 27:2 (function call).
    R_AARCH64_CONDBR19 = 280, // PC-rel. cond. br. imm. from 20:2 (conditional branch).

    // Dynamic linking (GOT access)
    R_AARCH64_ADR_GOT_PAGE = 311,     // P-page-rel. GOT off. ADRP 32:12.
    R_AARCH64_LD64_GOT_LO12_NC = 312, // Dir. GOT off. LD/ST imm. 11:3.
};

} // namespace elf

// We currently only support AArch64 ELF object files, so even the name of the namespace is
// obj_writer, it is actually a writer for AArch64 ELF object file for teaching purposes.
namespace obj_writer {

class Section {
  public:
    std::string name;
    elf::Elf64_Word type;
    elf::Elf64_Xword flags;
    elf::Elf64_Xword align;
    elf::Elf64_Xword entsize; // Size of each entry in the section
    endian::Endianness endian;

    std::vector<uint8_t> data;
    std::vector<elf::Elf64_Rela> relocations;

    enum class ContentState { None, Code, Data };
    ContentState lastState = ContentState::None;

    uint64_t virtualSize = 0u;

    Section(std::string_view n, elf::Elf64_Word t, elf::Elf64_Xword f, elf::Elf64_Xword a,
            elf::Elf64_Xword e = 0, endian::Endianness endian = endian::Endianness::little)
        : name(n), type(t), flags(f), align(a), entsize(e), endian(endian)
    {
    }

    // Append a numeric value to the section
    template <typename T> void emit(T val)
    {
        static_assert(std::is_trivially_copyable_v<T>, "only supports trivially copyable types");
        assert(type != elf::SectionType::SHT_NOBITS && "Section is no bits; cannot emit a value");

        size_t currentPos = data.size();
        data.resize(currentPos + sizeof(T));

        // Write the value to the section in the correct endianness
        endian::write<T>(&data[currentPos], val, endian);

        virtualSize += sizeof(T);
    }

    // Emit padding bytes to the section (for alignment).
    // Example:
    //   - emitPadding(12, 0xD503201F) in code section writes 3 NOP instructions (12 bytes)
    //   - emitPadding(12, 0x00) in code section writes 3 zero bytes (12 bytes)
    void emitPadding(uint64_t count, uint32_t fillValue);

    // Append raw bytes to the section (no endianness conversion).
    // Use this for:
    //   - Strings (byte sequences, no endianness needed)
    //   - Data already in the correct endianness
    void emitBytes(const void *src, uint64_t len);

    // Get the size of this section in memory (virtual size).
    //   - For normal sections (.text, .data, .rodata): equals the number of bytes in the file
    //   - For NOBITS sections (.bss): equals the size that will be allocated at runtime
    uint64_t virtSize() const { return virtualSize; }

    // Get the actual number of bytes this section occupies in the file.
    uint64_t fileSize() const { return type == elf::SectionType::SHT_NOBITS ? 0 : data.size(); }
};

class StringTable {
  private:
    std::vector<char> data;
    std::unordered_map<std::string, uint32_t> cache;

  public:
    StringTable() { data.push_back(0 /* null terminator */); }
    std::span<const char> getBuffer() const { return data; }

    // Add a string to the string table and return the offset
    uint32_t add(std::string s);
};

class Writer {
  private:
    std::vector<std::unique_ptr<Section>> sections;
    std::unordered_map<std::string, size_t> sectionNameToIndex;
    StringTable strTable;
    std::vector<elf::Elf64_Sym> symbols;

    // AArch64 ELF object files are little-endian by default.
    endian::Endianness endian = endian::Endianness::little;

  public:
    Writer()
    {
        // ELF format requires the first section (index 0) to be a NULL section.
        addSection("", elf::SectionType::SHT_NULL, 0 /* align */, 0 /* entsize */);

        // Similarly, symbol table requires the first symbol (index 0) to be a NULL symbol
        symbols.emplace_back();
    }

    // Section management
    size_t addSection(const std::string &name, elf::Elf64_Word type, elf::Elf64_Xword flags,
                      elf::Elf64_Xword align, elf::Elf64_Xword entsize = 0);

    // Get a section by name
    Section *getSection(const std::string &name);

    // Get the index of a section
    size_t getSectionIndex(const Section *sec) const;

    // Symbol management
    uint32_t addSymbol(const std::string &name, uint8_t bind, uint8_t type, uint16_t shndx,
                       uint64_t value, uint64_t size = 0);

    // Adjust the size of a symbol
    // For example, when defining a function, we don't know its final size until after all
    // code/data has been emitted. So we need to update the symbol size accordingly.
    void setSymbolSize(uint32_t symIdx, uint64_t size);

    // Relocation management
    void addRelocation(Section *targetSec, uint64_t offset, uint32_t symIdx, uint32_t type,
                       int64_t addend);

    // Data/code generation helpers
    void switchContent(Section *sec, Section::ContentState reqState);
    void emitAlignment(Section *sec, uint64_t align);
    void emitInstruction(Section *sec, uint32_t instr);
    template <typename T> uint64_t emitData(Section *sec, T &&val)
    {
        using RawType = std::remove_cv_t<std::remove_reference_t<T>>;

        // Handle string types (no alignment needed, no endianness)
        if constexpr (std::is_same_v<RawType, std::string_view> ||
                      std::is_same_v<RawType, std::string>) {
            switchContent(sec, Section::ContentState::Data);
            uint64_t offset = sec->virtSize();
            sec->emitBytes(val.data(), val.size());
            sec->emit<uint8_t>(0 /* null terminator */);
            return offset;
        }
        // Handle numeric types (need alignment based on type size)
        else if constexpr (std::is_integral_v<RawType> || std::is_floating_point_v<RawType>) {
            emitAlignment(sec, sizeof(RawType));

            // Set mapping symbol after alignment
            switchContent(sec, Section::ContentState::Data);

            uint64_t offset = sec->virtSize();
            sec->emit(std::forward<T>(val));
            return offset;
        }
        // Handle trivially copyable types (structs, POD types, etc.)
        else if constexpr (std::is_trivially_copyable_v<RawType>) {
            emitAlignment(sec, sizeof(RawType));

            // Set mapping symbol after alignment
            switchContent(sec, Section::ContentState::Data);

            uint64_t offset = sec->virtSize();
            sec->emitBytes(&val, sizeof(RawType));
            return offset;
        }
        // Unsupported types
        else {
            static_assert(
                std::is_trivially_copyable_v<RawType>,
                "emitData only supports strings, numeric types, and trivially copyable types");
        }
    }

    // Increase virtual size of the section and return the offset
    uint64_t reserveBSS(Section *sec, uint64_t size);

    // Write ELF object to the output stream
    bool write(std::ostream &outputStream);
    bool writeToFile(const std::string &filename);
};

} // namespace obj_writer
