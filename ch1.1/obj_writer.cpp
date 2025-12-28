#include <fstream>
#include <numeric>

#include "obj_writer.h"

namespace obj_writer {

//===----------------------------------------------------------------------===//
// Section implementation
//===----------------------------------------------------------------------===//

void Section::emitBytes(const void *src, uint64_t len)
{
    if (type == elf::SectionType::SHT_NOBITS) {
        virtualSize += len;
        return;
    }
    uint64_t currentPos = data.size();
    data.resize(currentPos + len);
    std::memcpy(&data[currentPos], src, len);
    virtualSize += len;
}

void Section::emitPadding(uint64_t count, uint32_t fillValue)
{
    if (count == 0) {
        return;
    }

    if (type == elf::SectionType::SHT_NOBITS) {
        virtualSize += count;
        return;
    }

    if (fillValue != 0) {
        while (count >= 4) {
            emit<uint32_t>(fillValue);
            count -= 4;
        }
    }

    if (count > 0) {
        size_t pos = data.size();
        data.resize(pos + count, 0x00);
        virtualSize += count;
    }
}

//===----------------------------------------------------------------------===//
// StringTable implementation
//===----------------------------------------------------------------------===//

uint32_t StringTable::add(std::string s)
{
    if (s.empty()) {
        return 0;
    }

    if (auto it = cache.find(s); it != cache.end()) {
        return it->second;
    }

    size_t offset = data.size();
    assert(offset <= std::numeric_limits<uint32_t>::max() &&
           "String table offset exceeds 32-bit limit");

    data.resize(offset + s.size() + 1);
    std::memcpy(&data[offset], s.data(), s.size());
    data[offset + s.size()] = 0; // Null terminator

    cache.emplace(std::move(s), offset);
    return static_cast<uint32_t>(offset);
}

//===----------------------------------------------------------------------===//
// Writer implementation
//===----------------------------------------------------------------------===//

size_t Writer::addSection(const std::string &name, elf::Elf64_Word type, elf::Elf64_Xword flags,
                          elf::Elf64_Xword align, elf::Elf64_Xword entsize)
{
    size_t idx = sections.size();
    if (sectionNameToIndex.insert({name, idx}).second) {
        sections.emplace_back(std::make_unique<Section>(name, type, flags, align, entsize, endian));
        return idx;
    }
    return sectionNameToIndex[name];
}

Section *Writer::getSection(const std::string &name)
{
    if (auto it = sectionNameToIndex.find(name); it != sectionNameToIndex.end()) {
        return sections[it->second].get();
    }
    return nullptr;
}

size_t Writer::getSectionIndex(const Section *sec) const
{
    return sectionNameToIndex.at(sec->name);
}

/// Add a symbol and return the index of the symbol
uint32_t Writer::addSymbol(const std::string &name, uint8_t bind, uint8_t type, uint16_t shndx,
                           uint64_t value, uint64_t size)
{
    uint32_t index = symbols.size();
    symbols.emplace_back();
    elf::Elf64_Sym &sym = symbols.back();

    // st_name is the offset of the string in the string table
    sym.st_name = name.empty() ? 0 : strTable.add(name);
    sym.setBindingAndType(bind, type);
    sym.st_other = 0;
    sym.st_shndx = shndx;
    sym.st_value = value;
    sym.st_size = size;
    return index;
}

/// Set the size of a symbol
void Writer::setSymbolSize(uint32_t symIdx, uint64_t size)
{
    assert(symIdx < symbols.size() && "Symbol index out of bounds");
    symbols[symIdx].st_size = size;
}

/// Add a relocation entry to the specified section.
void Writer::addRelocation(Section *targetSec, uint64_t offset, uint32_t symIdx, uint32_t type,
                           int64_t addend)
{
    targetSec->relocations.emplace_back();
    elf::Elf64_Rela &rela = targetSec->relocations.back();
    rela.r_offset = offset;
    rela.setSymbolAndType(symIdx, type);
    rela.r_addend = addend;
}

/// This function ensures ARM-specific mapping symbols $x and $d are emitted for code and data.
/// This is required by the ARM ELF specification to help disassemblers and debuggers correctly
/// interpret mixed-mode sections containing both code and data. According to the ARM ELF
/// specification, the mapping symbols are defined as follows "All mapping symbols have type
/// STT_NOTYPE and binding STB_LOCAL. The st_size field is unused and must be zero." Reference:
/// https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst#565mapping-symbols
void Writer::switchContent(Section *sec, Section::ContentState reqState)
{
    // if the section's last state and the required state are different, we add a mapping symbol
    if (sec->lastState != reqState) {
        std::string symName = (reqState == Section::ContentState::Code) ? "$x" : "$d";
        addSymbol(symName, elf::SymbolBinding::STB_LOCAL, elf::SymbolType::STT_NOTYPE,
                  getSectionIndex(sec), sec->virtSize(),
                  0 /* size: mapping symbols must have st_size = 0 */);
        sec->lastState = reqState;
    }
}

/// Emit padding bytes to the section
/// According to the content state of the section, emit different padding bytes
void Writer::emitAlignment(Section *sec, uint64_t align)
{
    uint64_t currentOffset = sec->virtSize();
    uint64_t padding = (align - (currentOffset % align)) % align;

    if (sec->lastState == Section::ContentState::Code) {
        sec->emitPadding(padding, 0xD503201F); // AArch64 NOP instruction
    }
    else {
        sec->emitPadding(padding, 0x00);
    }
}

/// Emit an instruction to the section.
void Writer::emitInstruction(Section *sec, uint32_t instr)
{
    // Align to 4 bytes BEFORE switching to Code state
    emitAlignment(sec, 4u);

    // Switch to Code state and set mapping symbol ($x) if needed.
    // This marks the start of code. If we're already in Code state, no new mapping symbol is
    // added.
    switchContent(sec, Section::ContentState::Code);

    // Emit the instruction
    sec->emit<uint32_t>(instr);
}

/// Reserve BSS space in the section
uint64_t Writer::reserveBSS(Section *sec, uint64_t size)
{
    // Ensure the section is a NOBITS section
    assert(sec->type == elf::SectionType::SHT_NOBITS && "Section is not a NOBITS section");

    uint64_t offset = sec->virtSize();
    sec->virtualSize += size;

    return offset;
}

bool Writer::write(std::ostream &os)
{
    // Symbol Sorting
    // Since sh_info should be the last local symbol index + 1
    // We need to sort the symbols so that the local symbols are first
    // STB_LOCAL = 0 < STB_GLOBAL = 1 < STB_WEAK = 2
    std::vector<size_t> indices(symbols.size());
    std::iota(indices.begin(), indices.end(), 0);

    std::stable_sort(indices.begin(), indices.end(), [&](size_t a, size_t b) {
        return symbols[a].getBinding() < symbols[b].getBinding();
    });

    // Reorder symbols and build old -> new index mapping
    std::vector<elf::Elf64_Sym> sortedSymbols;
    sortedSymbols.reserve(symbols.size());
    std::vector<size_t> oldToNew(symbols.size());
    for (size_t i = 0; i < indices.size(); ++i) {
        oldToNew[indices[i]] = i;
        sortedSymbols.push_back(std::move(symbols[indices[i]]));
    }
    symbols = std::move(sortedSymbols);

    // Update Relocations with new symbol indices
    for (auto &sec : sections) {
        for (auto &rela : sec->relocations) {
            uint32_t oldSym = rela.getSymbol();
            uint32_t type = rela.getType();
            rela.setSymbolAndType(oldToNew[oldSym], type);
        }
    }

    // Create Meta Sections
    // section name string table
    size_t shstrtabIdx = addSection(".shstrtab", elf::SectionType::SHT_STRTAB, 0, 1);
    // symbol string table
    size_t strtabIdx = addSection(".strtab", elf::SectionType::SHT_STRTAB, 0, 1);
    // symbol table
    size_t symtabIdx =
        addSection(".symtab", elf::SectionType::SHT_SYMTAB, 0, 8, sizeof(elf::Elf64_Sym));

    // Fill .strtab with symbol names
    sections[strtabIdx]->data.assign(strTable.getBuffer().begin(), strTable.getBuffer().end());
    sections[strtabIdx]->virtualSize = strTable.getBuffer().size();

    // Fill .symtab with symbols
    sections[symtabIdx]->data.resize(symbols.size() * sizeof(elf::Elf64_Sym));
    std::memcpy(sections[symtabIdx]->data.data(), symbols.data(),
                symbols.size() * sizeof(elf::Elf64_Sym));
    sections[symtabIdx]->virtualSize = sections[symtabIdx]->data.size();

    // Create Relocation Sections for sections that have relocations
    std::vector<size_t> relaSections;
    for (size_t i = 0; i < sections.size(); ++i) {
        if (sections[i]->relocations.empty()) {
            continue;
        }
        std::string relaName = ".rela" + sections[i]->name;
        size_t relaIdx = addSection(relaName, elf::SectionType::SHT_RELA,
                                    elf::SectionFlags::SHF_INFO_LINK, 8, sizeof(elf::Elf64_Rela));
        auto &relaSec = sections[relaIdx];
        relaSec->data.resize(sections[i]->relocations.size() * sizeof(elf::Elf64_Rela));
        std::memcpy(relaSec->data.data(), sections[i]->relocations.data(), relaSec->data.size());
        relaSec->virtualSize = relaSec->data.size();
        relaSections.push_back(relaIdx);
    }

    // Build Section Name String Table
    StringTable shStrTab;
    std::vector<uint32_t> secNameOffsets(sections.size());
    for (size_t i = 0; i < sections.size(); ++i) {
        secNameOffsets[i] = shStrTab.add(sections[i]->name);
    }

    sections[shstrtabIdx]->data.assign(shStrTab.getBuffer().begin(), shStrTab.getBuffer().end());
    sections[shstrtabIdx]->virtualSize = shStrTab.getBuffer().size();

    // Calculate Section Offsets and Section Headers
    std::vector<elf::Elf64_Shdr> shdrs(sections.size());
    uint64_t currentOffset = sizeof(elf::Elf64_Ehdr);

    for (size_t i = 0; i < sections.size(); ++i) {
        auto &sec = sections[i];
        auto &sh = shdrs[i];
        sh.sh_name = secNameOffsets[i];
        sh.sh_type = sec->type;
        sh.sh_flags = sec->flags;
        sh.sh_addr = 0; // Relocatable object files have zero addresses
        sh.sh_addralign = sec->align;
        sh.sh_entsize = sec->entsize;
        sh.sh_link = 0;
        sh.sh_info = 0;

        // Align current offset
        if (sec->align > 1) {
            uint64_t padding = (sec->align - (currentOffset % sec->align)) % sec->align;
            currentOffset += padding;
        }
        sh.sh_offset = currentOffset;
        sh.sh_size = sec->virtSize();
        currentOffset += sec->fileSize();
    }

    // Set Section Header Links and Info
    // .symtab links to .strtab and contains number of local symbols in sh_info
    size_t numLocal = 0;
    for (auto &sym : symbols) {
        if (sym.getBinding() != static_cast<uint8_t>(elf::SymbolBinding::STB_LOCAL)) {
            break;
        }
        numLocal++;
    }
    shdrs[symtabIdx].sh_link = strtabIdx;
    shdrs[symtabIdx].sh_info = numLocal;

    // .rela.* sections link to .symtab and contain target section index in sh_info
    for (size_t relaIdx : relaSections) {
        std::string targetName = sections[relaIdx]->name.substr(5); // Remove ".rela" prefix
        for (size_t j = 0; j < sections.size(); ++j) {
            if (sections[j]->name == targetName) {
                shdrs[relaIdx].sh_link = symtabIdx;
                shdrs[relaIdx].sh_info = j;
                break;
            }
        }
    }

    // Write ELF Header
    elf::Elf64_Ehdr ehdr = {};
    ehdr.e_ident[0] = 0x7f;
    ehdr.e_ident[1] = 'E';
    ehdr.e_ident[2] = 'L';
    ehdr.e_ident[3] = 'F';
    ehdr.e_ident[4] = 2; // ELFCLASS64
    ehdr.e_ident[5] = 1; // ELFDATA2LSB (little-endian)
    ehdr.e_ident[6] = 1; // EV_CURRENT
    ehdr.e_type = elf::ElfType::ET_REL;
    ehdr.e_machine = 183; // EM_AARCH64
    ehdr.e_version = 1;
    ehdr.e_shoff = currentOffset; // Section header table offset
    ehdr.e_ehsize = sizeof(elf::Elf64_Ehdr);
    ehdr.e_shentsize = sizeof(elf::Elf64_Shdr);
    ehdr.e_shnum = sections.size();
    ehdr.e_shstrndx = shstrtabIdx;

    os.write(reinterpret_cast<char *>(&ehdr), sizeof(ehdr));

    // Write Sections
    for (size_t i = 1; i < sections.size(); ++i) {
        // skip NOBITS sections
        if (sections[i]->type == elf::SectionType::SHT_NOBITS) {
            continue;
        }

        auto &sh = shdrs[i];
        uint64_t fpos = os.tellp();
        if (sh.sh_offset > static_cast<uint64_t>(fpos)) {
            // Add padding to reach the required offset
            std::vector<char> pad(sh.sh_offset - fpos, 0);
            os.write(pad.data(), pad.size());
        }
        os.write(reinterpret_cast<const char *>(sections[i]->data.data()),
                 sections[i]->data.size());
    }

    // Write Section Headers
    uint64_t fpos = os.tellp();
    if (ehdr.e_shoff > static_cast<uint64_t>(fpos)) {
        // Add padding before section headers
        std::vector<char> pad(ehdr.e_shoff - fpos, 0);
        os.write(pad.data(), pad.size());
    }
    os.write(reinterpret_cast<char *>(shdrs.data()), sizeof(elf::Elf64_Shdr) * shdrs.size());

    return true;
}

bool Writer::writeToFile(const std::string &filename)
{
    std::ofstream file(filename, std::ios::binary);
    if (!file) {
        return false;
    }
    return write(file);
}

} // namespace obj_writer
