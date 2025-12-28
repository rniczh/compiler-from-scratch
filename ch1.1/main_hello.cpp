#include <iostream>

#include "obj_writer.h"

using namespace elf;

namespace arm64 {
uint32_t adrp(uint8_t rd) { return (1 << 31) | (0b10000 << 24) | rd; }
uint32_t add_imm(uint8_t rd, uint8_t rn) { return (1 << 31) | (0b0010001 << 24) | (rn << 5) | rd; }
uint32_t bl() { return 0x94000000; }
uint32_t ret() { return 0xD65F03C0; }

// LDP post-index: ldp Rt1, Rt2, [Rn], #imm
uint32_t ldp_post_64(int32_t imm7, uint8_t rt2, uint8_t rn, uint8_t rt)
{
    return (0b10 << 30) | (0b101 << 27) | (0b0 << 26) | (0b00 << 24) | (0b11 << 22) |
           ((static_cast<uint32_t>(imm7) & 0x7F) << 15) | ((rt2 & 0x1F) << 10) |
           ((rn & 0x1F) << 5) | (rt & 0x1F);
}

// STP pre-index: stp Rt1, Rt2, [Rn, #imm]!
uint32_t stp_pre_64(int32_t imm7, uint8_t rt2, uint8_t rn, uint8_t rt)
{
    return (0b10 << 30) | (0b101 << 27) | (0b0 << 26) | (0b01 << 24) | (0b10 << 22) |
           ((static_cast<uint32_t>(imm7) & 0x7F) << 15) | ((rt2 & 0x1F) << 10) |
           ((rn & 0x1F) << 5) | (rt & 0x1F);
}
} // namespace arm64

int main()
{
    obj_writer::Writer writer;

    // Sections
    std::array<size_t, 2> sectionIndices = {
        writer.addSection(".text", SectionType::SHT_PROGBITS,
                          SectionFlags::SHF_ALLOC | SectionFlags::SHF_EXECINSTR, 4),
        writer.addSection(
            ".rodata.str1.1", SectionType::SHT_PROGBITS,
            SectionFlags::SHF_ALLOC | SectionFlags::SHF_MERGE | SectionFlags::SHF_STRINGS, 1, 1),
    };

    std::array<obj_writer::Section *, 2> sections = {
        writer.getSection(".text"),
        writer.getSection(".rodata.str1.1"),
    };

    // Symbols
    uint32_t symRodataStr = writer.addSymbol("", SymbolBinding::STB_LOCAL, SymbolType::STT_SECTION,
                                             sectionIndices[1], 0);
    uint32_t symMain = writer.addSymbol("main", SymbolBinding::STB_GLOBAL, SymbolType::STT_FUNC,
                                        sectionIndices[0], 0);
    uint32_t symPrintf = writer.addSymbol("printf", SymbolBinding::STB_GLOBAL,
                                          SymbolType::STT_NOTYPE, SectionIndex::SHN_UNDEF, 0);

    // .rodata.str1.1 section content:
    // Offset     Data                                Value
    // 0x00000000 68656c6c 6f20776f 726c640a 00       hello world..
    std::string helloStr = "hello world\n";
    uint64_t helloStrOffset = writer.emitData(sections[1], helloStr);

    // .text section content:
    uint64_t funcStart = sections[0]->virtSize();

    // Function prologue: save fp, lr and update fp
    //   0: a9bf7bfd      stp     x29, x30, [sp, #-0x10]!
    //   4: 910003fd      mov     x29, sp
    writer.emitInstruction(sections[0], arm64::stp_pre_64(-2, 30, 31, 29));
    writer.emitInstruction(sections[0], arm64::add_imm(29, 31));

    // Load string address (need to be relocated) into x0 (first argument)
    //   8: 90000000      adrp    x0, 0x0 <main>
    //   c: 91000000      add     x0, x0, #0x0
    uint64_t offAdrp = sections[0]->virtSize();
    writer.emitInstruction(sections[0], arm64::adrp(0));
    writer.addRelocation(sections[0], offAdrp, symRodataStr,
                         RelocationType::R_AARCH64_ADR_PREL_PG_HI21, helloStrOffset);
    uint64_t offAdd = sections[0]->virtSize();
    writer.emitInstruction(sections[0], arm64::add_imm(0, 0));
    writer.addRelocation(sections[0], offAdd, symRodataStr,
                         RelocationType::R_AARCH64_ADD_ABS_LO12_NC, helloStrOffset);

    // Call printf (need to be relocated)
    //  10: 94000000      bl      0x10 <main+0x10>
    uint64_t offBl = sections[0]->virtSize();
    writer.emitInstruction(sections[0], arm64::bl());
    writer.addRelocation(sections[0], offBl, symPrintf, RelocationType::R_AARCH64_CALL26, 0);

    // Now we have three relocations in .rela.text section:
    // [ 6] .rela.text        RELA             0000000000000000  00000150
    // 0000000000000048  0000000000000018   I       5     1     8
    // Relocation section '.rela.text' at offset 0x150 contains 3 entries:
    //   Offset          Info           Type           Sym. Value    Sym. Name + Addend
    // 000000000008  000100000113 R_AARCH64_ADR_PRE 0000000000000000 .rodata.str1.1 + 0
    // 00000000000c  000100000115 R_AARCH64_ADD_ABS 0000000000000000 .rodata.str1.1 + 0
    // 000000000010  00050000011b R_AARCH64_CALL26  0000000000000000 printf + 0

    // Set return value to 0
    //  14: d2800000      mov     x0, #0x0                // =0
    writer.emitInstruction(sections[0], 0xD2800000);

    // Function epilogue: restore fp and lr and return
    //  18: a8c17bfd      ldp     x29, x30, [sp], #0x10
    //  1c: d65f03c0      ret
    writer.emitInstruction(sections[0], arm64::ldp_post_64(2, 30, 31, 29));
    writer.emitInstruction(sections[0], arm64::ret());

    // Update `main` function size
    uint64_t funcEnd = sections[0]->virtSize();
    writer.setSymbolSize(symMain, funcEnd - funcStart);

    // Write to file
    if (writer.writeToFile("hello.o") == false) {
        std::cerr << "Failed to write hello.o\n";
        return 1;
    }

    std::cout << "Successfully generated hello.o\nRun `gcc -o hello hello.o` to link the object "
                 "file into an executable.\n";
    return 0;
}
