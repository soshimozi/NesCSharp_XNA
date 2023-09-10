using System;
using System.Collections.Generic;

namespace NesEmulatorCS.Emulator;

public class Olc6502
{
    private const ushort StackBase = 0x0100;

    #region private fields
    private readonly List<Instruction> _lookup;
    // helper variables
    private byte _fetched;
    private ushort _temp;
    private ushort _addrAbs;
    private ushort _addrRel;
    private byte _opcode;
    private byte _cycles;
    private uint _clockCount;

    private FamicomSystem _bus;
    #endregion

    #region Public Fields

    private Registers6502 _registers;
    #endregion


    public Registers6502 Registers
    {
        get => _registers;
    }

    public bool IsComplete
    {
        get => _cycles == 0;
    }

    public Dictionary<ushort, string> Disassemble(ushort nStart, ushort nStop)
    {
        uint addr = nStart;
        byte value = 0x00, lo = 0x00, hi = 0x00;
        Dictionary<ushort, string> mapLines = new Dictionary<ushort, string>();
        ushort line_addr = 0;

        Func<uint, byte, string> hex = (n, d) =>
        {
            string s = new string('0', d);
            for (int i = d - 1; i >= 0; i--, n >>= 4)
                s = s.Remove(i, 1).Insert(i, "0123456789ABCDEF"[(int)(n & 0xF)].ToString());
            return s;
        };

        while (addr <= nStop)
        {
            line_addr = (ushort)addr;

            string sInst = "$" + hex(addr, 4) + ": ";

            byte opcode = _bus.CpuRead((ushort)addr, true); addr++;
            sInst += _lookup[opcode].Mnemonic + " ";

            // ... remaining logic remains mostly the same ...

            // Instead of using "lookup[opcode].addrmode == &olc6502::IMP", 
            // you'd need to modify the comparison based on your C# data structure. 
            // Assuming addrmode is an enum in C# for this conversion.
            if (_lookup[opcode].AddressMode == Implied)
            {
                sInst += " {IMP}";
            }
            else if (_lookup[opcode].AddressMode == Immediate)
            {
                value = _bus.CpuRead((ushort)addr, true); addr++;
                sInst += "#$" + hex(value, 2) + " {IMM}";
            }
            else if (_lookup[opcode].AddressMode == ZeroPage)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = 0x00;
                sInst += "$" + hex(lo, 2) + " {ZP0}";
            }
            else if (_lookup[opcode].AddressMode == ZeroPageX)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = 0x00;
                sInst += "$" + hex(lo, 2) + ", X {ZPX}";
            }
            else if (_lookup[opcode].AddressMode == ZeroPageY)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = 0x00;
                sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
            }
            else if (_lookup[opcode].AddressMode == IndirectX)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = 0x00;
                sInst += "($" + hex(lo, 2) + ", X) {IZX}";
            }
            else if (_lookup[opcode].AddressMode == IndirectY)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = 0x00;
                sInst += "($" + hex(lo, 2) + "), Y {IZY}";
            }
            else if (_lookup[opcode].AddressMode == Absolute)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = _bus.CpuRead((ushort)addr, true); addr++;
                sInst += "$" + hex((uint)(hi << 8) | lo, 4) + " {ABS}";
            }
            else if (_lookup[opcode].AddressMode == AbsoluteX)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = _bus.CpuRead((ushort)addr, true); addr++;
                sInst += "$" + hex((uint)(hi << 8) | lo, 4) + ", X {ABX}";
            }
            else if (_lookup[opcode].AddressMode == AbsoluteY)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = _bus.CpuRead((ushort)addr, true); addr++;
                sInst += "$" + hex((uint)(hi << 8) | lo, 4) + ", Y {ABY}";
            }
            else if (_lookup[opcode].AddressMode == Indirect)
            {
                lo = _bus.CpuRead((ushort)addr, true); addr++;
                hi = _bus.CpuRead((ushort)addr, true); addr++;
                sInst += "($" + hex((uint)(hi << 8) | lo, 4) + ") {IND}";
            }
            else if (_lookup[opcode].AddressMode == Relative)
            {
                value = _bus.CpuRead((ushort)addr, true); addr++;
                sInst += "$" + hex(value, 2) + " [$" + hex(addr + (char)value, 4) + "] {REL}";
            }


            mapLines[line_addr] = sInst;
        }

        return mapLines;
    }

    #region Interface aka Pins
    public void Reset()
    {
        // Get address to set program counter to
        _addrAbs = 0xFFFC;

        var lo = Read((ushort)(_addrAbs + 0));
        var hi = Read((ushort)(_addrAbs + 1));

        // Set it
        _registers.Pc = (ushort)(hi << 8 | lo);

        // Reset internal registers
        _registers.A = 0;
        _registers.X = 0;
        _registers.Y = 0;

        _registers.Sp = 0xFD;
        _registers.Status = (byte)(0x00 | Flags6502.U);

        // clear internal variables
        _addrRel = 0x0000;
        _addrAbs = 0x0000;
        _fetched = 0x00;

        // Reset takes time
        _cycles = 8;
    }

    // Interrupt requests are a complex operation and only happen if the
    // "disable interrupt" flag is 0. IRQs can happen at any time, but
    // you dont want them to be destructive to the operation of the running 
    // program. Therefore the current instruction is allowed to finish
    // (which I facilitate by doing the whole thing when cycles == 0) and 
    // then the current program counter is stored on the stack. Then the
    // current status register is stored on the stack. When the routine
    // that services the interrupt has finished, the status register
    // and program counter can be restored to how they where before it 
    // occurred. This is impemented by the "RTI" instruction. Once the IRQ
    // has happened, in a similar way to a reset, a programmable address
    // is read form hard coded location 0xFFFE, which is subsequently
    // set to the program counter.
    public void Irq()
    {
        // if interrupt flag is clear do nothing
        if (GetFlag(Flags6502.I) != 0) return;


        // Push the program counter to the stack.  It's the 16-bits dont
        // forget so that takes two pushes
        StackPush((byte)(_registers.Pc >> 8 & 0X00FF));
        StackPush((byte)(_registers.Pc & 0x00FF));

        // Push status register to stack
        SetFlag(Flags6502.B, false);
        SetFlag(Flags6502.U, true);
        SetFlag(Flags6502.I, true);
        StackPush(_registers.Status);

        // Read program counter location form fixed address
        _addrAbs = 0xFFFE;
        _registers.Pc = ReadWord(_addrAbs);

        // IRQs take time
        _cycles = 7;
    }

    // A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
    // same way as a regular IRQ, but reads the new program counter address
    // form location 0xFFFA.
    public void Nmi()
    {
        // Push the program counter to the stack.  It's the 16-bits dont
        // forget so that takes two pushes
        StackPush((byte)(_registers.Pc >> 8 & 0X00FF));
        StackPush((byte)(_registers.Pc & 0x00FF));

        // Push status register to stack
        SetFlag(Flags6502.B, false);
        SetFlag(Flags6502.U, true);
        SetFlag(Flags6502.I, true);
        StackPush(_registers.Status);

        // Read program counter location form fixed address
        _addrAbs = 0xFFFA;
        _registers.Pc = ReadWord(_addrAbs);

        _cycles = 8;

    }

    public void Clock()
    {
        // Each instruction requires a variable number of clock cycles to execute.
        // In my emulation, I only care about the final result and so I perform
        // the entire computation in one hit. In hardware, each clock cycle would
        // perform "microcode" style transformations of the CPUs state.
        //
        // To remain compliant with connected devices, it's important that the 
        // emulation also takes "time" in order to execute instructions, so I
        // implement that delay by simply counting down the cycles required by 
        // the instruction. When it reaches 0, the instruction is complete, and
        // the next one is ready to be executed.
        if (_cycles == 0)
        {
            // get the next opcode
            _opcode = Read(_registers.Pc);

            // Always set the unused status flag bit to true
            SetFlag(Flags6502.U, true);

            _registers.Pc++;

            _cycles = _lookup[_opcode].Cycles;

            // Perform fetch of intermediate data using
            // correct addresing mode
            var additionalCycle1 = _lookup[_opcode].AddressMode?.Invoke() ?? 0;
            var additionalCycle2 = _lookup[_opcode].Operate?.Invoke() ?? 0;

            _cycles += (byte)(additionalCycle2 & additionalCycle1);

            // always set unused to true
            SetFlag(Flags6502.U, true);
        }

        _clockCount++;
        _cycles--;
    }
    #endregion

    #region Private Helpers
    public void ConnectBus(FamicomSystem b)
    {
        _bus = b;
    }

    private byte StackPop()
    {
        _registers.Sp++;
        return Read((ushort)(StackBase + _registers.Sp));
    }

    private void StackPush(byte data)
    {
        Write((ushort)(StackBase + _registers.Sp), data);
        _registers.Sp--;
    }

    private byte GetFlag(Flags6502 f)
    {
        return (_registers.Status & (byte)f) > 0 ? (byte)1 : (byte)0;
    }

    private void SetFlag(Flags6502 f, bool value)
    {
        if (value)
        {
            _registers.Status |= (byte)f;
        }
        else
        {
            _registers.Status &= (byte)~f;
        }
    }



    private ushort ReadWord(ushort address)
    {
        var lo = Read(address);
        var hi = Read(address, 1);

        return (ushort)(hi << 8 | lo);

    }

    private byte Read(ushort address, int offset = 0)
    {
        return _bus?.CpuRead((ushort)(address + offset), false) ?? 0;
    }

    private void Write(ushort address, byte value, int offset = 0)
    {
        _bus?.CpuWrite((ushort)(address + offset), value);
    }

    // The read location of data can come from two sources, a memory address, or
    // its immediately available as part of the instruction. This function decides
    // depending on address mode of instruction byte
    private byte Fetch()
    {
        if (_lookup[_opcode].AddressMode != Implied)
            _fetched = Read(_addrAbs);

        return _fetched;
    }

    #endregion

    #region Addressing Modes
    // Addressing Modes =============================================
    // The 6502 has a variety of addressing modes to access data in 
    // memory, some of which are direct and some are indirect (like
    // pointers in C++). Each opcode contains information about which
    // addressing mode should be employed to facilitate the 
    // instruction, in regards to where it reads/writes the data it
    // uses. The address mode changes the number of bytes that
    // makes up the full instruction, so we implement addressing
    // before executing the instruction, to make sure the program
    // counter is at the correct location, the instruction is
    // primed with the addresses it needs, and the number of clock
    // cycles the instruction requires is calculated. These functions
    // may adjust the number of cycles required depending upon where
    // and how the memory is accessed, so they return the required
    // adjustment.

    // Address Mode: Implied
    // There is no additional data required for this instruction. The instruction
    // does something very simple like like sets a status bit. However, we will
    // target the accumulator, for instructions like PHA
    private byte Implied()
    {
        _fetched = _registers.A;
        return 0x00;
    }

    // Address Mode: Immediate
    // The instruction expects the next byte to be used as a value, so we'll prep
    // the read address to point to the next byte
    private byte Immediate()
    {
        _addrAbs = _registers.Pc++;
        return 0;
    }

    // Address Mode: Zero Page
    // To save program bytes, zero page addressing allows you to absolutely address
    // a location in first 0xFF bytes of address range. Clearly this only requires
    // one byte instead of the usual two.
    private byte ZeroPage()
    {
        _addrAbs = Read(_registers.Pc);
        _registers.Pc++;

        _addrAbs &= 0x00FF;

        return 0x00;
    }

    // Address Mode: Zero Page with X Offset
    // Fundamentally the same as Zero Page addressing, but the contents of the X Register
    // is added to the supplied single byte address. This is useful for iterating through
    // ranges within the first page.
    private byte ZeroPageX()
    {
        _addrAbs = (ushort)(Read(_registers.Pc) + _registers.X);
        _registers.Pc++;
        _addrAbs &= 0x00FF;
        return 0;
    }


    // Address Mode: Zero Page with Y Offset
    // Same as above but uses Y Register for offset
    private byte ZeroPageY()
    {
        _addrAbs = (ushort)(Read(_registers.Pc) + _registers.Y);
        _registers.Pc++;
        _addrAbs &= 0x00FF;
        return 0;
    }

    // Address Mode: Relative
    // This address mode is exclusive to branch instructions. The address
    // must reside within -128 to +127 of the branch instruction, i.e.
    // you cant directly branch to any address in the addressable range.
    private byte Relative()
    {
        _addrRel = Read(_registers.Pc);
        _registers.Pc++;
        if ((_addrRel & 0x80) != 0)
        {
            _addrRel |= 0xFF00;
        }

        return 0;
    }

    // Address Mode: Absolute 
    // A full 16-bit address is loaded and used
    private byte Absolute()
    {
        var lo = (ushort)Read(_registers.Pc);
        _registers.Pc++;
        var hi = (ushort)Read(_registers.Pc);
        _registers.Pc++;

        _addrAbs = (ushort)(hi << 8 | lo);


        return 0x00;
    }

    // Address Mode: Absolute with X Offset
    // Fundamentally the same as absolute addressing, but the contents of the X Register
    // is added to the supplied two byte address. If the resulting address changes
    // the page, an additional clock cycle is required
    private byte AbsoluteX()
    {
        var lo = (ushort)Read(_registers.Pc);
        _registers.Pc++;
        var hi = (ushort)Read(_registers.Pc);
        _registers.Pc++;

        _addrAbs = (ushort)(hi << 8 | lo);
        _addrAbs += _registers.X;

        return (_addrAbs & 0xFF00) != hi << 8 ? (byte)1 : (byte)0;
    }

    // Address Mode: Absolute with Y Offset
    // Fundamentally the same as absolute addressing, but the contents of the Y Register
    // is added to the supplied two byte address. If the resulting address changes
    // the page, an additional clock cycle is required
    private byte AbsoluteY()
    {
        var lo = (ushort)Read(_registers.Pc);
        _registers.Pc++;
        var hi = (ushort)Read(_registers.Pc);
        _registers.Pc++;

        _addrAbs = (ushort)(hi << 8 | lo);
        _addrAbs += _registers.Y;

        return (_addrAbs & 0xFF00) != hi << 8 ? (byte)1 : (byte)0;
    }

    // Note: The next 3 address modes use indirection (aka Pointers!)

    // Address Mode: Indirect
    // The supplied 16-bit address is read to get the actual 16-bit address. This is
    // instruction is unusual in that it has a bug in the hardware! To emulate its
    // function accurately, we also need to emulate this bug. If the low byte of the
    // supplied address is 0xFF, then to read the high byte of the actual address
    // we need to cross a page boundary. This doesnt actually work on the chip as 
    // designed, instead it wraps back around in the same page, yielding an 
    // invalid actual address
    private byte Indirect()
    {
        var lo = (ushort)Read(_registers.Pc);
        _registers.Pc++;
        var hi = (ushort)Read(_registers.Pc);
        _registers.Pc++;

        ushort ptr = (ushort)(hi << 8 | lo);

        if (lo == 0x0FF) // Simulate page boundary hardware bug
        {
            _addrAbs = (ushort)(Read((ushort)(ptr & 0xFF00)) << 8 | Read((ushort)(ptr + 0)));
        }
        else
        {
            _addrAbs = (ushort)(Read((ushort)(ptr + 1)) << 8 | Read((ushort)(ptr + 0)));
        }

        return 0x00;

    }

    // Address Mode: Indirect X
    // The supplied 8-bit address is offset by X Register to index
    // a location in page 0x00. The actual 16-bit address is read 
    // from this location
    private byte IndirectX()
    {
        var t = Read(_registers.Pc);
        _registers.Pc++;

        var lo = Read((ushort)(t + _registers.X & 0x00FF));
        var hi = Read((ushort)(t + _registers.X + 1 & 0x00FF));


        _addrAbs = (ushort)(hi << 8 | lo);

        return 0x00;
    }

    // Address Mode: Indirect Y
    // The supplied 8-bit address indexes a location in page 0x00. From 
    // here the actual 16-bit address is read, and the contents of
    // Y Register is added to it to offset it. If the offset causes a
    // change in page then an additional clock cycle is required.
    private byte IndirectY()
    {
        var t = Read(_registers.Pc);
        _registers.Pc++;

        var lo = Read((ushort)(t & 0x00FF));
        var hi = Read((ushort)(t + 1 & 0x00FF));

        _addrAbs = (ushort)(hi << 8 | lo);

        return (_addrAbs & 0xff00) != hi << 8 ? (byte)1 : (byte)0x00;
    }

    #endregion

    #region OpCodes
    // Opcodes ======================================================
    // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
    // have not modelled "unofficial" opcodes. As each opcode is 
    // defined by 1 byte, there are potentially 256 possible codes.
    // Codes are not used in a "switch case" style on a processor,
    // instead they are repsonisble for switching individual parts of
    // CPU circuits on and off. The opcodes listed here are official, 
    // meaning that the functionality of the chip when provided with
    // these codes is as the developers intended it to be. Unofficial
    // codes will of course also influence the CPU circuitry in 
    // interesting ways, and can be exploited to gain additional
    // functionality!
    //
    // These functions return 0 normally, but some are capable of
    // requiring more clock cycles when executed under certain
    // conditions combined with certain addressing modes. If that is 
    // the case, they return 1.
    //
    // I have included detailed explanations of each function in 
    // the class implementation file. Note they are listed in
    // alphabetical order here for ease of finding.

    ///////////////////////////////////////////////////////////////////////////////
    // INSTRUCTION IMPLEMENTATIONS

    // Note: Ive started with the two most complicated instructions to emulate, which
    // ironically is addition and subtraction! Ive tried to include a detailed 
    // explanation as to why they are so complex, yet so fundamental. Im also NOT
    // going to do this through the explanation of 1 and 2's complement.

    // Instruction: Add with Carry In
    // Function:    A = A + M + C
    // Flags Out:   C, V, N, Z
    //
    // Explanation:
    // The purpose of this function is to add a value to the accumulator and a carry bit. If
    // the result is > 255 there is an overflow setting the carry bit. Ths allows you to
    // chain together ADC instructions to add numbers larger than 8-bits. This in itself is
    // simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
    //
    // 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
    // us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret 
    // this word as something else if we assume those 8 bits represent the range -128 to +127,
    // i.e. it has become signed.
    //
    // Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
    // called overflow, and this is a useful to know as it indicates that the calculation has
    // gone outside the permissable range, and therefore no longer makes numeric sense.
    //
    // Note the implementation of ADD is the same in binary, this is just about how the numbers
    // are represented, so the word 10000100 can be both -124 and 132 depending upon the 
    // context the programming is using it in. We can prove this!
    //
    //  10000100 =  132  or  -124
    // +00010001 = + 17      + 17
    //  ========    ===       ===     See, both are valid additions, but our interpretation of
    //  10010101 =  149  or  -107     the context changes the value, not the hardware!
    //
    // In principle under the -128 to 127 range:
    // 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
    // therefore negative numbers have the most significant set, positive numbers do not
    //
    // To assist us, the 6502 can set the overflow flag, if the result of the addition has
    // wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
    //
    // Let's suppose we have A = 30, M = 10 and C = 0
    //          A = 30 = 00011110
    //          M = 10 = 00001010+
    //     RESULT = 40 = 00101000
    //
    // Here we have not gone out of range. The resulting significant bit has not changed.
    // So let's make a truth table to understand when overflow has occurred. Here I take
    // the MSB of each component, where R is RESULT.
    //
    // A  M  R | V | A^R | A^M |~(A^M) | 
    // 0  0  0 | 0 |  0  |  0  |   1   |
    // 0  0  1 | 1 |  1  |  0  |   1   |
    // 0  1  0 | 0 |  0  |  1  |   0   |
    // 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
    // 1  0  0 | 0 |  1  |  1  |   0   |
    // 1  0  1 | 0 |  0  |  1  |   0   |
    // 1  1  0 | 1 |  1  |  0  |   1   |
    // 1  1  1 | 0 |  0  |  0  |   1   |
    //
    // We can see how the above equation calculates V, based on A, M and R. V was chosen
    // based on the following hypothesis:
    //       Positive Number + Positive Number = Negative Result -> Overflow
    //       Negative Number + Negative Number = Positive Result -> Overflow
    //       Positive Number + Negative Number = Either Result -> Cannot Overflow
    //       Positive Number + Positive Number = Positive Result -> OK! No Overflow
    //       Negative Number + Negative Number = Negative Result -> OK! NO Overflow
    byte Adc()
    {
        // Grab the data that we are adding to the accumulator
        Fetch();

        // Add is performed in 16-bit domain for emulation to capture any
        // carry bit, which will exist in bit 8 of the 16-bit word
        _temp = (ushort)(_registers.A + _fetched + GetFlag(Flags6502.C));

        // The carry flag out exists in the high byte bit 0
        SetFlag(Flags6502.C, _temp > 255);

        // The Zero flag is set if the result is 0
        SetFlag(Flags6502.Z, (_temp & 0X00FF) == 0);

        // The signed Overflow flag is set based on that up there!
        SetFlag(Flags6502.V, (~(_registers.A ^ _fetched) & (_registers.A ^ _temp) & 0x0080) != 0);

        // The negative flag is set to the most significate bit of the result
        SetFlag(Flags6502.N, (_temp & 0x080) != 0);

        // Load the result into the accumulator 
        _registers.A = (byte)(_temp & 0x00FF);

        // this instruction has the potential to require an additional clock cycle
        return 1;
    }

    // Instruction: Bitwise Logic AND
    // Function:    A = A & M
    // Flags Out:   N, Z
    private byte And()
    {
        Fetch();

        _registers.A &= _fetched;
        SetFlag(Flags6502.Z, _registers.A == 0x00);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0x00);

        return 1;
    }

    // Instruction: Arithmetic Shift Left
    // Function:    A = C <- (A << 1) <- 0
    // Flags Out:   N, Z, C
    private byte Asl()
    {
        Fetch();

        _temp = (ushort)(_fetched << 1);
        SetFlag(Flags6502.C, (_temp & 0xFF00) > 0);

        SetFlag(Flags6502.Z, _temp == 0x00);
        SetFlag(Flags6502.N, (_temp & 0x80) != 0x00);

        if (_lookup[_opcode].AddressMode == Implied)
        {
            _registers.A = (byte)(_temp & 0x00FF);
        }
        else
        {
            Write(_addrAbs, (byte)(_temp & 0x00FF));
        }

        return 0;
    }

    // Instruction: Branch if Carry Clear
    // Function:    if(C == 0) pc = address
    private byte Bcc()
    {
        if (GetFlag(Flags6502.C) != 0) return 0;

        _cycles++;
        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;

        return 0;
    }

    // Instruction: Branch if Carry Set
    // Function:    if(C == 1) pc = address
    private byte Bcs()
    {
        if (GetFlag(Flags6502.C) == 0) return 0;

        _cycles++;
        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;

        return 0;
    }

    // Instruction: Branch if Equal
    // Function:    if(Z == 1) pc = address
    private byte Beq()
    {
        if (GetFlag(Flags6502.Z) == 0) return 0;

        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;
        return 0;
    }

    private byte Bit()
    {
        Fetch();
        _temp = (ushort)(_registers.A & _fetched);
        SetFlag(Flags6502.Z, _temp == 0x00);
        SetFlag(Flags6502.N, (_fetched & 1 << 7) != 0);
        SetFlag(Flags6502.V, (_fetched & 1 << 6) != 0);

        return 0;

    }

    // Instruction: Branch if Negative
    // Function:    if(N == 1) pc = address
    private byte Bmi()
    {
        if (GetFlag(Flags6502.N) == 0) return 0;

        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;
        return 0;
    }

    // Instruction: Branch if Not Equal
    // Function:    if(Z == 0) pc = address
    private byte Bne()
    {
        if (GetFlag(Flags6502.Z) != 0) return 0;

        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;
        return 0;
    }

    // Instruction: Branch if Positive
    // Function:    if(N == 0) pc = address
    private byte Bpl()
    {
        if (GetFlag(Flags6502.N) != 0) return 0;

        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;
        return 0;
    }

    // Instruction: Break
    // Function:    Program Sourced Interrupt
    private byte Brk()
    {
        _registers.Pc++;

        SetFlag(Flags6502.I, true);
        StackPush((byte)(_registers.Pc >> 7 & 0x00FF));
        StackPush((byte)(_registers.Pc & 0x00FF));

        SetFlag(Flags6502.B, true);
        StackPush(_registers.Status);

        SetFlag(Flags6502.B, false);

        _registers.Pc = ReadWord(0xFFFE);
        return 0;
    }

    // Instruction: Branch if Overflow Clear
    // Function:    if(V == 0) pc = address
    private byte Bvc()
    {
        if (GetFlag(Flags6502.V) != 0) return 0;

        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;
        return 0;
    }
    // Instruction: Branch if Overflow Set
    // Function:    if(V == 1) pc = address
    private byte Bvs()
    {
        if (GetFlag(Flags6502.V) != 1) return 0;

        _addrAbs = (ushort)(_registers.Pc + _addrRel);

        if (CheckPageBoundary())
            _cycles++;

        _registers.Pc = _addrAbs;
        return 0;
    }

    // Instruction: Clear Carry Flag
    // Function:    C = 0
    private byte Clc()
    {
        SetFlag(Flags6502.C, false);
        return 0;
    }

    // Instruction: Clear Decimal Flag
    // Function:    D = 0
    private byte Cld()
    {
        SetFlag(Flags6502.D, false);
        return 0;
    }

    // Instruction: Disable Interrupts / Clear Interrupt Flag
    // Function:    I = 0
    private byte Cli()
    {
        SetFlag(Flags6502.I, false);
        return 0;
    }

    // Instruction: Clear Overflow Flag
    // Function:    V = 0
    private byte Clv()
    {
        SetFlag(Flags6502.V, false);
        return 0;
    }


    // Instruction: Compare Accumulator
    // Function:    C <- A >= M      Z <- (A - M) == 0
    // Flags Out:   N, C, Z
    private byte Cmp()
    {
        Fetch();

        _temp = (ushort)(_registers.A - _fetched);

        SetFlag(Flags6502.C, _registers.A >= _fetched);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);
        return 1;
    }

    // Instruction: Compare X Register
    // Function:    C <- X >= M      Z <- (X - M) == 0
    // Flags Out:   N, C, Z
    private byte Cpx()
    {
        Fetch();

        _temp = (ushort)(_registers.X - _fetched);

        SetFlag(Flags6502.C, _registers.X >= _fetched);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);
        return 0;
    }

    // Instruction: Compare Y Register
    // Function:    C <- Y >= M      Z <- (Y - M) == 0
    // Flags Out:   N, C, Z
    private byte Cpy()
    {
        Fetch();

        _temp = (ushort)(_registers.Y - _fetched);

        SetFlag(Flags6502.C, _registers.Y >= _fetched);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);
        return 0;
    }

    // Instruction: Decrement Value at Memory Location
    // Function:    M = M - 1
    // Flags Out:   N, Z
    private byte Dec()
    {
        Fetch();

        _temp = (ushort)(_fetched - 1);
        Write(_addrAbs, (byte)(_temp & 0x00FF));
        SetFlag(Flags6502.Z, (_temp & 0x00ff) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);

        return 0;
    }

    // Instruction: Decrement X Register
    // Function:    X = X - 1
    // Flags Out:   N, Z
    private byte Dex()
    {
        _registers.X--;
        SetFlag(Flags6502.Z, _registers.X == 0);
        SetFlag(Flags6502.N, (_registers.X & 0x0080) != 0);
        return 0;
    }

    // Instruction: Decrement Y Register
    // Function:    Y = Y - 1
    // Flags Out:   N, Z
    private byte Dey()
    {
        _registers.Y--;
        SetFlag(Flags6502.Z, _registers.Y == 0);
        SetFlag(Flags6502.N, (_registers.Y & 0x80) != 0);
        return 0;
    }

    // Instruction: Bitwise Logic XOR
    // Function:    A = A xor M
    // Flags Out:   N, Z
    private byte Eor()
    {
        Fetch();

        _registers.A ^= _fetched;
        SetFlag(Flags6502.Z, _registers.A == 0);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0);

        return 0;
    }

    // Instruction: Increment Value at Memory Location
    // Function:    M = M + 1
    // Flags Out:   N, Z
    private byte Inc()
    {
        Fetch();

        _temp = (byte)(_fetched + 1);
        Write(_addrAbs, (byte)(_temp & 0x00FF));
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);

        return 0;
    }

    // Instruction: Increment X Register
    // Function:    X = X + 1
    // Flags Out:   N, Z
    private byte Inx()
    {
        _registers.X++;
        SetFlag(Flags6502.Z, _registers.X == 0);
        SetFlag(Flags6502.N, (_registers.X & 0x0080) != 0);
        return 0;

    }
    // Instruction: Increment Y Register
    // Function:    Y = Y + 1
    // Flags Out:   N, Z
    private byte Iny()
    {
        _registers.Y++;
        SetFlag(Flags6502.Z, _registers.Y == 0);
        SetFlag(Flags6502.N, (_registers.Y & 0x0080) != 0);
        return 0;
    }


    // Instruction: Jump To Sub-Routine
    // Function:    Push current pc to stack, pc = address
    private byte Jsr()
    {
        _registers.Pc++;

        StackPush((byte)(_registers.Pc >> 8 & 0x00FF));
        StackPush((byte)(_registers.Pc | 0x00FF));

        _registers.Pc = _addrAbs;

        return 0;
    }

    // Instruction: Jump To Location
    // Function:    pc = address
    private byte Jmp()
    {
        _registers.Pc = _addrAbs;
        return 0;
    }

    // Instruction: Load The Accumulator
    // Function:    A = M
    // Flags Out:   N, Z
    private byte Lda()
    {
        Fetch();

        _registers.A = _fetched;
        SetFlag(Flags6502.Z, _registers.A == 0x00);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0);

        return 1;
    }

    // Instruction: Load The X Register
    // Function:    X = M
    // Flags Out:   N, Z
    private byte Ldx()
    {
        Fetch();

        _registers.X = _fetched;
        SetFlag(Flags6502.Z, _registers.X == 0x00);
        SetFlag(Flags6502.N, (_registers.X & 0x80) != 0);

        return 1;
    }

    // Instruction: Load The Y Register
    // Function:    Y = M
    // Flags Out:   N, Z
    private byte Ldy()
    {
        Fetch();

        _registers.Y = _fetched;
        SetFlag(Flags6502.Z, _registers.Y == 0x00);
        SetFlag(Flags6502.N, (_registers.Y & 0x80) != 0);

        return 1;
    }

    private byte Lsr()
    {
        Fetch();
        SetFlag(Flags6502.C, (_fetched & 0x0001) != 0);
        _temp = (ushort)(_fetched >> 1);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);

        if (_lookup[_opcode].AddressMode == Implied)
        {
            _registers.A = (byte)(_temp & 0x00ff);
        }
        else
        {
            Write(_addrAbs, (byte)(_temp & 0x00ff));
        }

        return 0;
    }

    private byte Nop()
    {
        // Sadly not all NOPs are equal, Ive added a few here
        // based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
        // and will add more based on game compatibility, and ultimately
        // I'd like to cover all illegal opcodes too
        switch (_opcode)
        {
            case 0x1C:
            case 0x3C:
            case 0x5C:
            case 0x7C:
            case 0xDC:
            case 0xFC:
                return 1;
        }
        return 0;
    }

    // Instruction: Bitwise Logic OR
    // Function:    A = A | M
    // Flags Out:   N, Z
    private byte Ora()
    {
        Fetch();

        _registers.A |= _fetched;
        SetFlag(Flags6502.Z, _registers.A == 0x00);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0);

        return 1;
    }

    // Instruction: Push Accumulator to Stack
    // Function:    A -> stack
    private byte Pha()
    {
        StackPush(_registers.A);

        return 0;
    }

    // Instruction: Push Status Register to Stack
    // Function:    status -> stack
    // Note:        Break flag is set to 1 before push
    private byte Php()
    {
        StackPush((byte)(_registers.Status | (byte)Flags6502.B | (byte)Flags6502.U));
        SetFlag(Flags6502.B, false);
        SetFlag(Flags6502.U, false);

        return 0;
    }

    // Instruction: Pop Accumulator off Stack
    // Function:    A <- stack
    // Flags Out:   N, Z
    private byte Pla()
    {
        _registers.A = StackPop();

        SetFlag(Flags6502.Z, _registers.A == 0x00);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0);
        return 0;
    }

    // Instruction: Pop Status Register off Stack
    // Function:    Status <- stack
    private byte Plp()
    {
        _registers.Status = StackPop();
        SetFlag(Flags6502.U, true);
        return 0;
    }

    private byte Rol()
    {
        Fetch();

        _temp = (ushort)((ushort)(_fetched << 1) | GetFlag(Flags6502.C));

        SetFlag(Flags6502.C, (_temp & 0xFF00) != 0);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);
        if (_lookup[_opcode].AddressMode == Implied)
        {
            _registers.A = (byte)(_temp & 0x00ff);
        }
        else
        {
            Write(_addrAbs, (byte)(_temp & 0x00ff));
        }

        return 0;
    }

    private byte Ror()
    {
        Fetch();

        _temp = (ushort)(GetFlag(Flags6502.C) << 7 | (ushort)(_fetched >> 1));

        SetFlag(Flags6502.C, (_temp & 0xFF00) != 0);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);
        if (_lookup[_opcode].AddressMode == Implied)
        {
            _registers.A = (byte)(_temp & 0x00ff);
        }
        else
        {
            Write(_addrAbs, (byte)(_temp & 0x00ff));
        }

        return 0;
    }

    private byte Rti()
    {
        _registers.Status = StackPop();
        _registers.Status &= unchecked((byte)~Flags6502.B);
        _registers.Status &= unchecked((byte)~Flags6502.U);

        _registers.Pc = StackPop();
        _registers.Pc |= (byte)(StackPop() << 8);

        _registers.Pc++;
        return 0;
    }

    private byte Rts()
    {
        _registers.Pc = StackPop();
        _registers.Pc |= (byte)(StackPop() << 8);

        _registers.Pc++;
        return 0;
    }

    // Instruction: Subtraction with Borrow In
    // Function:    A = A - M - (1 - C)
    // Flags Out:   C, V, N, Z
    //
    // Explanation:
    // Given the explanation for ADC above, we can reorganise our data
    // to use the same computation for addition, for subtraction by multiplying
    // the data by -1, i.e. make it negative
    //
    // A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
    //
    // To make a signed positive number negative, we can invert the bits and add 1
    // (OK, I lied, a little bit of 1 and 2s complement :P)
    //
    //  5 = 00000101
    // -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
    //
    // The range is actually unimportant, because if I take the value 15, and add 251
    // to it, given we wrap around at 256, the result is 10, so it has effectively 
    // subtracted 5, which was the original intention. (15 + 251) % 256 = 10
    //
    // Note that the equation above used (1-C), but this got converted to + 1 + C.
    // This means we already have the +1, so all we need to do is invert the bits
    // of M, the data(!) therfore we can simply add, exactly the same way we did 
    // before.
    private byte Sbc()
    {
        Fetch();

        ushort value = (ushort)(_fetched ^ 0x00ff);

        _temp = (ushort)(_registers.A + value + GetFlag(Flags6502.C));

        SetFlag(Flags6502.C, (_temp & 0xFF00) != 0);
        SetFlag(Flags6502.Z, (_temp & 0x00FF) == 0);
        SetFlag(Flags6502.V, ((_temp ^ _registers.A) & (_temp ^ value) & 0x0080) != 0);
        SetFlag(Flags6502.N, (_temp & 0x0080) != 0);

        _registers.A = (byte)(_temp & 0x00ff);
        return 1;
    }

    // Instruction: Set Carry Flag
    // Function:    C = 1
    private byte Sec()
    {
        SetFlag(Flags6502.C, true);
        return 0;
    }

    // Instruction: Set Decimal Flag
    // Function:    D = 1
    private byte Sed()
    {
        SetFlag(Flags6502.D, true);
        return 0;
    }

    // Instruction: Set Interrupt Flag / Enable Interrupts
    // Function:    I = 1
    private byte Sei()
    {
        SetFlag(Flags6502.I, true);
        return 0;
    }

    // Instruction: Store Accumulator at Address
    // Function:    M = A
    private byte Sta()
    {
        Write(_addrAbs, _registers.A);
        return 0;
    }

    // Instruction: Store X Register at Address
    // Function:    M = X
    private byte Stx()
    {
        Write(_addrAbs, _registers.X);
        return 0;
    }

    // Instruction: Store Y Register at Address
    // Function:    M = Y
    private byte Sty()
    {
        Write(_addrAbs, _registers.Y);
        return 0;
    }

    // Instruction: Transfer Accumulator to X Register
    // Function:    X = A
    // Flags Out:   N, Z
    private byte Tax()
    {
        _registers.X = _registers.A;

        SetFlag(Flags6502.Z, _registers.X == 0x00);
        SetFlag(Flags6502.N, (_registers.X & 0x80) != 0);

        return 0;
    }

    // Instruction: Transfer Accumulator to Y Register
    // Function:    Y = A
    // Flags Out:   N, Z
    private byte Tay()
    {
        _registers.Y = _registers.A;

        SetFlag(Flags6502.Z, _registers.Y == 0x00);
        SetFlag(Flags6502.N, (_registers.Y & 0x80) != 0);

        return 0;
    }

    // Instruction: Transfer Stack Pointer to X Register
    // Function:    X = stack pointer
    // Flags Out:   N, Z
    private byte Tsx()
    {
        _registers.X = _registers.Sp;
        SetFlag(Flags6502.Z, _registers.X == 0x00);
        SetFlag(Flags6502.N, (_registers.X & 0x80) != 0);

        return 0;
    }

    // Instruction: Transfer X Register to Accumulator
    // Function:    A = X
    // Flags Out:   N, Z
    private byte Txa()
    {
        _registers.A = _registers.X;
        SetFlag(Flags6502.Z, _registers.A == 0x00);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0);

        return 0;
    }

    // Instruction: Transfer X Register to Stack Pointer
    // Function:    stack pointer = X
    private byte Txs()
    {
        _registers.Sp = _registers.X;
        return 0;
    }

    // Instruction: Transfer Y Register to Accumulator
    // Function:    A = Y
    // Flags Out:   N, Z
    private byte Tya()
    {
        _registers.A = _registers.Y;

        SetFlag(Flags6502.Z, _registers.A == 0x00);
        SetFlag(Flags6502.N, (_registers.A & 0x80) != 0);

        return 0;
    }


    // I capture all "unofficial" opcodes with this function. It is
    // functionally identical to a NOP
    byte Unknown() { return Nop(); }
    #endregion

    private bool CheckPageBoundary()
    {
        return (_addrAbs & 0xFF00) != (_registers.Pc & 0XFF00);
    }

    #region Constructor
    public Olc6502()
    {
        _registers = new Registers6502();

        _lookup = new List<Instruction>
        {
            new ( "BRK", Brk, Immediate, 7 ),new ( "ORA", Ora, IndirectX, 6 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 3 ),new ( "ORA", Ora, ZeroPage, 3 ),new ( "ASL", Asl, ZeroPage, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "PHP", Php, Implied, 3 ),new ( "ORA", Ora, Immediate, 2 ),new ( "ASL", Asl, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Nop, Implied, 4 ),new ( "ORA", Ora, Absolute, 4 ),new ( "ASL", Asl, Absolute, 6 ),new ( "???", Unknown, Implied, 6 ),
            new ( "BPL", Bpl, Relative, 2 ),new ( "ORA", Ora, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 4 ),new ( "ORA", Ora, ZeroPageX, 4 ),new ( "ASL", Asl, ZeroPageX, 6 ),new ( "???", Unknown, Implied, 6 ),new ( "CLC", Clc, Implied, 2 ),new ( "ORA", Ora, AbsoluteY, 4 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 7 ),new ( "???", Nop, Implied, 4 ),new ( "ORA", Ora, AbsoluteX, 4 ),new ( "ASL", Asl, AbsoluteX, 7 ),new ( "???", Unknown, Implied, 7 ),
            new ( "JSR", Jsr, Absolute, 6 ),new ( "AND", And, IndirectX, 6 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "BIT", Bit, ZeroPage, 3 ),new ( "AND", And, ZeroPage, 3 ),new ( "ROL", Rol, ZeroPage, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "PLP", Plp, Implied, 4 ),new ( "AND", And, Immediate, 2 ),new ( "ROL", Rol, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "BIT", Bit, Absolute, 4 ),new ( "AND", And, Absolute, 4 ),new ( "ROL", Rol, Absolute, 6 ),new ( "???", Unknown, Implied, 6 ),
            new ( "BMI", Bmi, Relative, 2 ),new ( "AND", And, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 4 ),new ( "AND", And, ZeroPageX, 4 ),new ( "ROL", Rol, ZeroPageX, 6 ),new ( "???", Unknown, Implied, 6 ),new ( "SEC", Sec, Implied, 2 ),new ( "AND", And, AbsoluteY, 4 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 7 ),new ( "???", Nop, Implied, 4 ),new ( "AND", And, AbsoluteX, 4 ),new ( "ROL", Rol, AbsoluteX, 7 ),new ( "???", Unknown, Implied, 7 ),
            new ( "RTI", Rti, Implied, 6 ),new ( "EOR", Eor, IndirectX, 6 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 3 ),new ( "EOR", Eor, ZeroPage, 3 ),new ( "LSR", Lsr, ZeroPage, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "PHA", Pha, Implied, 3 ),new ( "EOR", Eor, Immediate, 2 ),new ( "LSR", Lsr, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "JMP", Jmp, Absolute, 3 ),new ( "EOR", Eor, Absolute, 4 ),new ( "LSR", Lsr, Absolute, 6 ),new ( "???", Unknown, Implied, 6 ),
            new ( "BVC", Bvc, Relative, 2 ),new ( "EOR", Eor, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 4 ),new ( "EOR", Eor, ZeroPageX, 4 ),new ( "LSR", Lsr, ZeroPageX, 6 ),new ( "???", Unknown, Implied, 6 ),new ( "CLI", Cli, Implied, 2 ),new ( "EOR", Eor, AbsoluteY, 4 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 7 ),new ( "???", Nop, Implied, 4 ),new ( "EOR", Eor, AbsoluteX, 4 ),new ( "LSR", Lsr, AbsoluteX, 7 ),new ( "???", Unknown, Implied, 7 ),
            new ( "RTS", Rts, Implied, 6 ),new ( "ADC", Adc, IndirectX, 6 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 3 ),new ( "ADC", Adc, ZeroPage, 3 ),new ( "ROR", Ror, ZeroPage, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "PLA", Pla, Implied, 4 ),new ( "ADC", Adc, Immediate, 2 ),new ( "ROR", Ror, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "JMP", Jmp, Indirect, 5 ),new ( "ADC", Adc, Absolute, 4 ),new ( "ROR", Ror, Absolute, 6 ),new ( "???", Unknown, Implied, 6 ),
            new ( "BVS", Bvs, Relative, 2 ),new ( "ADC", Adc, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 4 ),new ( "ADC", Adc, ZeroPageX, 4 ),new ( "ROR", Ror, ZeroPageX, 6 ),new ( "???", Unknown, Implied, 6 ),new ( "SEI", Sei, Implied, 2 ),new ( "ADC", Adc, AbsoluteY, 4 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 7 ),new ( "???", Nop, Implied, 4 ),new ( "ADC", Adc, AbsoluteX, 4 ),new ( "ROR", Ror, AbsoluteX, 7 ),new ( "???", Unknown, Implied, 7 ),
            new ( "???", Nop, Implied, 2 ),new ( "STA", Sta, IndirectX, 6 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 6 ),new ( "STY", Sty, ZeroPage, 3 ),new ( "STA", Sta, ZeroPage, 3 ),new ( "STX", Stx, ZeroPage, 3 ),new ( "???", Unknown, Implied, 3 ),new ( "DEY", Dey, Implied, 2 ),new ( "???", Nop, Implied, 2 ),new ( "TXA", Txa, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "STY", Sty, Absolute, 4 ),new ( "STA", Sta, Absolute, 4 ),new ( "STX", Stx, Absolute, 4 ),new ( "???", Unknown, Implied, 4 ),
            new ( "BCC", Bcc, Relative, 2 ),new ( "STA", Sta, IndirectY, 6 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 6 ),new ( "STY", Sty, ZeroPageX, 4 ),new ( "STA", Sta, ZeroPageX, 4 ),new ( "STX", Stx, ZeroPageY, 4 ),new ( "???", Unknown, Implied, 4 ),new ( "TYA", Tya, Implied, 2 ),new ( "STA", Sta, AbsoluteY, 5 ),new ( "TXS", Txs, Implied, 2 ),new ( "???", Unknown, Implied, 5 ),new ( "???", Nop, Implied, 5 ),new ( "STA", Sta, AbsoluteX, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "???", Unknown, Implied, 5 ),
            new ( "LDY", Ldy, Immediate, 2 ),new ( "LDA", Lda, IndirectX, 6 ),new ( "LDX", Ldx, Immediate, 2 ),new ( "???", Unknown, Implied, 6 ),new ( "LDY", Ldy, ZeroPage, 3 ),new ( "LDA", Lda, ZeroPage, 3 ),new ( "LDX", Ldx, ZeroPage, 3 ),new ( "???", Unknown, Implied, 3 ),new ( "TAY", Tay, Implied, 2 ),new ( "LDA", Lda, Immediate, 2 ),new ( "TAX", Tax, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "LDY", Ldy, Absolute, 4 ),new ( "LDA", Lda, Absolute, 4 ),new ( "LDX", Ldx, Absolute, 4 ),new ( "???", Unknown, Implied, 4 ),
            new ( "BCS", Bcs, Relative, 2 ),new ( "LDA", Lda, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 5 ),new ( "LDY", Ldy, ZeroPageX, 4 ),new ( "LDA", Lda, ZeroPageX, 4 ),new ( "LDX", Ldx, ZeroPageY, 4 ),new ( "???", Unknown, Implied, 4 ),new ( "CLV", Clv, Implied, 2 ),new ( "LDA", Lda, AbsoluteY, 4 ),new ( "TSX", Tsx, Implied, 2 ),new ( "???", Unknown, Implied, 4 ),new ( "LDY", Ldy, AbsoluteX, 4 ),new ( "LDA", Lda, AbsoluteX, 4 ),new ( "LDX", Ldx, AbsoluteY, 4 ),new ( "???", Unknown, Implied, 4 ),
            new ( "CPY", Cpy, Immediate, 2 ),new ( "CMP", Cmp, IndirectX, 6 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "CPY", Cpy, ZeroPage, 3 ),new ( "CMP", Cmp, ZeroPage, 3 ),new ( "DEC", Dec, ZeroPage, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "INY", Iny, Implied, 2 ),new ( "CMP", Cmp, Immediate, 2 ),new ( "DEX", Dex, Implied, 2 ),new ( "???", Unknown, Implied, 2 ),new ( "CPY", Cpy, Absolute, 4 ),new ( "CMP", Cmp, Absolute, 4 ),new ( "DEC", Dec, Absolute, 6 ),new ( "???", Unknown, Implied, 6 ),
            new ( "BNE", Bne, Relative, 2 ),new ( "CMP", Cmp, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 4 ),new ( "CMP", Cmp, ZeroPageX, 4 ),new ( "DEC", Dec, ZeroPageX, 6 ),new ( "???", Unknown, Implied, 6 ),new ( "CLD", Cld, Implied, 2 ),new ( "CMP", Cmp, AbsoluteY, 4 ),new ( "NOP", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 7 ),new ( "???", Nop, Implied, 4 ),new ( "CMP", Cmp, AbsoluteX, 4 ),new ( "DEC", Dec, AbsoluteX, 7 ),new ( "???", Unknown, Implied, 7 ),
            new ( "CPX", Cpx, Immediate, 2 ),new ( "SBC", Sbc, IndirectX, 6 ),new ( "???", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "CPX", Cpx, ZeroPage, 3 ),new ( "SBC", Sbc, ZeroPage, 3 ),new ( "INC", Inc, ZeroPage, 5 ),new ( "???", Unknown, Implied, 5 ),new ( "INX", Inx, Implied, 2 ),new ( "SBC", Sbc, Immediate, 2 ),new ( "NOP", Nop, Implied, 2 ),new ( "???", Sbc, Implied, 2 ),new ( "CPX", Cpx, Absolute, 4 ),new ( "SBC", Sbc, Absolute, 4 ),new ( "INC", Inc, Absolute, 6 ),new ( "???", Unknown, Implied, 6 ),
            new ( "BEQ", Beq, Relative, 2 ),new ( "SBC", Sbc, IndirectY, 5 ),new ( "???", Unknown, Implied, 2 ),new ( "???", Unknown, Implied, 8 ),new ( "???", Nop, Implied, 4 ),new ( "SBC", Sbc, ZeroPageX, 4 ),new ( "INC", Inc, ZeroPageX, 6 ),new ( "???", Unknown, Implied, 6 ),new ( "SED", Sed, Implied, 2 ),new ( "SBC", Sbc, AbsoluteY, 4 ),new ( "NOP", Nop, Implied, 2 ),new ( "???", Unknown, Implied, 7 ),new ( "???", Nop, Implied, 4 ),new ( "SBC", Sbc, AbsoluteX, 4 ),new ( "INC", Inc, AbsoluteX, 7 ),new ( "???", Unknown, Implied, 7 ),
        };
    }
    #endregion
}