using System;
using Microsoft.Xna.Framework.Graphics;

namespace NesEmulatorCS.Emulator;

public class FamicomSystem
{
    public readonly Olc6502 Cpu = new();

    private Cartridge Cartridge { get; set; }

    // 2KB of RAM
    private readonly byte[] _cpuRam = new byte[2048];

    private uint _systemClockCounter = 0;

    // The 2C02 Picture Processing Unit
    public readonly Olc2C02 Ppu; // = new olc2C02();

    private readonly byte[] _controllerState = new byte[2];
    public FamicomSystem(GraphicsDevice device)
    {
        Ppu = new Olc2C02(device);

        Cpu.ConnectBus(this);
        Array.Clear(_cpuRam, 0, _cpuRam.Length);
    }

    public void CpuWrite(ushort address, byte data)
    {
        if (Cartridge.CpuWrite(address, data))
        {
            // The cartridge "sees all" and has the facility to veto
            // the propagation of the bus transaction if it requires.
            // This allows the cartridge to map any address to some
            // other data, including the facility to divert transactions
            // with other physical devices. The NES does not do this
            // but I figured it might be quite a flexible way of adding
            // "custom" hardware to the NES in the future!
        }
        else if (address >= 0x0000 && address <= 0x1FFF)
        {
            // System RAM Address Range. The range covers 8KB, though
            // there is only 2KB available. That 2KB is "mirrored"
            // through this address range. Using bitwise AND to mask
            // the bottom 11 bits is the same as addr % 2048.
            _cpuRam[address & 0x07FF] = data;

        }
        else if (address >= 0x2000 && address <= 0x3FFF)
        {
            // PPU Address range. The PPU only has 8 primary registers
            // and these are repeated throughout this range. We can
            // use bitwise AND operation to mask the bottom 3 bits, 
            // which is the equivalent of addr % 8.
            Ppu.CpuWrite((ushort)(address & 0x0007), data);
        }
        else if (address >= 0x4016 && address <= 0x4017)
        {
            _controllerState[address & 0x0001] = _controllerState[address & 0x0001];
        }
    }

    public byte CpuRead(ushort address, bool readOnly = false)
    {
        byte data = 0x00;
        if (Cartridge.CpuRead(address, ref data))
        {
            // Cartridge Address Range
        }
        else if (address >= 0x0000 && address <= 0x1FFF)
        {
            // System RAM Address Range, mirrored every 2048
            data = _cpuRam[address & 0x07FF];
        }
        else if (address >= 0x2000 && address <= 0x3FFF)
        {
            // PPU Address range, mirrored every 8
            data = Ppu.CpuRead((ushort)(address & 0x0007), readOnly);
        }
        else if (address >= 0x4016 && address <= 0x4017)
        {
            data = (_controllerState[address & 0x0001] & 0x80) > 0 ? (byte)1 : (byte)0;
        }

        return data;
    }

    public void InsertCartridge(Cartridge cartridge)
    {
        Cartridge = cartridge;
        Ppu.ConnectCartridge(cartridge);
    }

    public void Reset()
    {
        Cartridge.Reset();
        Cpu.Reset();
        Ppu.Reset();
        _systemClockCounter = 0;
    }

    public void Clock()
    {
        // Clocking. The heart and soul of an emulator. The running
        // frequency is controlled by whatever calls this function.
        // So here we "divide" the clock as necessary and call
        // the peripheral devices clock() function at the correct
        // times.

        // The fastest clock frequency the digital system cares
        // about is equivalent to the PPU clock. So the PPU is clocked
        // each time this function is called.
        Ppu.Clock();

        // The CPU runs 3 times slower than the PPU so we only call its
        // clock() function every 3 times this function is called. We
        // have a global counter to keep track of this.
        if (_systemClockCounter % 3 == 0)
        {
            Cpu.Clock();
        }

        // The PPU is capable of emitting an interrupt to indicate the
        // vertical blanking period has been entered. If it has, we need
        // to send that irq to the CPU.
        if (Ppu.Nmi)
        {
            Ppu.Nmi = false;
            Cpu.Nmi();
        }



        _systemClockCounter++;
    }
}