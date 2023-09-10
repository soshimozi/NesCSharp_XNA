namespace NesEmulatorCS.Emulator;

public class Mapper000 : Mapper
{
    public Mapper000(byte prgBanks, byte chrBanks) : base(prgBanks, chrBanks)
    {
    }

    public override bool CpuMapRead(ushort address, ref uint mappedAddress)
    {
        // if PRGROM is 16KB
        //     CPU Address Bus          PRG ROM
        //     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
        //     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
        // if PRGROM is 32KB
        //     CPU Address Bus          PRG ROM
        //     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
        if (address < 0x8000) return false;
        mappedAddress = (uint)(address & (PrgBanks > 1 ? 0x7FFF : 0x3FFF));
        return true;


    }

    public override bool CpuMapWrite(ushort address, ref uint mappedAddress)
    {
        if (address < 0x8000) return false;
        mappedAddress = (uint)(address & (PrgBanks > 1 ? 0x7FFF : 0x3FFF));
        return true;

    }

    public override bool PpuMapRead(ushort address, ref uint mappedAddress)
    {
        // There is no mapping required for PPU
        // PPU Address Bus          CHR ROM
        // 0x0000 -> 0x1FFF: Map    0x0000 -> 0x1FFF
        if (address > 0x1FFF) return false;
        mappedAddress = address;
        return true;

    }

    public override bool PpuMapWrite(ushort address, ref uint mappedAddress)
    {
        if (address > 0x1FFF) return false;
        if (ChrBanks != 0) return false;

        // Treat as RAM
        mappedAddress = address;
        return true;

    }

    public override void Reset()
    {
    }
}