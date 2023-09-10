namespace NesEmulatorCS.Emulator;

public abstract class Mapper
{
    protected readonly byte PrgBanks = 0;
    protected readonly byte ChrBanks = 0;

    protected Mapper(byte prgBanks, byte chrBanks)
    {
        PrgBanks = prgBanks;
        ChrBanks = chrBanks;

    }

    public abstract bool CpuMapRead(ushort address, ref uint mappedAddress);
    public abstract bool CpuMapWrite(ushort address, ref uint mappedAddress);

    public abstract bool PpuMapRead(ushort address, ref uint mappedAddress);
    public abstract bool PpuMapWrite(ushort address, ref uint mappedAddress);

    public abstract void Reset();

}