using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;

namespace NesEmulatorCS.Emulator;


public enum MirrorType
{
    Horizontal,
    Vertical,
    OneScreenLO,
    OneScreenHI,
}
public class Cartridge
{
    // iNES Format Header
    struct SHeader
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
        public char[] Name;
        public byte PrgRomChunks;
        public byte ChrRomChunks;
        public byte Mapper1;
        public byte Mapper2;
        public byte PrgRamSize;
        public byte TvSystem1;
        public byte TvSystem2;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 5)]
        public char[] Unused;
    }

    private readonly bool _imageValid;

    private readonly byte _prgBanks = 0;
    private readonly byte _chrBanks = 0;

    private readonly List<byte> _prgMemory;
    private readonly List<byte> _chrMemory;

    private readonly Mapper _mapper;
    private readonly int _mapperId = 0;

    public MirrorType Mirror = MirrorType.Horizontal;

    public Cartridge(string fileName)
    {
        _imageValid = false;

        using FileStream ifs = new FileStream(fileName, FileMode.Open, FileAccess.Read);

        // Read file header
        var headerBytes = new byte[Marshal.SizeOf(typeof(SHeader))];
        ifs.Read(headerBytes, 0, headerBytes.Length);
        var handle = GCHandle.Alloc(headerBytes, GCHandleType.Pinned);

        var marshalledObject = Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof(SHeader));

        if (marshalledObject == null) return;

        var header = (SHeader)marshalledObject;

        handle.Free();

        // If a "trainer" exists we just need to read past
        // it before we get to the good stuff
        if ((header.Mapper1 & 0x04) != 0)
            ifs.Seek(512, SeekOrigin.Current);

        // Determine Mapper ID
        _mapperId = header.Mapper2 >> 4 << 4 | header.Mapper1 >> 4;
        Mirror = (header.Mapper1 & 0x01) != 0 ? MirrorType.Vertical : MirrorType.Horizontal;

        // "Discover" File Format
        byte nFileType = 1;

        if (nFileType == 0)
        {

        }

        if (nFileType == 1)
        {
            _prgBanks = header.PrgRomChunks;
            _prgMemory = new List<byte>(_prgBanks * 16384);

            byte[] prgData = new byte[_prgBanks * 16384];
            ifs.Read(prgData, 0, prgData.Length);
            _prgMemory.AddRange(prgData);

            _chrBanks = header.ChrRomChunks;
            _chrMemory = new List<byte>(_chrBanks * 8192);

            byte[] chrData = new byte[_chrBanks * 8192];
            ifs.Read(chrData, 0, chrData.Length);
            _chrMemory.AddRange(chrData);
        }

        if (nFileType == 2)
        {

        }

        // Load appropriate mapper
        switch (_mapperId)
        {
            case 0:
                _mapper = new Mapper000(_prgBanks, _chrBanks);
                break;
        }

        _imageValid = true;
    }

    public bool CpuRead(ushort address, ref byte data)
    {
        uint mappedAddr = 0;
        if (!_mapper.CpuMapRead(address, ref mappedAddr)) return false;

        data = _prgMemory[(int)mappedAddr];
        return true;

    }

    public bool CpuWrite(ushort address, byte data)
    {
        uint mappedAddr = 0;
        if (!_mapper.CpuMapWrite(address, ref mappedAddr)) return false;
        _prgMemory[(int)mappedAddr] = data;
        return true;

    }

    public bool PpuRead(ushort address, ref byte data)
    {
        uint mappedAddr = 0;
        if (!_mapper.PpuMapRead(address, ref mappedAddr)) return false;
        data = _chrMemory[(int)mappedAddr];
        return true;
    }

    public bool PpuWrite(ushort address, byte data)
    {
        uint mappedAddr = 0;
        if (!_mapper.PpuMapWrite(address, ref mappedAddr)) return false;
        _chrMemory[(int)mappedAddr] = data;
        return true;

    }

    public bool ImageValid()
    {
        return _imageValid;
    }

    public void Reset()
    {
        // Note: This does not reset the ROM contents,
        // but does reset the mapper.
        _mapper?.Reset();

    }

}