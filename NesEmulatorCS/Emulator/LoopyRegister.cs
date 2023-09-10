namespace NesEmulatorCS.Emulator;

public class LoopyRegister
{
    public enum BitFlag
    {
        CoarseXStart = 0,
        CoarseXEnd = 4,
        CoarseYStart = 5,
        CoarseYEnd = 9,
        NameTableX = 10,
        NameTableY = 11,
        FineYStart = 12,
        FineYEnd = 14,
        Unused = 15
    }

    private ushort _register = 0x0000;

    public ushort Value
    {
        get => _register;
        set => _register = value;
    }

    private bool GetFlag(BitFlag flag)
    {
        return (_register & 1 << (int)flag) != 0;
    }

    private ushort GetFlagRange(BitFlag start, BitFlag end)
    {
        int length = end - start + 1;
        int mask = (1 << length) - 1 << (int)start;
        return (ushort)((_register & mask) >> (int)start);
    }

    private void SetFlag(BitFlag flag, bool value)
    {
        if (value)
            _register |= (ushort)(1 << (int)flag);
        else
            _register &= (ushort)~(1 << (int)flag);
    }

    private void SetFlagRange(BitFlag start, BitFlag end, ushort value)
    {
        int length = end - start + 1;
        int mask = (1 << length) - 1 << (int)start;
        _register = (ushort)(_register & ~mask | value << (int)start & mask);
    }

    // Direct property access for each bit and range:
    public ushort CoarseX
    {
        get => GetFlagRange(BitFlag.CoarseXStart, BitFlag.CoarseXEnd);
        set => SetFlagRange(BitFlag.CoarseXStart, BitFlag.CoarseXEnd, value);
    }

    public ushort CoarseY
    {
        get => GetFlagRange(BitFlag.CoarseYStart, BitFlag.CoarseYEnd);
        set => SetFlagRange(BitFlag.CoarseYStart, BitFlag.CoarseYEnd, value);
    }

    public bool NameTableX
    {
        get => GetFlag(BitFlag.NameTableX);
        set => SetFlag(BitFlag.NameTableX, value);
    }

    public bool NameTableY
    {
        get => GetFlag(BitFlag.NameTableY);
        set => SetFlag(BitFlag.NameTableY, value);
    }

    public ushort FineY
    {
        get => GetFlagRange(BitFlag.FineYStart, BitFlag.FineYEnd);
        set => SetFlagRange(BitFlag.FineYStart, BitFlag.FineYEnd, value);
    }

    // Assuming you don't need direct access to the unused bit. If you do, you can add it.
}
