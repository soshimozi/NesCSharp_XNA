namespace NesEmulatorCS.Emulator;

public class PpuStatus
{
    public enum BitFlag
    {
        UnusedStart = 0,    // You can define the range of the unused bits for clarity, but you might not use these enums in actual code.
        UnusedEnd = 4,
        SpriteOverflow = 5,
        SpriteZeroHit = 6,
        VerticalBlank = 7
    }

    private byte _status;

    public byte Value
    {
        get => _status;
        set => _status = value;
    }

    private bool GetFlag(BitFlag flag)
    {
        return (_status & 1 << (int)flag) != 0;
    }

    private void SetFlag(BitFlag flag, bool value)
    {
        if (value)
            _status |= (byte)(1 << (int)flag);
        else
            _status &= (byte)~(1 << (int)flag);
    }

    // Direct property access for each bit:
    // Note: We skip properties for the unused bits as they don't have meaningful operations.

    public bool SpriteOverflow
    {
        get => GetFlag(BitFlag.SpriteOverflow);
        set => SetFlag(BitFlag.SpriteOverflow, value);
    }

    public bool SpriteZeroHit
    {
        get => GetFlag(BitFlag.SpriteZeroHit);
        set => SetFlag(BitFlag.SpriteZeroHit, value);
    }

    public bool VerticalBlank
    {
        get => GetFlag(BitFlag.VerticalBlank);
        set => SetFlag(BitFlag.VerticalBlank, value);
    }
}
