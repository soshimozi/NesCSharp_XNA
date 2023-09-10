namespace NesEmulatorCS.Emulator;

public class Ppuctrl
{
    private enum BitFlag
    {
        NameTableX = 0,
        NameTableY = 1,
        IncrementMode = 2,
        PatternSprite = 3,
        PatternBackground = 4,
        SpriteSize = 5,
        SlaveMode = 6, // unused
        EnableNmi = 7
    }

    public byte Reg { get; set; }

    private bool GetFlag(BitFlag flag)
    {
        return (Reg & 1 << (int)flag) != 0;
    }

    private void SetFlag(BitFlag flag, bool value)
    {
        if (value)
            Reg |= (byte)(1 << (int)flag);
        else
            Reg &= (byte)~(1 << (int)flag);
    }

    // Direct property access for each bit:
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

    public bool IncrementMode
    {
        get => GetFlag(BitFlag.IncrementMode);
        set => SetFlag(BitFlag.IncrementMode, value);
    }

    public bool PatternSprite
    {
        get => GetFlag(BitFlag.PatternSprite);
        set => SetFlag(BitFlag.PatternSprite, value);
    }

    public bool PatternBackground
    {
        get => GetFlag(BitFlag.PatternBackground);
        set => SetFlag(BitFlag.PatternBackground, value);
    }

    public bool SpriteSize
    {
        get => GetFlag(BitFlag.SpriteSize);
        set => SetFlag(BitFlag.SpriteSize, value);
    }

    public bool SlaveMode
    {
        get => GetFlag(BitFlag.SlaveMode);
        set => SetFlag(BitFlag.SlaveMode, value);
    }

    public bool EnableNmi
    {
        get => GetFlag(BitFlag.EnableNmi);
        set => SetFlag(BitFlag.EnableNmi, value);
    }
}
