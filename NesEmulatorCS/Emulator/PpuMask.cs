namespace NesEmulatorCS.Emulator;

public class PpuMask
{
    public enum BitFlag
    {
        Grayscale = 0,
        RenderBackgroundLeft = 1,
        RenderSpritesLeft = 2,
        RenderBackground = 3,
        RenderSprites = 4,
        EnhanceRed = 5,
        EnhanceGreen = 6,
        EnhanceBlue = 7
    }

    private byte _mask;

    public byte Value
    {
        get => _mask;
        set => _mask = value;
    }

    private bool GetFlag(BitFlag flag)
    {
        return (_mask & 1 << (int)flag) != 0;
    }

    private void SetFlag(BitFlag flag, bool value)
    {
        if (value)
            _mask |= (byte)(1 << (int)flag);
        else
            _mask &= (byte)~(1 << (int)flag);
    }

    // Direct property access for each bit:
    public bool Grayscale
    {
        get => GetFlag(BitFlag.Grayscale);
        set => SetFlag(BitFlag.Grayscale, value);
    }

    public bool RenderBackgroundLeft
    {
        get => GetFlag(BitFlag.RenderBackgroundLeft);
        set => SetFlag(BitFlag.RenderBackgroundLeft, value);
    }

    public bool RenderSpritesLeft
    {
        get => GetFlag(BitFlag.RenderSpritesLeft);
        set => SetFlag(BitFlag.RenderSpritesLeft, value);
    }

    public bool RenderBackground
    {
        get => GetFlag(BitFlag.RenderBackground);
        set => SetFlag(BitFlag.RenderBackground, value);
    }

    public bool RenderSprites
    {
        get => GetFlag(BitFlag.RenderSprites);
        set => SetFlag(BitFlag.RenderSprites, value);
    }

    public bool EnhanceRed
    {
        get => GetFlag(BitFlag.EnhanceRed);
        set => SetFlag(BitFlag.EnhanceRed, value);
    }

    public bool EnhanceGreen
    {
        get => GetFlag(BitFlag.EnhanceGreen);
        set => SetFlag(BitFlag.EnhanceGreen, value);
    }

    public bool EnhanceBlue
    {
        get => GetFlag(BitFlag.EnhanceBlue);
        set => SetFlag(BitFlag.EnhanceBlue, value);
    }

}