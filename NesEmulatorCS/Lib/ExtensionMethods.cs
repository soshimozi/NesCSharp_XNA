namespace NesEmulatorCS.Lib;

public static class ExtensionMethods
{
    public static string ToHexString(this int value, int length)
    {
        var s = new string('0', length);
        for (var i = length - 1; i >= 0; i--, value >>= 4)
            s = s.Remove(i, 1).Insert(i, "0123456789ABCDEF"[(int)(value & 0xF)].ToString());
        return s;
    }
}