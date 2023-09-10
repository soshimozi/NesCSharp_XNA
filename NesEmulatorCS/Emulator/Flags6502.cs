namespace NesEmulatorCS.Emulator;

public enum Flags6502
{
    C = 1 << 0,   // Carry bit
    Z = 1 << 1,   // Zero
    I = 1 << 2,   // Disable interrupts (except NMI)
    D = 1 << 3,   // Decimal mode (not used for this example)
    B = 1 << 4,   // Break
    U = 1 << 5,   // Unused
    V = 1 << 6,   // Overflow
    N = 1 << 7,   // Negative
}