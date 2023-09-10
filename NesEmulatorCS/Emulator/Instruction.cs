using System;

namespace NesEmulatorCS.Emulator;

record Instruction(string Mnemonic, Func<byte> Operate = null, Func<byte> AddressMode = null, byte Cycles = 0);