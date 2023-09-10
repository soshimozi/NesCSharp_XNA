using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using NesEmulatorCS.Lib;

namespace NesEmulatorCS.Emulator;

public class Olc2C02
{

    private Cartridge _cart;

    // Pixel "dot" position information
    private short _scanline = 0;
    private short _cycle = 0;

    public bool FrameComplete { get; set; }

    private readonly Random _random = new Random();


    private readonly byte[,] _tblName = new byte[2, 1024];
    private readonly byte[,] _tblPattern = new byte[2, 4096];
    private readonly byte[] _tblPalette = new byte[32];

    private readonly Color[] _palScreen = new Color[0x40];

    // In Video
    private readonly Sprite _sprScreen; // = new Sprite(256, 240);
    private readonly Sprite[] _sprNameTable; // =  new [] { new Sprite(256, 240), new Sprite(256, 240) };
    private readonly Sprite[] _sprPatternTable; // = new [] { new Sprite(128, 128), new Sprite(128, 128) };

    readonly PpuStatus _status = new PpuStatus();
    readonly PpuMask _mask = new PpuMask();
    readonly Ppuctrl _contrl = new Ppuctrl();

    LoopyRegister _vramAddr = new LoopyRegister(); // Active "pointer" address into nametable to extract background tile info
    readonly LoopyRegister _tramAddr = new LoopyRegister(); // Temporary store of information to be "transferred" into "pointer" at various times

    // Pixel offset horizontally
    byte _fineX = 0x00;

    // Internal communications
    byte _addressLatch = 0x00;
    byte _ppuDATABuffer = 0x00;

    public bool Nmi = false;

    // Background rendering
    byte _bgNextTileId = 0x00;
    byte _bgNextTileAttrib = 0x00;
    byte _bgNextTileLsb = 0x00;
    byte _bgNextTileMsb = 0x00;
    short _bgShifterPatternLO = 0x0000;
    short _bgShifterPatternHI = 0x0000;
    short _bgShifterAttribLO = 0x0000;
    short _bgShifterAttribHI = 0x0000;

    public Olc2C02(GraphicsDevice device)
    {
        _palScreen[0x00] = new Color(84, 84, 84);
        _palScreen[0x01] = new Color(0, 30, 116);
        _palScreen[0x02] = new Color(8, 16, 144);
        _palScreen[0x03] = new Color(48, 0, 136);
        _palScreen[0x04] = new Color(68, 0, 100);
        _palScreen[0x05] = new Color(92, 0, 48);
        _palScreen[0x06] = new Color(84, 4, 0);
        _palScreen[0x07] = new Color(60, 24, 0);
        _palScreen[0x08] = new Color(32, 42, 0);
        _palScreen[0x09] = new Color(8, 58, 0);
        _palScreen[0x0A] = new Color(0, 64, 0);
        _palScreen[0x0B] = new Color(0, 60, 0);
        _palScreen[0x0C] = new Color(0, 50, 60);
        _palScreen[0x0D] = new Color(0, 0, 0);
        _palScreen[0x0E] = new Color(0, 0, 0);
        _palScreen[0x0F] = new Color(0, 0, 0);

        _palScreen[0x10] = new Color(152, 150, 152);
        _palScreen[0x11] = new Color(8, 76, 196);
        _palScreen[0x12] = new Color(48, 50, 236);
        _palScreen[0x13] = new Color(92, 30, 228);
        _palScreen[0x14] = new Color(136, 20, 176);
        _palScreen[0x15] = new Color(160, 20, 100);
        _palScreen[0x16] = new Color(152, 34, 32);
        _palScreen[0x17] = new Color(120, 60, 0);
        _palScreen[0x18] = new Color(84, 90, 0);
        _palScreen[0x19] = new Color(40, 114, 0);
        _palScreen[0x1A] = new Color(8, 124, 0);
        _palScreen[0x1B] = new Color(0, 118, 40);
        _palScreen[0x1C] = new Color(0, 102, 120);
        _palScreen[0x1D] = new Color(0, 0, 0);
        _palScreen[0x1E] = new Color(0, 0, 0);
        _palScreen[0x1F] = new Color(0, 0, 0);

        _palScreen[0x20] = new Color(236, 238, 236);
        _palScreen[0x21] = new Color(76, 154, 236);
        _palScreen[0x22] = new Color(120, 124, 236);
        _palScreen[0x23] = new Color(176, 98, 236);
        _palScreen[0x24] = new Color(228, 84, 236);
        _palScreen[0x25] = new Color(236, 88, 180);
        _palScreen[0x26] = new Color(236, 106, 100);
        _palScreen[0x27] = new Color(212, 136, 32);
        _palScreen[0x28] = new Color(160, 170, 0);
        _palScreen[0x29] = new Color(116, 196, 0);
        _palScreen[0x2A] = new Color(76, 208, 32);
        _palScreen[0x2B] = new Color(56, 204, 108);
        _palScreen[0x2C] = new Color(56, 180, 204);
        _palScreen[0x2D] = new Color(60, 60, 60);
        _palScreen[0x2E] = new Color(0, 0, 0);
        _palScreen[0x2F] = new Color(0, 0, 0);

        _palScreen[0x30] = new Color(236, 238, 236);
        _palScreen[0x31] = new Color(168, 204, 236);
        _palScreen[0x32] = new Color(188, 188, 236);
        _palScreen[0x33] = new Color(212, 178, 236);
        _palScreen[0x34] = new Color(236, 174, 236);
        _palScreen[0x35] = new Color(236, 174, 212);
        _palScreen[0x36] = new Color(236, 180, 176);
        _palScreen[0x37] = new Color(228, 196, 144);
        _palScreen[0x38] = new Color(204, 210, 120);
        _palScreen[0x39] = new Color(180, 222, 120);
        _palScreen[0x3A] = new Color(168, 226, 144);
        _palScreen[0x3B] = new Color(152, 226, 180);
        _palScreen[0x3C] = new Color(160, 214, 228);
        _palScreen[0x3D] = new Color(160, 162, 160);
        _palScreen[0x3E] = new Color(0, 0, 0);
        _palScreen[0x3F] = new Color(0, 0, 0);


        _sprScreen = Sprite.CreateSprite(device, 256, 240);
        _sprNameTable = new[] { Sprite.CreateSprite(device, 256, 240), Sprite.CreateSprite(device, 256, 240) };
        _sprPatternTable = new[] { Sprite.CreateSprite(device, 128, 128), Sprite.CreateSprite(device, 128, 128) };
    }

    public Sprite GetScreen()
    {
        return _sprScreen;
    }

    public Sprite GetNameTable(int index)
    {
        return _sprNameTable[index];
    }

    public Sprite GetPatternTable(int index, byte palette)
    {
        // This function draw the CHR ROM for a given pattern table into
        // an olc::Sprite, using a specified palette. Pattern tables consist
        // of 16x16 "tiles or characters". It is independent of the running
        // emulation and using it does not change the systems state, though
        // it gets all the data it needs from the live system. Consequently,
        // if the game has not yet established palettes or mapped to relevant
        // CHR ROM banks, the sprite may look empty. This approach permits a 
        // "live" extraction of the pattern table exactly how the NES, and 
        // ultimately the player would see it.

        // A tile consists of 8x8 pixels. On the NES, pixels are 2 bits, which
        // gives an index into 4 different colours of a specific palette. There
        // are 8 palettes to choose from. Colour "0" in each palette is effectively
        // considered transparent, as those locations in memory "mirror" the global
        // background colour being used. This mechanics of this are shown in 
        // detail in ppuRead() & ppuWrite()

        // Characters on NES
        // ~~~~~~~~~~~~~~~~~
        // The NES stores characters using 2-bit pixels. These are not stored sequentially
        // but in singular bit planes. For example:
        //
        // 2-Bit Pixels       LSB Bit Plane     MSB Bit Plane
        // 0 0 0 0 0 0 0 0	  0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0
        // 0 1 1 0 0 1 1 0	  0 1 1 0 0 1 1 0   0 0 0 0 0 0 0 0
        // 0 1 2 0 0 2 1 0	  0 1 1 0 0 1 1 0   0 0 1 0 0 1 0 0
        // 0 0 0 0 0 0 0 0 =  0 0 0 0 0 0 0 0 + 0 0 0 0 0 0 0 0
        // 0 1 1 0 0 1 1 0	  0 1 1 0 0 1 1 0   0 0 0 0 0 0 0 0
        // 0 0 1 1 1 1 0 0	  0 0 1 1 1 1 0 0   0 0 0 0 0 0 0 0
        // 0 0 0 2 2 0 0 0	  0 0 0 1 1 0 0 0   0 0 0 1 1 0 0 0
        // 0 0 0 0 0 0 0 0	  0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0
        //
        // The planes are stored as 8 bytes of LSB, followed by 8 bytes of MSB

        // Loop through all 16x16 tiles
        for (int tileY = 0; tileY < 16; tileY++)
        {
            for (var tileX = 0; tileX < 16; tileX++)
            {
                var offset = tileY * 256 + tileX * 16;

                // now loop thru 8 rows of 8 pixels
                for (var row = 0; row < 8; row++)
                {
                    // For each row, we need to read both bit planes of the character
                    // in order to extract the least significant and most significant 
                    // bits of the 2 bit pixel value. in the CHR ROM, each character
                    // is stored as 64 bits of lsb, followed by 64 bits of msb. This
                    // conveniently means that two corresponding rows are always 8
                    // bytes apart in memory.
                    var tileLsb = PpuRead((ushort)(index * 0x100 + offset + row + 0x0000));
                    var tileMsb = PpuRead((ushort)(index * 0x100 + offset + row + 0x0008));

                    // Now we have a single row of the two bit planes for the character
                    // we need to iterate through the 8-bit words, combining them to give
                    // us the final pixel index
                    for (var col = 0; col < 8; col++)
                    {
                        // We can get the index value by simply adding the bits together
                        // but we're only interested in the lsb of the row words because...
                        var pixel = (byte)((tileLsb & 0x01) + (tileMsb & 0x01));

                        // ... we will shift the row words 1 bit right for each column of
                        // the character
                        tileLsb >>= 1; tileMsb >>= 1;

                        // Now we know the location and NES pixel value for a specific location
                        // in the pattern table, we can translate that to a screen colour, and an
                        // (x,y) location in the sprite
                        _sprPatternTable[index].SetPixel
                        (
                            tileX * 8 + (7 - col), // Because we are using the lsb of the row word first
                                                   // we are effectively reading the row from right
                                                   // to left, so we need to draw the row "backwards"
                            tileY * 8 + row,
                            GetColourFromPaletteRam(palette, pixel)
                        );

                    }
                }
            }
        }

        return _sprPatternTable[index];
    }

    public Color GetColourFromPaletteRam(byte palette, byte pixel)
    {
        // This is a convenience function that takes a specified palette and pixel
        // index and returns the appropriate screen colour.
        // "0x3F00"       - Offset into PPU addressable range where palettes are stored
        // "palette << 2" - Each palette is 4 bytes in size
        // "pixel"        - Each pixel index is either 0, 1, 2 or 3
        // "& 0x3F"       - Stops us reading beyond the bounds of the palScreen array
        return _palScreen[PpuRead((ushort)(0x3F00 + (palette << 2) + pixel)) & 0x3F];

        // Note: We dont access tblPalette directly here, instead we know that ppuRead()
        // will map the address onto the seperate small RAM attached to the PPU bus.
    }

    public void ConnectCartridge(Cartridge cart)
    {
        _cart = cart;
    }

    public byte CpuRead(ushort address, bool readOnly = false)
    {
        byte data = 0x00;

        if (readOnly)
        {
            // Reading from PPU registers can affect their contents
            // so this read only option is used for examining the
            // state of the PPU without changing its state. This is
            // really only used in debug mode.

            switch (address)
            {
                case 0x0000: // Control
                    data = _contrl.Reg;
                    break;
                case 0x0001: // Mask
                    data = _mask.Value;
                    break;
                case 0x0002: // Status
                    data = _status.Value;
                    break;
                case 0x0003: // OAM Address
                    break;
                case 0x0004: // OAM Data
                    break;
                case 0x0005: // Scroll
                    break;
                case 0x0006: // PPU Address
                    break;
                case 0x0007: // PPU Data
                    break;
            }

            return data;
        }
        else
        {
            // These are the live PPU registers that repsond
            // to being read from in various ways. Note that not
            // all the registers are capable of being read from
            // so they just return 0x00
            switch (address)
            {
                // Control - Not readable
                case 0x0000: break;

                // Mask - Not Readable
                case 0x0001: break;

                // Status
                case 0x0002:
                    // Reading from the status register has the effect of resetting
                    // different parts of the circuit. Only the top three bits
                    // contain status information, however it is possible that
                    // some "noise" gets picked up on the bottom 5 bits which 
                    // represent the last PPU bus transaction. Some games "may"
                    // use this noise as valid data (even though they probably
                    // shouldn't)
                    data = (byte)(_status.Value & 0xE0 | _ppuDATABuffer & 0x1F);

                    // Clear the vertical blanking flag
                    _status.VerticalBlank = false;

                    // Reset Loopy's Address latch flag
                    _addressLatch = 0;
                    break;

                // OAM Address
                case 0x0003: break;

                // OAM Data
                case 0x0004: break;

                // Scroll - Not Readable
                case 0x0005: break;

                // PPU Address - Not Readable
                case 0x0006: break;

                // PPU Data
                case 0x0007:
                    // Reads from the NameTable ram get delayed one cycle, 
                    // so output buffer which contains the data from the 
                    // previous read request
                    data = _ppuDATABuffer;
                    // then update the buffer for next time
                    _ppuDATABuffer = PpuRead(_vramAddr.Value);
                    // However, if the address was in the palette range, the
                    // data is not delayed, so it returns immediately
                    if (_vramAddr.Value >= 0x3F00) data = _ppuDATABuffer;
                    // All reads from PPU data automatically increment the nametable
                    // address depending upon the mode set in the control register.
                    // If set to vertical mode, the increment is 32, so it skips
                    // one whole nametable row; in horizontal mode it just increments
                    // by 1, moving to the next column
                    _vramAddr.Value += (ushort)(_contrl.IncrementMode ? 32 : 1);
                    break;
            }
        }

        return data;
    }

    public void CpuWrite(ushort address, byte data)
    {
        switch (address)
        {
            case 0x0000: // Control
                _contrl.Reg = data;
                _tramAddr.NameTableX = _contrl.NameTableX;
                _tramAddr.NameTableY = _contrl.NameTableY;
                break;
            case 0x0001: // Mask
                _mask.Value = data;
                break;
            case 0x0002: // Status
                break;
            case 0x0003: // OAM Address
                break;
            case 0x0004: // OAM Data
                break;
            case 0x0005: // Scroll
                if (_addressLatch == 0)
                {
                    // First write to scroll register contains X offset in pixel space
                    // which we split into coarse and fine x values
                    _fineX = (byte)(data & 0x07);
                    _tramAddr.CoarseX = (ushort)(data >> 3);
                    _addressLatch = 1;
                }
                else
                {
                    // First write to scroll register contains Y offset in pixel space
                    // which we split into coarse and fine Y values
                    _tramAddr.FineY = (ushort)(data & 0x07);
                    _tramAddr.CoarseY = (ushort)(data >> 3);
                    _addressLatch = 0;
                }
                break;
            case 0x0006: // PPU Address
                if (_addressLatch == 0)
                {
                    // PPU address bus can be accessed by CPU via the ADDR and DATA
                    // registers. The fisrt write to this register latches the high byte
                    // of the address, the second is the low byte. Note the writes
                    // are stored in the tram register...
                    _tramAddr.Value = (ushort)((data & 0x3F) << 8 | _tramAddr.Value & 0x00FF);
                    _addressLatch = 1;
                }
                else
                {
                    // ...when a whole address has been written, the internal vram address
                    // buffer is updated. Writing to the PPU is unwise during rendering
                    // as the PPU will maintam the vram address automatically whilst
                    // rendering the scanline position.
                    _tramAddr.Value = (ushort)(_tramAddr.Value & 0xFF00 | data);
                    _vramAddr = _tramAddr;
                    _addressLatch = 0;
                }
                break;
            case 0x0007: // PPU Data
                PpuWrite(_vramAddr.Value, data);
                // All writes from PPU data automatically increment the nametable
                // address depending upon the mode set in the control register.
                // If set to vertical mode, the increment is 32, so it skips
                // one whole nametable row; in horizontal mode it just increments
                // by 1, moving to the next column
                _vramAddr.Value += (ushort)(_contrl.IncrementMode ? 32 : 1);
                break;
        }
    }

    public byte PpuRead(ushort address, bool readOnly = false)
    {
        byte data = 0x00;
        address &= 0x3FFF;

        if (_cart.PpuRead(address, ref data))
        {

        }
        else if (address >= 0x0000 && address <= 0x1FFF)
        {
            // If the cartridge cant map the address, have
            // a physical location ready here
            data = _tblPattern[(address & 0x1000) >> 12, address & 0x0FFF];
        }
        else if (address >= 0x2000 && address <= 0x3EFF)
        {
            address &= 0x0FFF;

            if (_cart.Mirror == MirrorType.Vertical)
            {
                // Vertical
                if (address >= 0x0000 && address <= 0x03FF)
                    data = _tblName[0, address & 0x03FF];
                if (address >= 0x0400 && address <= 0x07FF)
                    data = _tblName[1, address & 0x03FF];
                if (address >= 0x0800 && address <= 0x0BFF)
                    data = _tblName[0, address & 0x03FF];
                if (address >= 0x0C00 && address <= 0x0FFF)
                    data = _tblName[1, address & 0x03FF];
            }
            else if (_cart.Mirror == MirrorType.Horizontal)
            {
                // Horizontal
                if (address >= 0x0000 && address <= 0x03FF)
                    data = _tblName[0, address & 0x03FF];
                if (address >= 0x0400 && address <= 0x07FF)
                    data = _tblName[0, address & 0x03FF];
                if (address >= 0x0800 && address <= 0x0BFF)
                    data = _tblName[1, address & 0x03FF];
                if (address >= 0x0C00 && address <= 0x0FFF)
                    data = _tblName[1, address & 0x03FF];
            }
        }
        else if (address >= 0x3F00 && address <= 0x3FFF)
        {
            address &= 0x001F;
            if (address == 0x0010) address = 0x0000;
            if (address == 0x0014) address = 0x0004;
            if (address == 0x0018) address = 0x0008;
            if (address == 0x001C) address = 0x000C;
            data = (byte)(_tblPalette[address] & (byte)(_mask.Grayscale ? 0x30 : 0x3F));
        }

        return data;
    }

    public void PpuWrite(ushort address, byte data)
    {
        address &= 0x3FFF;

        if (_cart.PpuWrite(address, data))
        {

        }
        else if (address >= 0x0000 && address <= 0x1FFF)
        {
            _tblPattern[(address & 0x1000) >> 12, address & 0x0FFF] = data;
        }
        else if (address >= 0x2000 && address <= 0x3EFF)
        {
            address &= 0x0FFF;
            if (_cart.Mirror == MirrorType.Vertical)
            {
                // Vertical
                if (address >= 0x0000 && address <= 0x03FF)
                    _tblName[0, address & 0x03FF] = data;
                if (address >= 0x0400 && address <= 0x07FF)
                    _tblName[1, address & 0x03FF] = data;
                if (address >= 0x0800 && address <= 0x0BFF)
                    _tblName[0, address & 0x03FF] = data;
                if (address >= 0x0C00 && address <= 0x0FFF)
                    _tblName[1, address & 0x03FF] = data;
            }
            else if (_cart.Mirror == MirrorType.Horizontal)
            {
                // Horizontal
                if (address >= 0x0000 && address <= 0x03FF)
                    _tblName[0, address & 0x03FF] = data;
                if (address >= 0x0400 && address <= 0x07FF)
                    _tblName[0, address & 0x03FF] = data;
                if (address >= 0x0800 && address <= 0x0BFF)
                    _tblName[1, address & 0x03FF] = data;
                if (address >= 0x0C00 && address <= 0x0FFF)
                    _tblName[1, address & 0x03FF] = data;
            }
        }
        else if (address >= 0x3F00 && address <= 0x3FFF)
        {
            address &= 0x001F;
            if (address == 0x0010) address = 0x0000;
            if (address == 0x0014) address = 0x0004;
            if (address == 0x0018) address = 0x0008;
            if (address == 0x001C) address = 0x000C;
            _tblPalette[address] = data;
        }

    }

    public void Reset()
    {
        _fineX = 0x00;
        _addressLatch = 0x00;
        _ppuDATABuffer = 0x00;
        _scanline = 0;
        _cycle = 0;
        _bgNextTileId = 0x00;
        _bgNextTileAttrib = 0x00;
        _bgNextTileLsb = 0x00;
        _bgNextTileMsb = 0x00;
        _bgShifterPatternLO = 0x0000;
        _bgShifterPatternHI = 0x0000;
        _bgShifterAttribLO = 0x0000;
        _bgShifterAttribHI = 0x0000;
        _status.Value = 0x00;
        _mask.Value = 0x00;
        _contrl.Reg = 0x00;
        _vramAddr.Value = 0x0000;
        _tramAddr.Value = 0x0000;
    }

    public void Clock()
    {
        // As we progress through scanlines and cycles, the PPU is effectively
        // a state machine going through the motions of fetching background 
        // information and sprite information, compositing them into a pixel
        // to be output.

        // The lambda functions (functions inside functions) contain the various
        // actions to be performed depending upon the output of the state machine
        // for a given scanline/cycle combination

        // ==============================================================================
        void IncrementScrollX()
        {
            // Note: pixel perfect scrolling horizontally is handled by the 
            // data shifters. Here we are operating in the spatial domain of 
            // tiles, 8x8 pixel blocks.

            // Ony if rendering is enabled
            if (_mask.RenderBackground || _mask.RenderSprites)
            {
                // A single name table is 32x30 tiles. As we increment horizontally
                // we may cross into a neighbouring nametable, or wrap around to
                // a neighbouring nametable
                if (_vramAddr.CoarseX == 31)
                {
                    // Leaving nametable so wrap address round
                    _vramAddr.CoarseX = 0;

                    // Flip target nametable bit
                    _vramAddr.NameTableX = !_vramAddr.NameTableX;
                }
                else
                {
                    // Staying in current nametable, so just increment

                    _vramAddr.CoarseX++;
                }
            }
        }

        void IncrementScrollY()
        {
            // Incrementing vertically is more complicated. The visible nametable
            // is 32x30 tiles, but in memory there is enough room for 32x32 tiles.
            // The bottom two rows of tiles are in fact not tiles at all, they
            // contain the "attribute" information for the entire table. This is
            // information that describes which palettes are used for different 
            // regions of the nametable.

            // In addition, the NES doesnt scroll vertically in chunks of 8 pixels
            // i.e. the height of a tile, it can perform fine scrolling by using
            // the fine_y component of the register. This means an increment in Y
            // first adjusts the fine offset, but may need to adjust the whole
            // row offset, since fine_y is a value 0 to 7, and a row is 8 pixels high

            // Ony if rendering is enabled
            if (_mask.RenderBackground || _mask.RenderSprites)
            {
                if (_vramAddr.FineY < 7)
                {
                    _vramAddr.FineY++;
                }
                else
                {
                    // If we have gone beyond the height of a row, we need to
                    // increment the row, potentially wrapping into neighbouring
                    // vertical nametables. Dont forget however, the bottom two rows
                    // do not contain tile information. The coarse y offset is used
                    // to identify which row of the nametable we want, and the fine
                    // y offset is the specific "scanline"
                    _vramAddr.FineY = 0;

                    if (_vramAddr.CoarseY == 29)
                    {
                        // We do, so reset coarse y offset
                        _vramAddr.CoarseY = 0;
                        // And flip the target nametable bit
                        _vramAddr.NameTableY = !_vramAddr.NameTableY;
                    }
                    else if (_vramAddr.CoarseY == 31)
                    {
                        // In case the pointer is in the attribute memory, we
                        // just wrap around the current nametable
                        _vramAddr.CoarseY = 0;
                    }
                    else
                    {
                        // None of the above boundary/wrapping conditions apply
                        // so just increment the coarse y offset
                        _vramAddr.CoarseY++;
                    }
                }
            }
        }

        // ==============================================================================
        // Transfer the temporarily stored horizontal nametable access information
        // into the "pointer". Note that fine x scrolling is not part of the "pointer"
        // addressing mechanism
        void TransferAddressX()
        {
            if (_mask.RenderBackground || _mask.RenderSprites)
            {
                _vramAddr.NameTableX = _tramAddr.NameTableX;
                _vramAddr.CoarseX = _tramAddr.CoarseX;
            }
        }

        // ==============================================================================
        // Transfer the temporarily stored vertical nametable access information
        // into the "pointer". Note that fine y scrolling is part of the "pointer"
        // addressing mechanism
        void TransferAddressY()
        {
            if (_mask.RenderBackground || _mask.RenderSprites)
            {
                _vramAddr.FineY = _tramAddr.FineY;
                _vramAddr.NameTableY = _tramAddr.NameTableY;
                _vramAddr.CoarseY = _tramAddr.CoarseY;
            }
        }

        // ==============================================================================
        // Prime the "in-effect" background tile shifters ready for outputting next
        // 8 pixels in scanline.
        void LoadBackgroundShifters()
        {
            // feeding the pixel compositor with the binary information it needs. Its
            // 16 bits wide, because the top 8 bits are the current 8 pixels being drawn
            // and the bottom 8 bits are the next 8 pixels to be drawn. Naturally this means
            // the required bit is always the MSB of the shifter. However, "fine x" scrolling
            // plays a part in this too, whcih is seen later, so in fact we can choose
            // any one of the top 8 bits.
            _bgShifterPatternLO = (short)(_bgShifterPatternLO & 0xFF00 | _bgNextTileLsb);
            _bgShifterPatternHI = (short)(_bgShifterPatternHI & 0xFF00 | _bgNextTileMsb);

            // Attribute bits do not change per pixel, rather they change every 8 pixels
            // but are synchronised with the pattern shifters for convenience, so here
            // we take the bottom 2 bits of the attribute word which represent which 
            // palette is being used for the current 8 pixels and the next 8 pixels, and 
            // "inflate" them to 8 bit words.
            _bgShifterAttribLO = (short)(_bgShifterAttribLO & 0xFF00 | ((_bgNextTileAttrib & 0b01) != 0 ? 0xFF : 0x00));
            _bgShifterAttribHI = (short)(_bgShifterAttribHI & 0xFF00 | ((_bgNextTileAttrib & 0b10) != 0 ? 0xFF : 0x00));
        }

        // ==============================================================================
        // Every cycle the shifters storing pattern and attribute information shift
        // their contents by 1 bit. This is because every cycle, the output progresses
        // by 1 pixel. This means relatively, the state of the shifter is in sync
        // with the pixels being drawn for that 8 pixel section of the scanline.
        void UpdateShifters()
        {
            if (_mask.RenderBackground)
            {
                _bgShifterPatternLO <<= 1;
                _bgShifterPatternHI <<= 1;

                _bgShifterAttribLO <<= 1;
                _bgShifterAttribHI <<= 1;
            }
        }

        // All but 1 of the secanlines is visible to the user. The pre-render scanline
        // at -1, is used to configure the "shifters" for the first visible scanline, 0.
        if (_scanline >= -1 && _scanline < 240)
        {
            if (_scanline == 0 && _cycle == 0)
            {
                // "Odd Frame" cycle skip
                _cycle = 1;
            }

            if (_scanline == -1 && _cycle == 1)
            {
                // Effectively start of new frame, so clear vertical blank flag
                _status.VerticalBlank = false;
            }


            if (_cycle >= 2 && _cycle < 258 || _cycle >= 321 && _cycle < 338)
            {
                UpdateShifters();


                // In these cycles we are collecting and working with visible data
                // The "shifters" have been preloaded by the end of the previous
                // scanline with the data for the start of this scanline. Once we
                // leave the visible region, we go dormant until the shifters are
                // preloaded for the next scanline.

                // Fortunately, for background rendering, we go through a fairly
                // repeatable sequence of events, every 2 clock cycles.
                switch ((_cycle - 1) % 8)
                {
                    case 0:
                        // Load the current background tile pattern and attributes into the "shifter"
                        LoadBackgroundShifters();

                        // Fetch the next background tile ID
                        // "(vram_addr.reg & 0x0FFF)" : Mask to 12 bits that are relevant
                        // "| 0x2000"                 : Offset into nametable space on PPU address bus
                        _bgNextTileId = PpuRead((ushort)(0x2000 | _vramAddr.Value & 0x0FFF));

                        // Explanation:
                        // The bottom 12 bits of the loopy register provide an index into
                        // the 4 nametables, regardless of nametable mirroring configuration.
                        // nametable_y(1) nametable_x(1) coarse_y(5) coarse_x(5)
                        //
                        // Consider a single nametable is a 32x32 array, and we have four of them
                        //   0                1
                        // 0 +----------------+----------------+
                        //   |                |                |
                        //   |                |                |
                        //   |    (32x32)     |    (32x32)     |
                        //   |                |                |
                        //   |                |                |
                        // 1 +----------------+----------------+
                        //   |                |                |
                        //   |                |                |
                        //   |    (32x32)     |    (32x32)     |
                        //   |                |                |
                        //   |                |                |
                        //   +----------------+----------------+
                        //
                        // This means there are 4096 potential locations in this array, which 
                        // just so happens to be 2^12!
                        break;
                    case 2:
                        // Fetch the next background tile attribute. OK, so this one is a bit
                        // more involved :P

                        // Recall that each nametable has two rows of cells that are not tile 
                        // information, instead they represent the attribute information that
                        // indicates which palettes are applied to which area on the screen.
                        // Importantly (and frustratingly) there is not a 1 to 1 correspondance
                        // between background tile and palette. Two rows of tile data holds
                        // 64 attributes. Therfore we can assume that the attributes affect
                        // 8x8 zones on the screen for that nametable. Given a working resolution
                        // of 256x240, we can further assume that each zone is 32x32 pixels
                        // in screen space, or 4x4 tiles. Four system palettes are allocated
                        // to background rendering, so a palette can be specified using just
                        // 2 bits. The attribute byte therefore can specify 4 distinct palettes.
                        // Therefore we can even further assume that a single palette is
                        // applied to a 2x2 tile combination of the 4x4 tile zone. The very fact
                        // that background tiles "share" a palette locally is the reason why
                        // in some games you see distortion in the colours at screen edges.

                        // As before when choosing the tile ID, we can use the bottom 12 bits of
                        // the loopy register, but we need to make the implementation "coarser"
                        // because instead of a specific tile, we want the attribute byte for a 
                        // group of 4x4 tiles, or in other words, we divide our 32x32 address
                        // by 4 to give us an equivalent 8x8 address, and we offset this address
                        // into the attribute section of the target nametable.

                        // Reconstruct the 12 bit loopy address into an offset into the
                        // attribute memory

                        // "(vram_addr.coarse_x >> 2)"        : integer divide coarse x by 4, 
                        //                                      from 5 bits to 3 bits
                        // "((vram_addr.coarse_y >> 2) << 3)" : integer divide coarse y by 4, 
                        //                                      from 5 bits to 3 bits,
                        //                                      shift to make room for coarse x

                        // Result so far: YX00 00yy yxxx

                        // All attribute memory begins at 0x03C0 within a nametable, so OR with
                        // result to select target nametable, and attribute byte offset. Finally
                        // OR with 0x2000 to offset into nametable address space on PPU bus.				
                        _bgNextTileAttrib = PpuRead((ushort)(0x23C0 | (_vramAddr.NameTableY ? 1 : 0) << 11
                                                             | (_vramAddr.NameTableX ? 1 : 0) << 10
                                                             | _vramAddr.CoarseY >> 2 << 3
                                                             | _vramAddr.CoarseX >> 2));

                        // Right we've read the correct attribute byte for a specified address,
                        // but the byte itself is broken down further into the 2x2 tile groups
                        // in the 4x4 attribute zone.

                        // The attribute byte is assembled thus: BR(76) BL(54) TR(32) TL(10)
                        //
                        // +----+----+			    +----+----+
                        // | TL | TR |			    | ID | ID |
                        // +----+----+ where TL =   +----+----+
                        // | BL | BR |			    | ID | ID |
                        // +----+----+			    +----+----+
                        //
                        // Since we know we can access a tile directly from the 12 bit address, we
                        // can analyse the bottom bits of the coarse coordinates to provide us with
                        // the correct offset into the 8-bit word, to yield the 2 bits we are
                        // actually interested in which specifies the palette for the 2x2 group of
                        // tiles. We know if "coarse y % 4" < 2 we are in the top half else bottom half.
                        // Likewise if "coarse x % 4" < 2 we are in the left half else right half.
                        // Ultimately we want the bottom two bits of our attribute word to be the
                        // palette selected. So shift as required...				
                        if ((_vramAddr.CoarseY & 0x02) != 0) _bgNextTileAttrib >>= 4;
                        if ((_vramAddr.CoarseX & 0x02) != 0) _bgNextTileAttrib >>= 2;
                        _bgNextTileAttrib &= 0x03;
                        break;

                    // Compared to the last two, the next two are the easy ones... :P

                    case 4:
                        // Fetch the next background tile LSB bit plane from the pattern memory
                        // The Tile ID has been read from the nametable. We will use this id to 
                        // index into the pattern memory to find the correct sprite (assuming
                        // the sprites lie on 8x8 pixel boundaries in that memory, which they do
                        // even though 8x16 sprites exist, as background tiles are always 8x8).
                        //
                        // Since the sprites are effectively 1 bit deep, but 8 pixels wide, we 
                        // can represent a whole sprite row as a single byte, so offsetting
                        // into the pattern memory is easy. In total there is 8KB so we need a 
                        // 13 bit address.

                        // "(control.pattern_background << 12)"  : the pattern memory selector 
                        //                                         from control register, either 0K
                        //                                         or 4K offset
                        // "((uint16_t)bg_next_tile_id << 4)"    : the tile id multiplied by 16, as
                        //                                         2 lots of 8 rows of 8 bit pixels
                        // "(vram_addr.fine_y)"                  : Offset into which row based on
                        //                                         vertical scroll offset
                        // "+ 0"                                 : Mental clarity for plane offset
                        // Note: No PPU address bus offset required as it starts at 0x0000
                        _bgNextTileLsb = PpuRead((ushort)(((_contrl.PatternBackground ? 1 : 0) << 12)
                                                   + (_bgNextTileId << 4)
                                                   + _vramAddr.FineY + 0));

                        break;
                    case 6:
                        // Fetch the next background tile MSB bit plane from the pattern memory
                        // This is the same as above, but has a +8 offset to select the next bit plane
                        _bgNextTileMsb = PpuRead((ushort)(((_contrl.PatternBackground ? 1 : 0) << 12)
                                                   + (_bgNextTileId << 4)
                                                   + _vramAddr.FineY + 8));
                        break;
                    case 7:
                        // Increment the background tile "pointer" to the next tile horizontally
                        // in the nametable memory. Note this may cross nametable boundaries which
                        // is a little complex, but essential to implement scrolling
                        IncrementScrollX();
                        break;
                }
            }

            // End of a visible scanline, so increment downwards...
            if (_cycle == 256)
            {
                IncrementScrollY();
            }

            //...and reset the x position
            if (_cycle == 257)
            {
                LoadBackgroundShifters();
                TransferAddressX();
            }

            // Superfluous reads of tile id at end of scanline
            if (_cycle == 338 || _cycle == 340)
            {
                _bgNextTileId = PpuRead((ushort)(0x2000 | _vramAddr.Value & 0x0FFF));
            }

            if (_scanline == -1 && _cycle >= 280 && _cycle < 305)
            {
                // End of vertical blank period so reset the Y address ready for rendering
                TransferAddressY();
            }
        }

        if (_scanline == 240)
        {
            // Post Render Scanline - Do Nothing!
        }

        if (_scanline >= 241 && _scanline < 261)
        {
            if (_scanline == 241 && _cycle == 1)
            {
                // Effectively end of frame, so set vertical blank flag
                _status.VerticalBlank = true;

                // If the control register tells us to emit a NMI when
                // entering vertical blanking period, do it! The CPU
                // will be informed that rendering is complete so it can
                // perform operations with the PPU knowing it wont
                // produce visible artefacts
                if (_contrl.EnableNmi)
                    Nmi = true;
            }
        }



        // Composition - We now have background pixel information for this cycle
        // At this point we are only interested in background

        byte bgPixel = 0x00;   // The 2-bit pixel to be rendered
        byte bgPalette = 0x00; // The 3-bit index of the palette the pixel indexes

        // We only render backgrounds if the PPU is enabled to do so. Note if 
        // background rendering is disabled, the pixel and palette combine
        // to form 0x00. This will fall through the colour tables to yield
        // the current background colour in effect
        if (_mask.RenderBackground)
        {
            // Handle Pixel Selection by selecting the relevant bit
            // depending upon fine x scolling. This has the effect of
            // offsetting ALL background rendering by a set number
            // of pixels, permitting smooth scrolling
            var bitMux = 0x8000 >> _fineX;

            // Select Plane pixels by extracting from the shifter 
            // at the required location. 
            byte p0Pixel = (byte)((_bgShifterPatternLO & bitMux) > 0 ? 1 : 0);
            byte p1Pixel = (byte)((_bgShifterPatternHI & bitMux) > 0 ? 1 : 0);

            // Combine to form pixel index
            bgPixel = (byte)(p1Pixel << 1 | p0Pixel);

            // Get palette
            byte bgPal0 = (_bgShifterAttribLO & bitMux) > 0 ? (byte)1 : (byte)0;
            byte bgPal1 = (_bgShifterAttribHI & bitMux) > 0 ? (byte)1 : (byte)0;
            bgPalette = (byte)(bgPal1 << 1 | bgPal0);
        }


        // Now we have a final pixel colour, and a palette for this cycle
        // of the current scanline. Let's at long last, draw that ^&%*er :P

        _sprScreen.SetPixel(_cycle - 1, _scanline, GetColourFromPaletteRam(bgPalette, bgPixel));

        // Fake some noise for now
        //sprScreen.SetPixel(cycle - 1, scanline, palScreen[(rand() % 2) ? 0x3F : 0x30]);


        // Fake some noise for now
        //sprScreen.SetPixel(cycle - 1, scanline, palScreen[random.NextDouble() >= 0.5 ? 0x3F : 0x30]);

        // Advance renderer - it never stops, it's relentless
        _cycle++;
        if (_cycle >= 341)
        {
            _cycle = 0;
            _scanline++;
            if (_scanline >= 261)
            {
                _scanline = -1;
                FrameComplete = true;
            }
        }
    }

}