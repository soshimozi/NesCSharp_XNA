using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using NesEmulatorCS.Emulator;
using System;
using System.Collections.Generic;
using System.Text;
using NesEmulatorCS.Lib;
using SharpDX.Direct2D1;
using SpriteBatch = Microsoft.Xna.Framework.Graphics.SpriteBatch;
using SharpDX.Direct2D1.Effects;
using SharpDX.Direct3D9;

namespace NesEmulatorCS
{
    public class FamicomEmulator : Game
    {
        private GraphicsDeviceManager _graphics;
        private SpriteBatch _spriteBatch;
        private SpriteFont _gameFont;
        //private SpriteFont _boldGameFont;

        private FamicomSystem _nes;
        private Cartridge _cartridge;

        private Dictionary<ushort, string> mapAsm;

        private bool _freeRunning = false;
        private int _selectedPalette = 0;

        KeyboardState currentKeyState;
        KeyboardState previousKeyState;
        public FamicomEmulator(Vector2? screenSize = null, bool? fullScreen = null)
        {
            _graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
            IsMouseVisible = true;

            _graphics.PreferredBackBufferWidth = (int)(screenSize?.X ?? 100);
            _graphics.PreferredBackBufferHeight = (int)(screenSize?.Y ?? 100); ;

            if (fullScreen == true)
            {
                _graphics.IsFullScreen = true;
            }

            _graphics.ApplyChanges();
        }

        protected override void Initialize()
        {
            // TODO: Add your initialization logic here

            _nes = new FamicomSystem(GraphicsDevice);
            _cartridge = new Cartridge("./nestest.nes");

            _nes.InsertCartridge(_cartridge);

            // Extract dissassembly
            mapAsm = _nes.Cpu.Disassemble(0x0000, 0xFFFF);

            _nes.Reset();

            base.Initialize();
        }

        protected override void LoadContent()
        {
            _spriteBatch = new SpriteBatch(GraphicsDevice);

            _gameFont = Content.Load<SpriteFont>("GameFont");
            //_boldGameFont = Content.Load<SpriteFont>("GameFontBold");
            // TODO: use this.Content to load your game content here
        }

        private bool IsKeyJustPressed(Keys key)
        {
            return currentKeyState.IsKeyDown(key) && previousKeyState.IsKeyUp(key);
        }

        protected override void Update(GameTime gameTime)
        {
            previousKeyState = currentKeyState;
            currentKeyState = Keyboard.GetState();

            if (GamePad.GetState(PlayerIndex.One).Buttons.Back == ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
                Exit();

            if (GamePad.GetState(PlayerIndex.One).Buttons.A == ButtonState.Pressed ||
                Keyboard.GetState().IsKeyDown(Keys.Space))
                _freeRunning = !_freeRunning;


            if (GamePad.GetState(PlayerIndex.One).Buttons.X == ButtonState.Pressed ||
                IsKeyJustPressed(Keys.P))
            {
                _selectedPalette++;
                _selectedPalette %= 8;
            }

            if (GamePad.GetState(PlayerIndex.One).Buttons.Y == ButtonState.Pressed ||
                IsKeyJustPressed(Keys.R))
            {
                _nes.Reset();
            }

            // Emulate code step-by-step
            if (GamePad.GetState(PlayerIndex.One).Buttons.B == ButtonState.Pressed ||
                    IsKeyJustPressed(Keys.C))
            {
                // Clock enough times to execute a whole CPU instruction
                do { _nes.Clock(); } while (!_nes.Cpu.IsComplete);

                // CPU clock runs slower than system clock, so it may be
                // complete for additional system clock cycles. Drain
                // those out
                do { _nes.Clock(); } while (_nes.Cpu.IsComplete);
            }

            if (GamePad.GetState(PlayerIndex.One).Buttons.LeftShoulder == ButtonState.Pressed ||
                IsKeyJustPressed(Keys.F))
            {
                // Clock enough times to draw a single frame
                do { _nes.Clock(); } while (!_nes.Ppu.FrameComplete);

                // Use residual clock cycles to complete current instruction
                do { _nes.Clock(); } while (!_nes.Cpu.IsComplete);

                // Reset frame completion flag
                _nes.Ppu.FrameComplete = false;
            }



            // update texture
            _nes.Ppu.GetScreen().Update(gameTime);

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.DarkBlue);

            _spriteBatch.Begin();

            DrawCpu(1032, 2);
            DrawCode(1032, 144, 26);

            // Draw Palettes & Pattern Tables ==============================================
            const int nSwatchSize = 6;
            const int scaledSwatchSize = nSwatchSize * 2;
            for (int p = 0; p < 8; p++) // For each palette
            for (int s = 0; s < 4; s++) // For each index
                FillRectangle(_spriteBatch, new Rectangle(1032 + p * (scaledSwatchSize * 5) + s * scaledSwatchSize, 682,
                    scaledSwatchSize, scaledSwatchSize), _nes.Ppu.GetColourFromPaletteRam((byte)p, (byte)s));

            // Draw selection reticule around selected palette
            DrawRectangle(_spriteBatch, new Rectangle(1032 + _selectedPalette * (scaledSwatchSize * 5) - 1, 682, (scaledSwatchSize * 4), scaledSwatchSize), Color.White);

            // Generate Pattern Tables
            _spriteBatch.Draw(_nes.Ppu.GetPatternTable(0, (byte)_selectedPalette).Texture, new Vector2(1032, 696), Color.White);
            _spriteBatch.Draw(_nes.Ppu.GetPatternTable(1, (byte)_selectedPalette).Texture, new Vector2(1296, 696), Color.White);

            // Draw rendered output ========================================================
            DrawGameScreen();
            _spriteBatch.End();


            base.Draw(gameTime);
        }

        private static string Hex(uint n, byte d)
        {
            return ((int)n).ToHexString(d);
        }

        public void DrawCode(int x, int y, int nLines)
        {
            const int verticalSpace = 20;
            List<ushort> keys = new List<ushort>(mapAsm.Keys);
            keys.Sort();

            int keyIndex = keys.IndexOf(_nes.Cpu.Registers.Pc);
            int nLineY = (nLines >> 1) * verticalSpace + y;

            if (keyIndex != -1)
            {
                DrawString(_gameFont, x, nLineY, mapAsm[keys[keyIndex]], Color.Cyan);
                while (nLineY < (nLines * verticalSpace) + y)
                {
                    nLineY += verticalSpace;
                    keyIndex++;
                    if (keyIndex < keys.Count)
                    {
                        DrawString(_gameFont, x, nLineY, mapAsm[keys[keyIndex]]);
                    }
                }
            }

            keyIndex = keys.IndexOf(_nes.Cpu.Registers.Pc);
            nLineY = (nLines >> 1) * verticalSpace + y;

            if (keyIndex != -1)
            {
                while (nLineY > y)
                {
                    nLineY -= verticalSpace;
                    keyIndex--;
                    if (keyIndex >= 0)
                    {
                        DrawString(_gameFont, x, nLineY, mapAsm[keys[keyIndex]]);
                    }
                }
            }
        }

        void DrawCpu(int x, int y)
        {
            //var status = "STATUS: ";
            DrawString(_gameFont, x, y, "STATUS:", Color.White);
            DrawString(_gameFont, x + 128, y, "N", (_nes.Cpu.Registers.Status & (byte)Flags6502.N) !=0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 160, y, "V", (_nes.Cpu.Registers.Status & (byte)Flags6502.V) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 192, y, "-", (_nes.Cpu.Registers.Status & (byte)Flags6502.U) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 224, y, "B", (_nes.Cpu.Registers.Status & (byte)Flags6502.B) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 256, y, "D", (_nes.Cpu.Registers.Status & (byte)Flags6502.D) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 288, y, "I", (_nes.Cpu.Registers.Status & (byte)Flags6502.I) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 320, y, "Z", (_nes.Cpu.Registers.Status & (byte)Flags6502.Z) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x + 356, y, "C", (_nes.Cpu.Registers.Status & (byte)Flags6502.C) != 0 ? Color.Green : Color.Red);
            DrawString(_gameFont, x, y + 20, $"PC: ${((int)_nes.Cpu.Registers.Pc).ToHexString(4)}");
            DrawString(_gameFont, x, y + 40, $"A: ${Hex(_nes.Cpu.Registers.A, 2)}  [{_nes.Cpu.Registers.A}]");
            DrawString(_gameFont, x, y + 60, $"X: ${Hex(_nes.Cpu.Registers.X, 2)}  [{_nes.Cpu.Registers.X}]");
            DrawString(_gameFont, x, y + 80, $"Y: ${Hex(_nes.Cpu.Registers.Y, 2)}  [{_nes.Cpu.Registers.Y}]");
            DrawString(_gameFont, x, y + 100, $"Stack P: ${Hex(_nes.Cpu.Registers.Sp, 4)}");
        }
        private void DrawRam(int x, int y, ushort nAddr, int nRows, int nColumns)
        {
            var nRamX = x;
            var nRamY = y;

            for (var row = 0; row < nRows; row++)
            {
                var sOffset = new StringBuilder(); sOffset.Append("$" + Hex(nAddr, 4) + ":");

                DrawString(_gameFont, nRamX, nRamY, sOffset.ToString(), Color.Yellow, 2.0f);

                sOffset.Clear();

                for (var col = 0; col < nColumns; col++)
                {
                    sOffset.Append(" ");
                    sOffset.Append(Hex(_nes.CpuRead(nAddr, true), 2));
                    nAddr += 1;
                }

                DrawString(_gameFont, nRamX + 45, nRamY, sOffset.ToString(), Color.White, 2.0f);
                nRamY += 15;
            }
        }

        private void DrawString(SpriteFont font, int x, int y, string s, Color color, float? scale = null)
        {
            var scaleVector = new Vector2(scale ?? 1, scale ?? 1);
            DrawString(font, x, y, s, color, scaleVector);
        }

        private void DrawString(SpriteFont font, int x, int y, string s, Color? color = null, Vector2? scale = null)
        {
            _spriteBatch.DrawString(font, s, new Vector2(x, y), color ?? Color.White, 0, Vector2.Zero, scale ?? new Vector2(1, 1), SpriteEffects.None, 1);
        }

        private void DrawGameScreen(bool centered = false)
        {
            var screenWidth = GraphicsDevice.Viewport.Width;
            var screenHeight = GraphicsDevice.Viewport.Height;

            const float aspectRatioNes = 256.0f / 240.0f;
            var aspectRatioScreen = screenWidth / screenHeight;

            var scalingFactor = (aspectRatioNes >= aspectRatioScreen)
                ? screenHeight / 240.0 // letter box on sides
                : screenWidth / 256.0; // letter box on bottoms

            var offsetX = centered ? (screenWidth - (256 * scalingFactor)) / 2 : 0;
            var offsetY = centered ?  (screenHeight - (240 * scalingFactor)) / 2 : 0;

            var position = new Vector2((float)offsetX, (float)offsetY);

            var scale = new Vector2((float)scalingFactor, (float)scalingFactor);

            _nes.Ppu.GetScreen().Position = position;
            _nes.Ppu.GetScreen().Scale = scale; // new Vector2((float)scalingFactor, (float)scalingFactor);

            _spriteBatch.Draw(_nes.Ppu.GetScreen().Texture, position, null, Color.White, 0f, Vector2.Zero, scale, SpriteEffects.None, 0f);

        }

        private void DrawRectangle(SpriteBatch spriteBatch, Rectangle rectangle, Color rectColor)
        {
            Texture2D pixel;
            pixel = new Texture2D(GraphicsDevice, 1, 1);
            pixel.SetData(new[] { rectColor });

            int lineWidth = 2; // This will be the thickness of the stroke.
            // Top
            spriteBatch.Draw(pixel, new Rectangle(rectangle.X, rectangle.Y, rectangle.Width, lineWidth), rectColor);
            // Bottom
            spriteBatch.Draw(pixel, new Rectangle(rectangle.X, rectangle.Y + rectangle.Height - lineWidth, rectangle.Width, lineWidth), rectColor);
            // Left
            spriteBatch.Draw(pixel, new Rectangle(rectangle.X, rectangle.Y, lineWidth, rectangle.Height), rectColor);
            // Right
            spriteBatch.Draw(pixel, new Rectangle(rectangle.X + rectangle.Width - lineWidth, rectangle.Y, lineWidth, rectangle.Height), rectColor);
        }
        private void FillRectangle(SpriteBatch spriteBatch, Rectangle rectangle, Color fillColor)
        {
            Texture2D pixel;
            pixel = new Texture2D(GraphicsDevice, 1, 1);
            pixel.SetData(new[] { fillColor });

            _spriteBatch.Draw(pixel, rectangle, fillColor);
        }
    }
}