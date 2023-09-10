using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;

namespace NesEmulatorCS.Lib;

public class Sprite
{
    public Vector2 Position { get; set; }

    public int Width { get; private set; }
    public int Height { get; private set; }

    public Rectangle? SourceRectangle { get; set; }
    public float Rotation { get; set; }
    public Vector2 Scale { get; set; }

    private readonly Color[] _pixelData;
    private readonly Texture2D _texture;

    public static Sprite CreateSprite(GraphicsDevice device, int width, int height)
    {
        return new Sprite(width, height, new Texture2D(device, width, height));
    }

    private Sprite(int width, int height, Texture2D spriteTexture, Color? color = null)
    {
        Width = width;
        Height = height;

        _pixelData = new Color[width * height];

        for (var i = 0; i < _pixelData.Length; i++)
        {
            _pixelData[i] = color ?? Color.Black;
        }

        _texture = spriteTexture;

        Rotation = 0f;
        Scale = new Vector2(1f, 1f);
    }

    public bool SetPixel(int x, int y, Color color)
    {
        if (x < 0 || x >= Width || y < 0 || y >= Height) return false;

        _pixelData[(y * Width) + x] = color;
        return true;

    }

    public Color GetPixel(int x, int y)
    {
        return _pixelData[(y * Width) + x];
    }


    public void Update(GameTime gameTime)
    {
        // TODO: Add your update logic here
        _texture.SetData(_pixelData);
    }

    public Texture2D Texture
    {
        get => _texture;
    }


    //public void Draw(SpriteBatch spriteBatch, Color? tint = null)
    //{
    //    spriteBatch.Draw(_texture,
    //        Position,
    //        SourceRectangle,
    //        tint ?? Color.White,
    //        Rotation,
    //        new Vector2(0, 0), // origin);
    //        Scale,
    //        SpriteEffects.None,
    //        0f);
    //}
}