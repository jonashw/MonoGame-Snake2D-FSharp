[<AutoOpen>]
module RectangleF

open Microsoft.Xna.Framework
open System.Diagnostics

//Adapted from https://github.com/47th/Monogame/blob/master/Engine/Util/RectangleF.cs
/// <summary>
/// Much like Rectangle, but stored as two Vector2s
/// </summary>
type RectangleF = 
    { Min: Vector2
    ; Max: Vector2
    }
    with
    member r.Left = r.Min.X
    member r.Right = r.Max.X
    member r.Top = r.Min.Y
    member r.Bottom = r.Max.Y
    member r.Width = r.Max.X - r.Min.X
    member r.Height = r.Max.Y - r.Min.Y
    member r.Center = (r.Min + r.Max) / 2.0f
    member r.Intersects (o: RectangleF) =
            (r.Min.X < o.Max.X) &&
            (r.Min.Y < o.Max.Y) &&
            (r.Max.X > o.Min.X) &&
            (r.Max.Y > o.Min.Y)
    member r.Intersects (o: Rectangle) =
            (r.Min.X < (float32 o.Right)) &&
            (r.Min.Y < (float32 o.Bottom)) &&
            (r.Max.X > (float32 o.Left)) &&
            (r.Max.Y > (float32 o.Top))
    member r.IsEmpty =
            r.Min.X = 0.0f &&
            r.Min.Y = 0.0f &&
            r.Max.X = 0.0f &&
            r.Max.Y = 0.0f
    static member fromMinAndMax min max = { Min = min ; Max = max }
    static member fromParts x y w h = { Min = Vector2(x,y); Max = Vector2(x + w, y + h) }

let private _empty = { Min = Vector2.Zero ; Max = Vector2.Zero }
type RectangleF with
    static member empty = _empty



    (*
    public bool Contains(float x, float y)
    {
        return
            (Min.X <= x) &&
            (Min.Y <= y) &&
            (Max.X >= x) &&
            (Max.Y >= y);
    }

    public bool Contains(Vector2 vector)
    {
        return
            (Min.X <= vector.X) &&
            (Min.Y <= vector.Y) &&
            (Max.X >= vector.X) &&
            (Max.Y >= vector.Y);
    }

    public void Contains(ref Vector2 rect, out bool result)
    {
        result =
            (Min.X <= rect.X) &&
            (Min.Y <= rect.Y) &&
            (Max.X >= rect.X) &&
            (Max.Y >= rect.Y);
    }

    public bool Contains(RectangleF rect)
    {
        return
            (Min.X <= rect.Min.X) &&
            (Min.Y <= rect.Min.Y) &&
            (Max.X >= rect.Max.X) &&
            (Max.Y >= rect.Max.Y);
    }

    public void Contains(ref RectangleF rect, out bool result)
    {
        result =
            (Min.X <= rect.Min.X) &&
            (Min.Y <= rect.Min.Y) &&
            (Max.X >= rect.Max.X) &&
            (Max.Y >= rect.Max.Y) ;
    }


    public static RectangleF Intersect(RectangleF rect1, RectangleF rect2)
    {
        RectangleF result;

        float num8 = rect1.Max.X;
        float num7 = rect2.Max.X;
        float num6 = rect1.Max.Y;
        float num5 = rect2.Max.Y;
        float num2 = (rect1.Min.X > rect2.Min.X) ? rect1.Min.X : rect2.Min.X;
        float num = (rect1.Min.Y > rect2.Min.Y) ? rect1.Min.Y : rect2.Min.Y;
        float num4 = (num8 < num7) ? num8 : num7;
        float num3 = (num6 < num5) ? num6 : num5;

        if ((num4 > num2) && (num3 > num))
        {
            result.Min.X = num2;
            result.Min.Y = num;
            result.Max.X = num4;
            result.Max.Y = num3;

            return result;
        }

        result.Min.X = 0;
        result.Min.Y = 0;
        result.Max.X = 0;
        result.Max.Y = 0;

        return result;
    }

    public static void Intersect(ref RectangleF rect1, ref RectangleF rect2, out RectangleF result)
    {
        float num8 = rect1.Max.X;
        float num7 = rect2.Max.X;
        float num6 = rect1.Max.Y;
        float num5 = rect2.Max.Y;
        float num2 = (rect1.Min.X > rect2.Min.X) ? rect1.Min.X : rect2.Min.X;
        float num = (rect1.Min.Y > rect2.Min.Y) ? rect1.Min.Y : rect2.Min.Y;
        float num4 = (num8 < num7) ? num8 : num7;
        float num3 = (num6 < num5) ? num6 : num5;

        if ((num4 > num2) && (num3 > num))
        {
            result.Min.X = num2;
            result.Min.Y = num;
            result.Max.X = num4;
            result.Max.Y = num3;
        }

        result.Min.X = 0;
        result.Min.Y = 0;
        result.Max.X = 0;
        result.Max.Y = 0;
    }

    public static RectangleF Union(RectangleF rect1, RectangleF rect2)
    {
        RectangleF result;

        float num6 = rect1.Max.X;
        float num5 = rect2.Max.X;
        float num4 = rect1.Max.Y;
        float num3 = rect2.Max.Y;
        float num2 = (rect1.Min.X < rect2.Min.X) ? rect1.Min.X : rect2.Min.X;
        float num = (rect1.Min.Y < rect2.Min.Y) ? rect1.Min.Y : rect2.Min.Y;
        float num8 = (num6 > num5) ? num6 : num5;
        float num7 = (num4 > num3) ? num4 : num3;

        result.Min.X = num2;
        result.Min.Y = num;
        result.Max.X = num8;
        result.Max.Y = num7;

        return result;
    }

    public static void Union(ref RectangleF rect1, ref RectangleF rect2, out RectangleF result)
    {
        float num6 = rect1.Max.X;
        float num5 = rect2.Max.X;
        float num4 = rect1.Max.Y;
        float num3 = rect2.Max.Y;
        float num2 = (rect1.Min.X < rect2.Min.X) ? rect1.Min.X : rect2.Min.X;
        float num = (rect1.Min.Y < rect2.Min.Y) ? rect1.Min.Y : rect2.Min.Y;
        float num8 = (num6 > num5) ? num6 : num5;
        float num7 = (num4 > num3) ? num4 : num3;

        result.Min.X = num2;
        result.Min.Y = num;
        result.Max.X = num8;
        result.Max.Y = num7;
    }

    public bool Equals(RectangleF other)
    {
        return
            (Min.X == other.Min.X) &&
            (Min.Y == other.Min.Y) &&
            (Max.X == other.Max.X) &&
            (Max.Y == other.Max.Y);
    }

    public override int GetHashCode()
    {
        return Min.GetHashCode() + Max.GetHashCode();
    }

    public static bool operator ==(RectangleF a, RectangleF b)
    {
        return
            (a.Min.X == b.Min.X) &&
            (a.Min.Y == b.Min.Y) &&
            (a.Max.X == b.Max.X) &&
            (a.Max.Y == b.Max.Y);
    }

    public static bool operator !=(RectangleF a, RectangleF b)
    {
        return
            (a.Min.X != b.Min.X) ||
            (a.Min.Y != b.Min.Y) ||
            (a.Max.X != b.Max.X) ||
            (a.Max.Y != b.Max.Y);
    }

    public static RectangleF operator +(RectangleF rect, Vector2 v)
    {
        return new RectangleF(rect.Min + v, rect.Max + v);
    }

    public override bool Equals(object obj)
    {
        if (obj is RectangleF)
        {
            return this == (RectangleF)obj;
        }

        return false;
    }
    *)