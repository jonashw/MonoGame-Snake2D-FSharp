module PalleteColor

open Microsoft.Xna.Framework

type PalleteColor = Rank * Saturation
and Rank = Primary | SecondaryA | SecondaryB | Complement | Blank
and Saturation = Lowest | Low | Normal | High | Highest

let toColor = function
| (Blank, _ )           -> new Color(0xff,0xff,0xff)
| (Primary, Lowest)     -> new Color(0x62,0x9A,0x7D)
| (Primary, Low)        -> new Color(0x51,0xC3,0x89)
| (Primary, Normal)     -> new Color(0x49,0xE3,0x95)
| (Primary, High)       -> new Color(0x47,0xF9,0x9E)
| (Primary, Highest)    -> new Color(0x4E,0xFF,0xA5)
| (SecondaryA, Lowest)  -> new Color(0xD1,0xD8,0x89)
| (SecondaryA, Low)     -> new Color(0xEF,0xFC,0x69)
| (SecondaryA, Normal)  -> new Color(0xEF,0xFD,0x52)
| (SecondaryA, High)    -> new Color(0xEF,0xFF,0x48)
| (SecondaryA, Highest) -> new Color(0xF0,0xFF,0x4E)
| (SecondaryB, Lowest)  -> new Color(0x6B,0x69,0x99)
| (SecondaryB, Low)     -> new Color(0x65,0x61,0xC2)
| (SecondaryB, Normal)  -> new Color(0x64,0x5F,0xE3)
| (SecondaryB, High)    -> new Color(0x66,0x60,0xF9)
| (SecondaryB, Highest) -> new Color(0x6D,0x68,0xFF)
| (Complement, Lowest)  -> new Color(0xDB,0x9F,0x8B)
| (Complement, Low)     -> new Color(0xFF,0x8E,0x6A)
| (Complement, Normal)  -> new Color(0xFF,0x7C,0x52)
| (Complement, High)    -> new Color(0xFF,0x74,0x49)
| (Complement, Highest) -> new Color(0xFF,0x79,0x4E)