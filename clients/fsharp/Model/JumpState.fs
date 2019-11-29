namespace AiCup2019.Model

type JumpState =
    {
        CanJump: bool
        Speed: double
        MaxTime: double
        CanCancel: bool
    } with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.CanJump
        writer.Write this.Speed
        writer.Write this.MaxTime
        writer.Write this.CanCancel

    static member readFrom (reader: System.IO.BinaryReader) =
        {
            CanJump = reader.ReadBoolean()
            Speed = reader.ReadDouble()
            MaxTime = reader.ReadDouble()
            CanCancel = reader.ReadBoolean()
        }
