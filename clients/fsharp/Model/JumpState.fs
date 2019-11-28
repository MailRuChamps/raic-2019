namespace AiCup2019.Model

module JumpState =
    type T =
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

    let readFrom (reader: System.IO.BinaryReader) =
        {
            CanJump = reader.ReadBoolean()
            Speed = reader.ReadDouble()
            MaxTime = reader.ReadDouble()
            CanCancel = reader.ReadBoolean()
        }

