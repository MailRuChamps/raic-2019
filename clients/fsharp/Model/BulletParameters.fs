namespace AiCup2019.Model

type BulletParameters = {Speed: double; Size: double; Damage: int} with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.Speed
        writer.Write this.Size
        writer.Write this.Damage

    static member readFrom (reader: System.IO.BinaryReader) =
        {
            Speed = reader.ReadDouble()
            Size = reader.ReadDouble()
            Damage = reader.ReadInt32()
        }
