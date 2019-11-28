namespace AiCup2019.Model

module BulletParameters =
    type T = {Speed: double; Size: double; Damage: int} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.Speed
            writer.Write this.Size
            writer.Write this.Damage

    let readFrom (reader: System.IO.BinaryReader) =
        {
            Speed = reader.ReadDouble()
            Size = reader.ReadDouble()
            Damage = reader.ReadInt32()
        }




