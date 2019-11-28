namespace AiCup2019.Model

module ExplosionParameters = 
    type T = {Radius : double; Damage: int} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.Radius
            writer.Write this.Damage

    let readFrom (reader: System.IO.BinaryReader) = 
        {Radius = reader.ReadDouble(); Damage = reader.ReadInt32()}