namespace AiCup2019.Model

type ExplosionParameters = {Radius : double; Damage: int} with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.Radius
        writer.Write this.Damage

    static member readFrom (reader: System.IO.BinaryReader) = 
        {Radius = reader.ReadDouble(); Damage = reader.ReadInt32()}
