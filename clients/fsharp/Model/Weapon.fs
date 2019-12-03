#nowarn "0058"
namespace AiCup2019.Model
type Weapon = {
    Typ: WeaponType;
    Parameters: WeaponParameters;
    Magazine: int;
    WasShooting: bool;
    Spread: double;
    FireTimer: option<double>;
    LastAngle: option<double>;
    LastFireTick: option<int>;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write (int this.Typ)
        this.Parameters.writeTo writer
        writer.Write this.Magazine
        writer.Write this.WasShooting
        writer.Write this.Spread
        match this.FireTimer with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        match this.LastAngle with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        match this.LastFireTick with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
    static member readFrom(reader: System.IO.BinaryReader) = {
        Typ = reader.ReadInt32() |> enum
        Parameters = WeaponParameters.readFrom reader
        Magazine = reader.ReadInt32()
        WasShooting = reader.ReadBoolean()
        Spread = reader.ReadDouble()
        FireTimer = match reader.ReadBoolean() with
            | true ->
                Some(
                    reader.ReadDouble()
                    )
            | false -> None
        LastAngle = match reader.ReadBoolean() with
            | true ->
                Some(
                    reader.ReadDouble()
                    )
            | false -> None
        LastFireTick = match reader.ReadBoolean() with
            | true ->
                Some(
                    reader.ReadInt32()
                    )
            | false -> None
    }
