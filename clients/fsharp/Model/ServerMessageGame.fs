#nowarn "0058"
namespace AiCup2019.Model
type ServerMessageGame = {
    PlayerView: option<PlayerView>;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this.PlayerView with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
    static member readFrom(reader: System.IO.BinaryReader) = {
        PlayerView = match reader.ReadBoolean() with
            | true ->
                Some(
                    PlayerView.readFrom reader
                    )
            | false -> None
    }
