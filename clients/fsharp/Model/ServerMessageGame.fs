namespace AiCup2019.Model

type ServerMessageGame = {PlayerView: option<PlayerView>} with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        match this.PlayerView with
            | Some x -> writer.Write true
                        x.writeTo writer
            | None -> writer.Write false
    
    static member readFrom (reader: System.IO.BinaryReader) =
        match reader.ReadBoolean() with
            | true -> Some(PlayerView.readFrom reader)
            | _ -> None     
